(in-package :stateless-iterators)

;; Basic iteration functions

(sera:defconstructor iterator
  (next       function)
  (init-state t))

(sera:-> iterate-for-effects
         (iterator (function (t) t))
         (values &optional))
(defun iterate-for-effects (iterator function)
  "Extract a value from @c(iterator) and call @c(function) with that
value. Repeat until the next state is @c(stop) and return no values."
  (declare (optimize (speed 3)))
  (let ((next  (iterator-next       iterator))
        (state (iterator-init-state iterator)))
    (labels ((%iter (state)
               (multiple-value-bind (value next-state)
                   (funcall next state)
                 (unless (eq next-state 'stop)
                   (funcall function value)
                   (%iter next-state)))))
      (%iter state)))
  (values))

(sera:-> consume-one (iterator &optional t)
         (values t iterator boolean &optional))
(defun consume-one (iterator &optional default)
  "Get exactly one value from @c(iterator) and return this value and a
new iterator without this value. If the iterator does not contain
values, return @c(default). Useful when we need to get the first value
which satisfies some condition:

@begin[lang=lisp](code)
(consume-one
 (drop-while
  #'oddp
  (list->iterator '(1 3 5 7 2 3 5))))
;; => 2, (ITERATOR #<FUNCTION STATELESS-ITERATORS::LIST->ITERATOR/NEXT> (3 5)), T
@end(code)

The third value is a boolean which indicates if the returned value is
not default."
  (declare (optimize (speed 3)))
  (let ((next (iterator-next        iterator))
        (state (iterator-init-state iterator)))
    (multiple-value-bind (value next-state)
        (funcall next state)
      (let ((new-value-p (not (eq next-state 'stop))))
        (values (if new-value-p value default)
                (iterator next next-state)
                new-value-p)))))

(sera:-> nth (unsigned-byte iterator)
         (values t iterator &optional))
(defun nth (n iterator)
  "Get n-th value from an iterator.

@begin[lang=lisp](code)
(nth 4 (count-from 0))
;; => 5, (STATELESS-ITERATORS:ITERATOR #<FUNCTION STATELESS-ITERATORS::COUNT-FROM/NEXT> 6)
@end(code)"
  (multiple-value-bind (x next)
      (consume-one iterator)
    (if (zerop n)
        (values x next)
        (nth (1- n) next))))

(defmacro do-iterator ((val iterator) &body body)
  "Execute @c(body) for each value from @c(iterator). The value is
bound to @c(val). This is equal to the following code:

@begin[lang=lisp](code)
(iterate-for-effects
  iterator
  (lambda (val) ...))
@end(code)"
  `(iterate-for-effects ,iterator (lambda (,val) ,@body)))

(sera:-> collect (iterator)
         (values list &optional))
(defun collect (iterator)
  "Collect all values from an iterator into a list. This is the
inverse of @c(list->iterator)."
  (let* ((acc (list nil))
         (ptr acc))
    (do-iterator (x iterator)
      (setf (cdr ptr) (list x)
            ptr (cdr ptr)))
    (cdr acc)))

;; Empty iterator
(defun empty/next (state)
  (declare (ignore state))
  (values nil 'stop))

(defparameter +empty+
  (iterator #'empty/next nil)
  "An empty iterator.")

(defun singleton/next (state)
  (destructuring-bind (state . value) state
    (ecase state
      ((t)   (values value (cons nil value)))
      ((nil) (values value 'stop)))))

(defun singleton (value)
  "Return an iterator containing only one specified value"
  (iterator #'singleton/next (cons t value)))

;; Converting sequences to iterator
(defun list->iterator/next (list)
  (values (car list)
          (if list (cdr list) 'stop)))

(defun list->iterator (list)
  "Convert a list to an iterator. This is the inverse of @c(collect)."
  (iterator #'list->iterator/next list))

(defun vector->iterator/next (vector)
  (let ((length (cl:length vector)))
    (lambda (state)
      (if (< state length)
          (values (aref vector state)
                  (1+ state))
          (values nil 'stop)))))

(sera:-> vector->iterator (vector)
         (values iterator &optional))
(defun vector->iterator (vector)
  "Convert a vector to an iterator."
  (iterator (vector->iterator/next vector) 0))

;; Take first n
(defun take/next (n next)
  (lambda (state)
    (destructuring-bind (counter . inner-state) state
      (if (< counter n)
          (multiple-value-bind (val new-inner-state)
              (funcall next inner-state)
            (values
             val (if (eq new-inner-state 'stop)
                     'stop (cons (1+ counter) new-inner-state))))
          (values nil 'stop)))))

(sera:-> take ((integer 0) iterator)
         (values iterator &optional))
(defun take (n iterator)
  "Create an iterator which takes @c(n) values from @c(iterator) and
stops.

@begin[lang=lisp](code)
(collect (take 3 (list->iterator '(1 2 3 4)))) -> '(1 2 3)
(collect (take 3 (list->iterator '(1)))) -> '(1)
@end(code)"
  (iterator (take/next n (iterator-next iterator))
            (cons 0 (iterator-init-state iterator))))

;; Iterate
(defun iterate/next (function)
  (lambda (state)
    (values state (funcall function state))))

(sera:-> iterate ((function (t) t) t)
         (values iterator &optional))
(defun iterate (function x)
  "Create an infinite iterator whose values are @c(x), @c(f(x)),
@c(f(f(x))) and so on.

@begin[lang=lisp](code)
(collect (take 3 (iterate (lambda (x) (/ x 2)) 1))) -> '(1 1/2 1/4)
@end(code)"
  (iterator (iterate/next function) x))

;; Unfold
(defun unfold/next (function)
  (lambda (state)
    (funcall function state)))

(sera:-> unfold ((sera:-> (t) (values t t &optional)) t)
         (values iterator &optional))
(defun unfold (function x)
  "When an element is forced from this iterator, it calls a function
@c(function) with the current state and uses the first returned value
as the element and the second value as a next state. @c(x) is an
initial state for iteration. A generation of new values can be stopped
if the symbol @c(stop) is the second returned value from @c(function).

These two calls are equivalent:
@begin[lang=lisp](code)
(iterate fn x)
(unfold (lambda (state) (values state (funcall fn state))) x)
@end(code)

Example:
@begin[lang=lisp](code)
(collect
 (unfold
  (lambda (state)
    (values
     (* state 2)
     (if (< state 5) (1+ state) 'stop)))
  1)) -> '(2 4 6 8)
@end(code)"
  (iterator (unfold/next function) x))

;; Counters
(defun count-from/next (state)
  (values state (1+ state)))

(sera:-> count-from (number)
         (values iterator &optional))
(defun count-from (n)
  "An infinite counting iterator which starts from @c(n) and
increments each next value by 1.

@begin[lang=lisp](code)
(collect (take 4 (count-from 1))) -> '(1 2 3 4)
@end(code)"
  (iterator #'count-from/next n))

(sera:-> range (real real &optional real)
         (values iterator &optional))
(defun range (from to &optional (step 1))
  "Create an iterator which counts from @c(from) to @c(to) (excluding
@c(to)) stepping by @c(step).

@begin[lang=lisp](code)
(collect (range 1 8 2)) -> '(1 3 5 7)
@end(code)"
  (take-while (lambda (x) (< x to))
              (iterate (lambda (x) (+ x step)) from)))

;; Take while
(defun take-while/next (next predicate)
  (lambda (state)
    (multiple-value-bind (value next-state)
        (funcall next state)
      (if (and (not (eq next-state 'stop))
               (funcall predicate value))
          (values value next-state)
          (values nil   'stop)))))

(sera:-> take-while ((sera:-> (t) (values t &optional)) iterator)
         (values iterator &optional))
(defun take-while (predicate iterator)
  "Create an iterator which returns values from @c(iterator) while
@c(predicate) is true.

@begin[lang=lisp](code)
(collect (take-while #'oddp (list->iterator '(1 3 8 2 5)))) -> '(1 3)
@end(code)"
  (iterator (take-while/next (iterator-next iterator) predicate)
            (iterator-init-state iterator)))

;; Drop while
(sera:-> drop-while ((sera:-> (t) (values t &optional)) iterator)
         (values iterator &optional))
(defun drop-while (predicate iterator)
  "Create an iterator which has the same values as in @c(iterator)
but drops initial values which satisfy the @c(predicate).

@begin[lang=lisp](code)
(collect (drop-while #'oddp (list->iterator '(1 3 8 2 5)))) -> '(8 2 5)
@end(code)"
  (declare (optimize (speed 3)))
  (let ((next       (iterator-next       iterator))
        (init-state (iterator-init-state iterator)))
    (labels ((%drop (state)
               (multiple-value-bind (value next-state)
                   (funcall next state)
                 (if (and (not (eq next-state 'stop))
                          (funcall predicate value))
                     (%drop next-state)
                     state))))
      (iterator next (%drop init-state)))))

;; Map
(defun imap/next (nexts function)
  (declare (optimize (speed 3))
           (type function function))
  (lambda (states)
    (destructuring-bind (values . next-states)
        (loop for next in nexts
              for state in states
              for value-and-next-state = (multiple-value-call #'cons
                                           (funcall (the function next) state))
              collect (car value-and-next-state) into values
              collect (cdr value-and-next-state) into next-states
              finally (return (cons values next-states)))
      (declare (type list values next-states))
      (if (find 'stop next-states :test #'eq)
          (values nil 'stop)
          (values (apply function values)
                  next-states)))))

(sera:-> imap (function &rest iterator)
         (values iterator &optional))
(defun imap (function &rest iterators)
  "Create an iterator which applies @c(function) to values of
@c(iterators). This iterator stops when at least one of @c(iterators) stops.

@begin[lang=lisp](code)
(collect (imap #'+ (list->iterator '(1 3 8 2)) (count-from 0))) -> '(1 4 10 5)
@end(code)"
  (iterator (imap/next (mapcar #'iterator-next iterators) function)
            (mapcar #'iterator-init-state iterators)))

;; Zipping
(sera:-> zip (iterator iterator)
         (values iterator &optional))
(defun zip (iter1 iter2)
  "Create an iterator which returns consed pairs of values of
@c(iter1) and @c(iter2).

@begin[lang=lisp](code)
(collect (take 3 (zip (count-from 1) (count-from 2)))) -> '((1 . 2) (2 . 3) (3 . 4))
@end(code)"
  (imap #'cons iter1 iter2))

(sera:-> zip* (&rest iterator)
         (values iterator &optional))
(defun zip* (&rest iterators)
  "Create an iterator which returns lists of values from @c(iterators).

@begin[lang=lisp](code)
(collect
    (take 3 (zip* (count-from 1)
                  (count-from 2)
                  (count-from 3)))) -> '((1 2 3) (2 3 4) (3 4 5))
@end(code)"
  (apply #'imap #'list iterators))

;; Enumeration
(sera:-> enumerate (iterator)
         (values iterator &optional))
(defun enumerate (iterator)
  "Creatre an iterator which enumerates values of @c(iterator),
i.e. translates a value @c(x) to a cons cell @c((i . x)) where @c(i)
is an integer which is incremented by 1 starting from 0.

@begin[lang=lisp](code)
(collect (enumerate (replicate 3 2))) -> '((0 . 3) (1 . 3))
@end(code)"
  (zip (count-from 0) iterator))

;; Repetition
(defun repeat/next (value)
  (lambda (state)
    (declare (ignore state))
    (values value t)))

(sera:-> repeat (t)
         (values iterator &optional))
(defun repeat (value)
  "Create an iterator which repeats @c(value) infinitely."
  (iterator (repeat/next value) t))

(sera:-> replicate (t (integer 0))
         (values iterator &optional))
(defun replicate (value n)
  "Create an iterator which repeats @c(value) @c(n) times.

@begin[lang=lisp](code)
(replicate x n) = (take n (repeat x))
@end(code)"
  (take n (repeat value)))

;; Folding
(sera:-> foldl ((function (t t) t) t iterator)
         (values t &optional))
(declaim (inline foldl))
(defun foldl (function init iterator)
  "Left-associative fold for iterators. The returned value is the same
as for the following, but without consing:

@begin[lang=lisp](code)
(reduce function (collect iterator) :initial-value init)
@end(code)"
  (declare (optimize (speed 3)))
  (let ((acc init))
    (do-iterator (x iterator)
      (setq acc (funcall function acc x)))
    acc))

(sera:-> foldr ((function (t t) t) t iterator)
         (values t &optional))
(declaim (inline foldr))
(defun foldr (function init iterator)
  "Right-associative fold for iterators. The returned value is the same
as for the following, but maybe more effective:

@begin[lang=lisp](code)
(reduce function (collect iterator) :initial-value init :from-end t)
@end(code)"
  (declare (optimize (speed 3)))
  (funcall
   (foldl (lambda (g x) (lambda (y) (funcall g (funcall function x y))))
          #'identity iterator)
   init))

;; Cycle
(defun cycle/next (next first-state)
  (labels ((%next (state)
             (multiple-value-bind (value next-state)
                 (funcall next state)
               (if (eq next-state 'stop)
                   (%next first-state)
                   (values value next-state)))))
    #'%next))

(sera:-> cycle (iterator)
         (values iterator &optional))
(defun cycle (iterator)
  "Repeat values of @c(iterator) infinitely.

@begin[lang=lisp](code)
(collect (take 5 (cycle (list->iterator '(1 2 3))))) -> '(1 2 3 1 2)
@end(code)"
  (iterator (cycle/next (iterator-next iterator)
                        (iterator-init-state iterator))
            (iterator-init-state iterator)))

(defun concat/next (state)
  (destructuring-bind (current-state . iterators)
      state
    (multiple-value-bind (value next-state)
        (funcall (iterator-next (car iterators)) current-state)
      (if (eq next-state 'stop)
          (let ((tail (cdr iterators)))
            (if (null tail)
                (values nil 'stop)
                (concat/next (cons (iterator-init-state (car tail)) tail))))
          (values value (cons next-state iterators))))))

(defun concat (&rest iterators)
  "Concatenate one or more iterators into one iterator.

@begin[lang=lisp](code)
(collect (concat (replicate 3 2) (replicate 1 3))) -> '(3 3 1 1 1)
@end(code)"
  (iterator #'concat/next
            (cons (iterator-init-state (car iterators))
                  iterators)))

;; Product
(defun product/next (next-outer next-inner init-state-inner)
  (labels ((%product (state)
             (destructuring-bind (state-outer . state-inner)
                 state
               (multiple-value-bind (value-outer next-state-outer)
                   (funcall next-outer state-outer)
                 (if (eq next-state-outer 'stop)
                     (values nil 'stop)
                     (multiple-value-bind (value-inner next-state-inner)
                         (funcall next-inner state-inner)
                       (if (eq next-state-inner 'stop)
                           (%product (cons next-state-outer init-state-inner))
                           (values (cons value-outer value-inner)
                                   (cons state-outer next-state-inner)))))))))
    #'%product))
                  
(sera:-> product (iterator iterator)
         (values iterator &optional))
(defun product (outer inner)
  "Create an iterator which has all possible pairs @c((x . y)) where
@c(x) ∈ @c(outer) and @c(y) ∈ @c(inner).

@begin[lang=lisp](code)
(collect (product (range 1 3) (range 3 5))) -> '((1 . 3) (1 . 4) (2 . 3) (2 . 4))
@end(code)"
  (iterator (product/next (iterator-next outer)
                          (iterator-next inner)
                          (iterator-init-state inner))
            (cons (iterator-init-state outer)
                  (iterator-init-state inner))))

;; Filter
(defun filter/next (next predicate)
  (labels ((%next (state)
             (multiple-value-bind (value next-state)
                 (funcall next state)
               (cond
                 ((eq next-state 'stop)
                  (values nil 'stop))
                 ((funcall predicate value)
                  (values value next-state))
                 (t (%next next-state))))))
    #'%next))

(sera:-> filter ((function (t) (values t &optional)) iterator)
         (values iterator &optional))
(defun filter (predicate iterator)
  "Create an iterator which returns only those values of @c(iterator)
which satisfy @c(predicate).

@begin[lang=lisp](code)
(collect (take 6 (filter #'oddp (count-from 0)))) -> '(1 3 5 7 9 11)
@end(code)"
  (iterator (filter/next (iterator-next iterator) predicate)
            (iterator-init-state iterator)))

(sera:-> indices (list)
         (values iterator &optional))
(defun indices (dimensions)
  "For a list of array dimensions, return an iterator which iterates
through all possible indices in the array.

@begin[lang=lisp](code)
(collect (indices '(3 4 2))) ->
'((0 0 0) (0 0 1) (0 1 0) (0 1 1) (0 2 0) (0 2 1) (0 3 0) (0 3 1) (1 0 0)
  (1 0 1) (1 1 0) (1 1 1) (1 2 0) (1 2 1) (1 3 0) (1 3 1) (2 0 0) (2 0 1)
  (2 1 0) (2 1 1) (2 2 0) (2 2 1) (2 3 0) (2 3 1))
@end(code)"
  (reduce #'product dimensions
          :key (lambda (d) (range 0 d))
          :from-end t
          :initial-value (singleton nil)))

(sera:-> power (iterator (integer 1))
         (values iterator &optional))
(defun power (iterator n)
  "For an iterator which contains a set of elements \\(A\\), return an
  iterator which contains all elements of \\(A^n\\).
@begin[lang=lisp](code)

(collect (power (list->iterator '(-1 0 1)) 2)) ->
'((-1 -1) (-1 0) (-1 1) (0 -1) (0 0) (0 1) (1 -1) (1 0) (1 1))
@end(code)"
  (reduce #'product
          (loop repeat n collect iterator)
          :from-end t
          :initial-value (singleton nil)))

(sera:-> every ((sera:-> (t) (values t &optional)) iterator)
         (values boolean &optional))
(defun every (predicate iterator)
  "Return @c(t) if @c(predicate) is true for every element contained
in the iterator and @c(nil) otherwise."
  (let ((next  (iterator-next iterator))
        (state (iterator-init-state iterator)))
    (labels ((%every (state)
               (multiple-value-bind (value next-state)
                   (funcall next state)
                 (cond
                   ((eq next-state 'stop) t)
                   ((funcall predicate value)
                    (%every next-state))
                   (t nil)))))
      (%every state))))

(sera:-> some ((sera:-> (t) (values t &optional)) iterator)
         (values boolean &optional))
(defun some (predicate iterator)
  "Return @c(t) if @c(predicate) is true for at least one element
contained in the iterator and @c(nil) otherwise."
  (let ((next  (iterator-next iterator))
        (state (iterator-init-state iterator)))
    (labels ((%some (state)
               (multiple-value-bind (value next-state)
                   (funcall next state)
                 (cond
                   ((eq next-state 'stop) nil)
                   ((funcall predicate value) t)
                   (t (%some next-state))))))
      (%some state))))

(sera:-> find-if ((sera:-> (t) (values t &optional)) iterator)
         (values t iterator &optional))
(defun find-if (predicate iterator)
  "Return the first value which satisfies the predicate. The second
returned value is an iterator which contains values past this
value. You can use @c(drop-while) if the first value is not needed."
  (declare (optimize (speed 3)))
  (let ((next       (iterator-next       iterator))
        (init-state (iterator-init-state iterator)))
    (labels ((%find (state)
               (multiple-value-bind (value next-state)
                   (funcall next state)
                 (cond
                   ((eq next-state 'stop)
                    (values nil 'stop))
                   ((funcall predicate value)
                    (values value next-state))
                   (t (%find next-state))))))
      (multiple-value-bind (value state)
          (%find init-state)
        (values value (iterator next state))))))

(sera:-> length (iterator)
         (values alex:non-negative-fixnum &optional))
(defun length (iterator)
  "Return a number of elements in the iterator"
  (foldl
   (lambda (acc element)
     (declare (ignore element))
     (1+ acc))
   0 iterator))

;; Undefined
(define-condition undefined-value (error)
  ((string :reader        undefined-value-string
           :initarg       :string
           :documentation "Additional clarification of what happened"))
  (:report
   (lambda (c s)
     (format s "Trying to force an undefined value: ~a"
             (undefined-value-string c))))
  (:documentation "A condition signalled by @c(undefined)."))

(defun undefined/next (state)
  (values (error 'undefined-value :string state) nil))

(sera:-> undefined (string)
         (values iterator &optional))
(defun undefined (string)
  "When a value is forced out of this iterator, a condition of type
@c(undefined-value) is signalled with a supplied text."
  (iterator #'undefined/next string))
