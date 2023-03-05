(in-package :stateless-iterators-tests)

(def-suite stateless-iterators :description "Test stateless iterators")

(defun run-tests ()
  (let ((status (run 'stateless-iterators)))
    (explain! status)
    (results-status status)))

(defun cequalp (iterator list)
  (equalp (si:collect iterator) list))

(in-suite stateless-iterators)

(test sequence->iterator
  (let ((list   '(1 2 3))
        (vector #(1 2 3)))
    (is (cequalp (si:list->iterator   list)   list))
    (is (cequalp (si:vector->iterator vector) list))))

(test count-from+take
  (is (cequalp (si:take 5 (si:count-from 2)) '(2 3 4 5 6))))

(test range
  (is (cequalp (si:range 1 10)   '(1 2 3 4 5 6 7 8 9)))
  (is (cequalp (si:range 1 10 2) '(1 3 5 7 9))))

(test take-while
  (is (cequalp (si:take-while #'oddp (si:list->iterator '(1 3 5 4 1 3 1))) '(1 3 5)))
  (is (cequalp (si:take-while #'oddp (si:list->iterator '(1 3 5 1 3 1)))   '(1 3 5 1 3 1)))
  (is (cequalp (si:take-while #'oddp (si:list->iterator '(2 1 3 5 1 3 1))) '())))

(test drop-while
  (is (cequalp (si:drop-while #'oddp (si:list->iterator '(3 5 7 1))) '()))
  (is (cequalp (si:drop-while #'oddp (si:list->iterator '()))        '()))
  (is (cequalp (si:drop-while #'oddp (si:list->iterator '(1 3 2 1))) '(2 1))))

(test imap
  (is (cequalp (si:take 5 (si:imap (alex:curry #'* 2)
                                   (si:count-from 0)))
               '(0 2 4 6 8))))

(test zip
  (is (cequalp (si:zip (si:list->iterator '(1 3 2 1))
                       (si:list->iterator '(#\a #\b)))
               '((1 . #\a) (3 . #\b))))
  (is (cequalp (si:zip* (si:list->iterator '(1 3 2 1))
                        (si:list->iterator '(#\a #\b))
                        (si:list->iterator '("a" "b" "c")))
               '((1 #\a "a") (3 #\b "b")))))

(test enumerate
  (is (cequalp (si:enumerate (si:list->iterator '(#\a #\b #\c)))
               '((0 . #\a) (1 . #\b) (2 . #\c)))))

(test repetition
  (is (cequalp (si:take 2 (si:repeat #\a))
               '(#\a #\a)))
  (is (cequalp (si:replicate 5 3) '(5 5 5))))

(test foldl
  (is (= (si:foldl (sera:hook2 #'+ (alex:curry #'* 2)) 0
                   (si:replicate 2 3))
         12)))

(test iterate
  (is (cequalp (si:take 4 (si:iterate (alex:rcurry #'/ 2) 1))
               '(1 1/2 1/4 1/8))))

(test cycle
  (is (cequalp (si:take 10 (si:cycle (si:list->iterator '(1 2 3))))
               '(1 2 3 1 2 3 1 2 3 1))))

(test concat
  (is (cequalp (si:concat (si:list->iterator '(1 2 3))
                          (si:list->iterator '())
                          (si:replicate 10 2)
                          (si:imap #'1+ (si:range 1 4)))
               '(1 2 3 10 10 2 3 4))))

(test product
  (let ((a (si:range 0 2)))
    (is (cequalp (si:imap #'alex:flatten
                          (si:product a (si:product a a)))
                 '((0 0 0) (0 0 1) (0 1 0) (0 1 1)
                   (1 0 0) (1 0 1) (1 1 0) (1 1 1))))))
