(defun do-all()
  (ql:quickload :stateless-iterators/tests)
  (uiop:quit
   (if (uiop:call-function "stateless-iterators-tests:run-tests")
       0 1)))

(do-all)
