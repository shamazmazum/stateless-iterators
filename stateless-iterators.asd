(defsystem :stateless-iterators
  :name :stateless-iterators
  :version "0.1"
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :description "Stateless iterators similar to those in Lua (or Julia) language"
  :licence "2-clause BSD"
  :serial t
  :pathname "src/"
  :components ((:file "package")
               (:file "stateless-iterators"))
  :depends-on (:serapeum)
  :in-order-to ((test-op (load-op "stateless-iterators/tests")))
  :perform (test-op (op system)
                    (declare (ignore op system))
                    (uiop:symbol-call :stateless-iterators-tests '#:run-tests)))

(defsystem :stateless-iterators/tests
  :name :stateless-iterators/tests
  :version "0.1"
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :licence "2-clause BSD"
  :serial t
  :pathname "tests/"
  :components ((:file "package")
               (:file "tests"))
  :depends-on (:stateless-iterators
               :alexandria
               :serapeum
               :fiveam))
