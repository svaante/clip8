(defsystem :clip8
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on (:sdl2 :alexandria :cl-interpol :adopt)
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description ""
  :build-operation "program-op"
  :build-pathname "clip8"
  :entry-point "clip8::main"
  :in-order-to ((test-op (test-op "clip8/tests"))))

(defsystem "clip8/tests"
  :author ""
  :license ""
  :depends-on ("clip8"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for clip8"
  :perform (test-op (op c) (symbol-call :rove :run c)))
