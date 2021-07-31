(defsystem "clutter"
  :version "0.1.0"
  :author "Alessio Stalla"
  :license "LGPL"
  :depends-on ("closer-mop" "doplus")
  :components ((:module "src"
                :components
                ((:file "packages") (:file "clutter"))))
  :description "Cluttering classes and slots with annotations/decorators/attributes metadata"
  :in-order-to ((test-op (test-op "clutter/tests"))))

(defsystem "clutter/tests"
  :author "Alessio Stalla"
  :license "LGPL"
  :depends-on ("clutter" "rove")
  :components ((:module "test"
                :components
                ((:file "tests"))))
  :description "Test system for clutter"
  :perform (test-op (op c) (symbol-call :rove :run c)))
