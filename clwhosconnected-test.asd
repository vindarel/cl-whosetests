#|
  This file is a part of clwhosconnected project.
|#

(in-package :cl-user)
(defpackage clwhosconnected-test-asd
  (:use :cl21
        :asdf))
(in-package :clwhosconnected-test-asd)

(defsystem clwhosconnected-test
  :author ""
  :license ""
  :depends-on (:clwhosconnected
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "clwhosconnected"))))
  :description "Test system for clwhosconnected"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
