#|
  This file is a part of clwhosconnected project.
|#

(in-package :cl-user)
(defpackage clwhosconnected-asd
  (:use :cl :asdf))
(in-package :clwhosconnected-asd)

(defsystem clwhosconnected
  :version "0.1"
  :author ""
  :license ""
  :depends-on (:cl21
               :dexador
               :plump
               :lquery
               :lparallel
               :alexandria
               :str
               :cl-annot
               :cl-readline
               :osicat
               :unix-opts
               :cl-ansi-text)
  :components ((:module "src"
                :components
                ((:file "clwhosconnected"))))
  ;; build
  :build-operation "program-op"
  :build-pathname "whosconnected"
  :entry-point "clwhosconnected:main"

  :description ""
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op clwhosconnected-test))))
