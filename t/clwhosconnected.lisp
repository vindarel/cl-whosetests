(in-package :cl-user)
(defpackage clwhosconnected-test
  (:use :cl
        :clwhosconnected
        :prove))
(in-package :clwhosconnected-test)

;; NOTE: To run this test file, execute `(asdf:test-system :clwhosconnected)' in your Lisp.

(plan nil)

(defparameter *names* '(
                        "one"
                        ("two" . ((:keywords . ("second elt"))))
                        ))

(subtest "Checking utils"
  (is (format nil "one~%") (print-name (first *names*) :stream nil) "Printing a name, simple.")
  (is (format nil "second elt~%") ;; use is-print ?
      ;; doesn't catch successive "format" printing .
      (print-name (second *names*) :stream nil) "Printing a name with properties (incomplete).")
  (let ((*watchlist* *names*))
    (is "one" (watched? "one") "watched? with a simple name.")
    (is (second *names*) (watched? "two") "watched? with a cons cell")

    ;; call to watched? doesn't see "two"
    (is '("one" (second *names*)) (mapcar (lambda (it) (watched? it)) *names*))
    ;; the code sees it.
    (is (second *names*) (find "two" *watchlist* :test 'equal :key (lambda (%) (if (consp %) (car %) %))))
    ;; useless test. use is-print ?
    (ok (print-connected *names*) "print-connected does not error out."))
    )


;; blah blah blah.

(finalize)
