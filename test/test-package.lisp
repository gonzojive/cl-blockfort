(defpackage cl-blockfort-tests
  (:use :common-lisp :cl-blockfort :hu.dwim.stefil :cl-transaction-log)
  (:nicknames :blockfort-tests :bfort-tests)
  (:export #:cl-blockfort-tests))

(in-package bfort-tests)

(defsuite cl-blockfort-tests)
(defsuite cl-transaction-log-tests)

(in-suite cl-blockfort-tests)

(defparameter *test-directory-pathname*
  (asdf:component-pathname (asdf:find-component (asdf:find-system :cl-blockfort-tests) "test")))

(defun run-all-tests ()
  (cl-blockfort-tests)
  (cl-transaction-log-tests))
  