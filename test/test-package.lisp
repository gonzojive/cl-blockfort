(defpackage cl-blockfort-tests
  (:use :common-lisp :cl-blockfort :stefil)
  (:nicknames :blockfort-tests :bfort-tests)
  (:export #:cl-blockfort-tests))

(in-package bfort-tests)

(defsuite cl-blockfort-tests)

(in-suite cl-blockfort-tests)

(defparameter *test-directory-pathname*
  (asdf:component-pathname (asdf:find-component (asdf:find-system :cl-blockfort-tests) "test")))