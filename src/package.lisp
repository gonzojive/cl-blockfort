(defpackage cl-blockfort
  (:use :common-lisp)
  (:nicknames :blockfort :bfort)
  (:export
   ;; macros
   #:with-transaction
   ;; classes
   #:persistent-heap
   ;;; heap operations
   #:open-heap #:heap-close #:heap-read-sequence #:heap-write-sequence #:heap-environment
   #:heap-expand
   #:heap-size
   #:heap-begin-transaction #:heap-commit-transaction #:heap-rollback-transaction
   ;; variables
   #:*heap*
   ))

(in-package :blockfort)






