(in-package :cl-blockfort)

;;;; INTERFACE

(defgeneric acquire-lock-on-node (node place store &key &allow-other-keys)
  (:documentation "Acquires a lock on the data designated by the PLACE
in the given NODE attached to the given data store."))

(defgeneric release-lock-on-node (node lock store &key &allow-other-keys)
  (:documentation "Releases the lock on the data designated by the
PLACE in the given NODE attached to the given data store."))

