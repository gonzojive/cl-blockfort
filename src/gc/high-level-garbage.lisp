(defpackage :high-level-garbage
    (:nicknames :hl-garbage :hl-gc)
  (:use :common-lisp :alexandria))

(in-package :high-level-garbage)

(defgeneric mark-and-sweep (world &key &allow-other-keys)
  (:documentation "Garbage collect stuff in the given thing."))

(defgeneric map-all-objects (world lambda &key recursive &allow-other-keys)
  (:documentation "Calls lambda on every object in the world."))

(defgeneric map-root-objects (world lambda &key recursive &allow-other-keys)
  (:documentation "Garbage collect stuff in the given thing."))

(defgeneric mark (world object &key marking-data &allow-other-keys)
  (:documentation "Marks the world.  May use either marking-data or the world itself
to store information about marked object."))

(defgeneric sweep (world &key marking-data &allow-other-keys)
  (:documentation "Sweeps the world, resetting any 'marked' flag to
false if necessary along the way."))

(defmethod mark-and-sweep (world &key marking-data &allow-other-keys)
  ;; breadth-first marking
  (map-root-objects world
		    #'(lambda (obj)
			(setf marking-data
			      (mark world obj :marking-data marking-data))))
  (sweep world :marking-data marking-data))

