(in-package :blockfort)

;;; utility
(defun integer-to-octet-vector (octet-count integer)
  "Given an integer and the number of bytes of desired output, returns a vector of lenghth
BYTE-COUNT with a Little Endian integer in it."
  (declare (optimize speed)
	   (type integer integer)
	   (type fixnum octet-count))
  (let ((array (make-array octet-count :element-type '(unsigned-byte 8))))
    (integer-into-octet-vector array octet-count integer)))

(defun integer-into-octet-vector (octet-vector octet-count integer &key (start 0))
  "Given an integer and a byte vector, inserts OCTET-COUNT bytes into OCTET-VECTOR
beginning START bytes from the first byte."
  (declare (type integer integer)
	   (type fixnum octet-count start)
	   (type (simple-array (unsigned-byte 8)) octet-vector)
	   (optimize (speed 3)))
  (dotimes (i octet-count)
    (setf (elt octet-vector (+ i start))
	  (ldb (byte 8 (* 8 i)) integer)))
  octet-vector)

(defun octet-vector-to-integer (vector &optional signed?)
  "Given a byte vector in Little Endian form, returns a signed integer."
  (let ((unsigned-value 0))
    (dotimes (i (length vector))
      (incf unsigned-value (ash (elt vector i) (* 8 i))))
    (if (and signed?
	     (>= unsigned-value (ash 1 (1- (* 8 (length vector))))))
	(- unsigned-value (ash 1 (* 8 (length vector))))
	unsigned-value)))

