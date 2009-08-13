(in-package :blockfort)

;;;; files.lisp
;;;; The definition of the class BINARY-FILE, a class for dealing with binary files.

(defclass binary-file ()
  ((path
    :initarg :path :initarg :path :accessor binary-file-path
    :documentation "The pathname of the file.")
   (stream
    :initarg :stream :accessor binary-file-stream
    :documentation "File stream of the open binary file, or nil if it is not open.")))

(defmethod initialize-instance :after ((file binary-file) &key (if-does-not-exist :create) (if-exists :new-version))
  "Open up the binary stream associated with the given file."
  (setf (binary-file-stream file)
	(open (binary-file-path file)
	      :direction :io
	      :element-type '(unsigned-byte 8)
	      :if-exists if-exists
	      :if-does-not-exist if-does-not-exist)))

(defmethod close-file ((bf binary-file))
  (close (binary-file-stream bf)))