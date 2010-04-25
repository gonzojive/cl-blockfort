(in-package :blockfort)

;;;; files.lisp
;;;; The definition of the class BINARY-FILE, a class for dealing with binary files.

(defclass binary-file ()
  ((path
    :initarg :path :initarg :path :accessor binary-file-path
    :documentation "The pathname of the file."))
  (:documentation "Thread-safe access to binary file io streams."))

(defun binary-file-stream (binary-file)
  "Maps a binary file to an open stream that the current thread may
use to access the binary file's contents."
  (let* ((file->stream (or (thread-local-binding file->stream)
			  (setf (thread-local-binding file->stream)
				(make-hash-table))))
        (result (or (gethash binary-file file->stream)
                    (setf (gethash binary-file file->stream)
                          (open-thread-local-stream binary-file :if-exists :overwrite)))))
;    (format t "REsult of binary-file-stream ~A: ~A~%" binary-file result)
    result))

(defun open-thread-local-stream (binary-file &key if-exists if-does-not-exist (direction :io))
  "Opens a new stream in the current thread for the given binary file and sets up
for garbage collection later in life."
  (let ((stream (open (binary-file-path binary-file)
		      :direction direction
		      :element-type '(unsigned-byte 8)
		      :if-exists if-exists
		      :if-does-not-exist if-does-not-exist)))
    (trivial-garbage:finalize (bordeaux-threads:current-thread)
			      #'(lambda () (close stream)))
    stream))

;(defmethod close-file ((bf binary-file))
;  (close (binary-file-stream bf)))