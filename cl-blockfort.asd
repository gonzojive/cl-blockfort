(defpackage org.iodb.cl-blockfort.system
  (:use #:cl #:asdf))

(in-package :org.iodb.cl-blockfort.system)

(defsystem cl-blockfort
  :description "An ACID-compliant, persistent, and eventually distributed heap."
  :version "0.2.0"
  :author "Red Daly <reddaly at gmail>"
  :license "LGPL"
  :components ((:module "src"
			:components
			((:file "package")
			 (:file "files" :depends-on ("package"))
			 (:file "log" :depends-on ("files" "package"))
			 (:file "heap" :depends-on ("log"))
			 ;;(:file "sails" :depends-on ("package"))
	       )))
  :depends-on ())

(defsystem cl-blockfort-tests
  :components ((:module "test"
                        :components ((:file "test-package")
				     (:file "log-tests" :depends-on ("test-package"))
				     )))
  :depends-on ("cl-blockfort" "stefil"))
