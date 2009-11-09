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
			 (:file "conditions" :depends-on ("package"))
			 (:file "crc" :depends-on ("package"))
			 (:file "files" :depends-on ("package" "conditions" "basic-threading"))
			 ;; (abstract) network nodes
			 ;; (:file "nodes")
			 ;; Concurrency
			 (:file "basic-threading" :depends-on ("package"))
			 (:file "local-locks" :depends-on ("package"))
			 (:file "distributed-locks" :depends-on ("package"))
			 ;; Logging
			 (:file "log" :depends-on ("files" "package" "crc" "basic-threading"))
			 
			 ;(:file "store" :depends-on ("log" "files" "distributed-locks"))

	       )))
  :depends-on ("bordeaux-threads" "trivial-garbage" "alexandria" "usocket"))

(defsystem cl-blockfort-tests
  :components ((:module "test"
                        :components ((:file "test-package")
				     (:file "log-tests" :depends-on ("test-package"))
				     (:file "locks-tests" :depends-on ("test-package"))
				     )))
  :depends-on ("cl-blockfort" "stefil"))
