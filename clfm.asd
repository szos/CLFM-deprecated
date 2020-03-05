;;;; clfm.asd

(asdf:defsystem #:clfm
  :description "Describe clfm here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:mcclim
	       #:slim
	       #:cl-ppcre
	       #:uiop
	       #:osicat
	       #:alexandria)
  :components ((:file "package")
	       (:file "dialog")
	       (:file "filesystem")
	       (:file "shell")
	       (:file "clfm-variables")
	       (:file "open-files")
               (:file "clfm")))
