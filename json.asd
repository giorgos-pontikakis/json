;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(in-package :cl)

(asdf:defsystem :json
    :serial t
    :depends-on (:iterate)
    :components ((:file "package") 
		 (:file "json")))




