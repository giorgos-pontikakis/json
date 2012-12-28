;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(in-package :cl)

(defpackage :json-asdf
    (:use :cl :asdf))

(in-package :json-asdf)

(defsystem :json
  :serial t
  :components ((:file "package")
               (:file "json")))

(defsystem :json-tests
  :depends-on (:json :hu.dwim.stefil)
  :serial t
  :components ((:module "tests"
                :serial t
                :components ((:file "package")
                             (:file "json-tests")))))
