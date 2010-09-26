;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(asdf:defsystem musicxml
  :version "0"
  :description "musicxml export for pwgl"
  :maintainer "Kilian Sprotte <kilian.sprotte@gmail.com>"
  :author "Kilian Sprotte <kilian.sprotte@gmail.com>"
  :licence "GPLv3"
  :depends-on (s-xml cxml myam)
  :serial t
  :components ((:file "pprint-xml")	       
	       (:file "musicxml")
	       (:file "test")))
