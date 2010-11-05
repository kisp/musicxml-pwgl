;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(asdf:defsystem musicxml
  :version "0.1.3"
  :description "musicxml export for pwgl"
  :maintainer "Kilian Sprotte <kilian.sprotte@gmail.com>"
  :author "Kilian Sprotte <kilian.sprotte@gmail.com>"
  :licence "GPLv3"
  :depends-on (s-xml cxml myam sqlite-orm)
  :serial t
  :components ((:file "pprint-xml")
               (:file "musicxml")
               (:file "enp2musicxml")
               (:file "test-db")
               (:file "xml-filter")
               (:file "test")))

(defmethod perform ((op asdf:test-op)
                    (system (eql (asdf:find-system :musicxml))))
  (asdf:oos 'asdf:load-op :musicxml)
  (funcall (intern "RUN!" "MYAM") :musicxml))
