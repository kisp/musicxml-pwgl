;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

;;; This file is part of MusicXML-PWGL.

;;; Copyright (c) 2010 - 2011, Kilian Sprotte. All rights reserved.

;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.

;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(asdf:defsystem musicxml-pwgl
  :version "0.1.130"
  :description "musicxml export for pwgl"
  :maintainer "Kilian Sprotte <kilian.sprotte@gmail.com>"
  :author "Kilian Sprotte <kilian.sprotte@gmail.com>"
  :licence "GPLv3"
  :depends-on (s-xml cxml myam sqlite-orm drakma alexandria)
  :serial t
  :components ((:file "packages")
               (:file "mapcar-state")
               (:file "pprint-xml")
               (:file "musicxml")
               (:file "enp2musicxml")
               (:file "test-db")
               (:file "xml-filter")
               (:file "test")))

(defmethod perform ((op asdf:test-op)
                    (system (eql (asdf:find-system :musicxml-pwgl))))
  (asdf:oos 'asdf:load-op :musicxml-pwgl)
  (funcall (intern "RUN!" "MYAM") :musicxml-pwgl))
