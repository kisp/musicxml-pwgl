;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

;;; This file is part of MusicXML-PWGL.

;;; Copyright (c) 2010, Kilian Sprotte. All rights reserved.

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

(defpackage #:musicxml-pwgl.xml-filter
  (:use #:cl)
  (:export
   #:filter-string
   #:filter-file))

(in-package #:musicxml-pwgl.xml-filter)

(defclass filter (cxml:sax-proxy)
  ((names     :accessor names     :initarg :names
              :initform '("b"))
   (gate-open :accessor gate-open :initform t)))

(defmethod sax:start-element
    ((handler filter) namespace-uri local-name qname attributes)
  (if (not (member local-name (names handler) :test #'string=))
      (when (gate-open handler) (call-next-method))
      (setf (gate-open handler) nil)))

(defmethod sax:end-element
    ((handler filter) namespace-uri local-name qname)
  (if (not (member local-name (names handler) :test #'string=))
      (when (gate-open handler) (call-next-method))
      (setf (gate-open handler) t)))

(defmethod sax:characters ((handler filter) data)
  (when (gate-open handler) (call-next-method)))



(defun filter-string (string element-names)
  (let* ((octets (sb-ext:string-to-octets string))
         (dom (cxml:parse-octets octets (cxml-dom:make-dom-builder))))
    (dom:map-document
     (make-instance 'filter
                    :names element-names
                    :chained-handler (cxml:make-string-sink :canonical t))
     dom)))

(defun filter-file (path out-path element-names)
  (let ((dom (cxml:parse-file path (cxml-dom:make-dom-builder))))
    (with-open-file (out out-path :direction :output :if-exists :supersede)
      (dom:map-document (make-instance
                         'filter
                         :names element-names
                         :chained-handler
                         (cxml:make-character-stream-sink out)) dom))))
