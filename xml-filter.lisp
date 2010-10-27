;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(defpackage #:xml-filter
  (:use #:cl)
  (:export
   #:filter-string
   #:filter-file))

(in-package #:xml-filter)

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
