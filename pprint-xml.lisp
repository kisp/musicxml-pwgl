;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(defpackage #:pprint-xml
  (:nicknames #:ppxml)
  (:use #:cl)
  (:export
   #:pprint-xml))

(in-package #:pprint-xml)

(defparameter *pprint-xml-table* (copy-pprint-dispatch nil))

(defun pprint-redispatch (obj stream)
  (funcall (pprint-dispatch obj) stream obj))

(set-pprint-dispatch
 '(cons (cons symbol t) t)
 (lambda (stream obj)
   (destructuring-bind ((elt &rest attributes) &rest elts)
       obj
     (pprint-logical-block (stream nil)
       (pprint-logical-block (stream nil :prefix "<" :suffix ">")
	 (write-string (string elt) stream)
	 (let ((*print-pretty* nil))
	   (format stream "~{ ~A='~A'~}" attributes)))
       (format stream "~{~W~}" elts)
       (let ((*print-pretty* nil))
	 (format stream "</~A>" elt)))))
 0 *pprint-xml-table*)

(set-pprint-dispatch
 '(cons symbol t)
 (lambda (stream obj)
   (pprint-redispatch (cons (list (car obj)) (cdr obj)) stream))
 0 *pprint-xml-table*)

(set-pprint-dispatch
 'symbol
 (lambda (stream obj)
   (let ((*print-pretty* nil))
     (format stream "<~A/>" obj)))
 0 *pprint-xml-table*)

(set-pprint-dispatch
 'string
 (lambda (stream obj)
   (write-string obj stream))
 0 *pprint-xml-table*)

(defun pprint-xml (dom &key (stream t))
  (let ((*print-pprint-dispatch* *pprint-xml-table*))
    (pprint dom stream)))
