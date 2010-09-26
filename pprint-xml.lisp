;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(defpackage #:pprint-xml
  (:nicknames #:ppxml)
  (:use #:cl)
  (:export
   #:pprint-xml
   #:remove-whitespace))

(in-package #:pprint-xml)

(defparameter *pprint-xml-table* (copy-pprint-dispatch nil))

(defun pprint-redispatch (obj stream)
  (funcall (pprint-dispatch obj) stream obj))

(defun group-in-pairs (list)
  (loop for tail on list by #'cddr
       collect (list (first tail) (second tail))))

(set-pprint-dispatch
 '(cons (cons symbol t) t)
 (lambda (stream obj)
   (destructuring-bind ((elt &rest attributes) &rest elts)
       obj
     (pprint-logical-block (stream nil)
       (pprint-logical-block (stream nil :prefix "<" :suffix ">")
	 (write-string (string elt) stream)
	 (dolist (pair (group-in-pairs attributes))
	   (write-char #\space stream)
	   (pprint-newline :fill stream)
	   (write-string (string (first pair)) stream)
	   (write-string "='" stream)
	   (write-string (second pair) stream)
	   (write-string "'" stream))
	 (pprint-newline :linear stream))
       (format stream "~{~W~}" elts)
       (write-string "</" stream)
       (write-string (string elt) stream)
       (pprint-indent :block -1 stream)
       (pprint-newline :linear stream)
       (write-string ">" stream))))
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

(defun remove-whitespace (dom)
  "Remove any text elements that contain only whitespace."
  ;; TODO really remove them
  (flet ((whitespace-p (char)
	   (member char '(#\space #\page #\newline #\return #\tab))))
    (subst-if "" (lambda (obj) (and (stringp obj) (every #'whitespace-p obj)))
	      dom)))
