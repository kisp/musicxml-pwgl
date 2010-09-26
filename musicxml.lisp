;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(defpackage #:musicxml
  (:nicknames #:mxml)
  (:use #:cl #:pprint-xml)
  (:export
   #:from-lxml
   #:to-lxml
   #:make-constructor-form
   #:pitch))

(in-package #:musicxml)

(defun from-lxml (dom)
  (translate-from-lxml dom (car dom)))

(defun to-lxml (obj)
  (translate-to-lxml obj))

(defgeneric make-constructor-form (obj))

(defun generic-pretty-printer (stream obj)
  (pprint-redispatch (translate-to-lxml obj) stream))

(defmacro assoc-bind (bindings exp &body body)
  (let ((=exp= (gensym "=EXP=")))
    `(let ((,=exp= ,exp))
       (let ,(mapcar
	      (lambda (binding)
		`(,binding
		  (second (assoc
			   ,(intern (string-downcase (string binding))
				    "KEYWORD")
			   ,=exp=))))
	      bindings)
	 ,@body))))

(defstruct musicxml-object)

(defmethod print-object ((musicxml-object musicxml-object) stream)
  (write-string "#." stream)
  (prin1 (make-constructor-form musicxml-object) stream))

(defstruct (pitch (:include musicxml-object))
  step alter octave)

(defmethod translate-from-lxml (dom (type (eql ':|pitch|)))
  (assoc-bind (step alter octave) (cdr dom)
    (make-pitch :step (intern step)
		:alter (parse-integer alter)
		:octave (parse-integer octave))))

(defmethod translate-to-lxml ((pitch pitch))
  `(:|pitch|
     (:|step| ,(string (pitch-step pitch)))
     (:|alter| ,(princ-to-string (pitch-alter pitch)))
     (:|octave| ,(princ-to-string (pitch-octave pitch)))))

(defmethod make-constructor-form ((pitch pitch))
  `(pitch ',(pitch-step pitch)
	  ,(pitch-alter pitch)
	  ,(pitch-octave pitch)))

(defun pitch (step alter octave)
  (make-pitch :step step :alter alter :octave octave))

(set-pprint-dispatch 'pitch 'generic-pretty-printer 0 *pprint-xml-table*)
