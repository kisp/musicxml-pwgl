;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(defpackage #:musicxml
  (:nicknames #:mxml)
  (:use #:cl #:pprint-xml)
  (:export
   #:from-lxml
   #:to-lxml
   #:make-constructor-form
   #:pitch
   #:rest*))

(in-package #:musicxml)

(defun from-lxml (dom)
  (cond ((consp dom)
	 (translate-from-lxml dom (car dom)))
	((stringp dom) dom)
	((keywordp dom)
	 (translate-from-lxml dom dom))
	(t
	 (error "dunno with ~S?" dom))))

(defmethod translate-from-lxml (dom type)
  (if (consp dom)
      (cons (car dom)
	    (mapcar 'from-lxml (cdr dom)))
      dom))

(defun to-lxml (obj)
  (translate-to-lxml obj))

(defmethod translate-to-lxml (obj)
  (if (consp obj)
      (cons (car obj)
	    (mapcar 'translate-to-lxml (cdr obj)))
      obj))

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

(defun assoc* (item list)
  (if (null list)
      nil
      (let ((candidate (first list)))
	(if (cond ((and (consp candidate)
			(consp (first candidate)))
		   (eql item (caar candidate)))
		  ((consp candidate)
		   (eql item (first candidate)))
		  (t
		   (eql item candidate)))
	    candidate
	    (assoc* item (rest list))))))

(defmacro assoc-bind* (bindings exp &body body)
  (let ((=exp= (gensym "=EXP=")))
    `(let ((,=exp= ,exp))
       (let ,(mapcar
	      (lambda (binding)
		`(,binding
		  (assoc*
		   ,(intern (string-downcase (string binding))
			    "KEYWORD")
		   ,=exp=)))
	      bindings)
	 ,@body))))

(defstruct musicxml-object)

(defmethod print-object ((musicxml-object musicxml-object) stream)
  (write-string "#." stream)
  (prin1 (make-constructor-form musicxml-object) stream))

;;; pitch
(defstruct (pitch (:include musicxml-object))
  step alter octave)

(defmethod translate-from-lxml (dom (type (eql ':|pitch|)))
  (assoc-bind (step alter octave) (cdr dom)
    (make-pitch :step (intern step)
		:alter (if (null alter) 0 (parse-integer alter))
		:octave (parse-integer octave))))

(defmethod translate-to-lxml ((pitch pitch))
  `(:|pitch|
     (:|step| ,(string (pitch-step pitch)))
     ,@(unless
	(eql 0 (pitch-alter pitch))
	`((:|alter| ,(princ-to-string (pitch-alter pitch)))))
     (:|octave| ,(princ-to-string (pitch-octave pitch)))))

(defmethod make-constructor-form ((pitch pitch))
  `(pitch ',(pitch-step pitch)
	  ,(pitch-alter pitch)
	  ,(pitch-octave pitch)))

(defun pitch (step alter octave)
  (make-pitch :step step :alter alter :octave octave))

(set-pprint-dispatch 'pitch 'generic-pretty-printer 0 *pprint-xml-table*)

;;; rest*
(defstruct (rest* (:include musicxml-object)))

(defmethod translate-from-lxml (dom (type (eql ':|rest|)))
  (make-rest*))

(defmethod translate-to-lxml ((rest* rest*))
  :|rest|)

(defmethod make-constructor-form ((rest* rest*))
  '(rest*))

(defun rest* () (make-rest*))

(set-pprint-dispatch 'rest* 'generic-pretty-printer 0 *pprint-xml-table*)

;;; note
(defstruct (note (:include musicxml-object))
  pitch-or-rest duration chordp staff accidental type
  notations tie)

(defmethod translate-from-lxml (dom (type (eql ':|note|)))
  (assoc-bind* (duration chord rest pitch staff
			 accidental type notations tie)
      dom
    (make-note :pitch-or-rest (if rest (rest*) (from-lxml pitch))
	       :duration (parse-integer (second duration))
	       :chordp chord
	       :staff (and staff (parse-integer (second staff)))
	       :accidental (and accidental (intern (string-upcase (second accidental))))
	       :type (and type (intern (string-upcase (second type))))
	       :notations (second notations)
	       :tie (and tie (intern (string-upcase (third (first tie))))))))

(defmethod translate-to-lxml ((note note))
  `(:|note|
     ,@(when (note-chordp note) '(:|chord|))
     ,(translate-to-lxml (note-pitch-or-rest note))
     (:|duration| ,(princ-to-string (note-duration note)))
     ,@(when (note-tie note)
	     `(((:|tie| :|type| ,(string-downcase (symbol-name (note-tie note)))))))
     ,@(when (note-type note)
	     `((:|type| ,(string-downcase (symbol-name (note-type note))))))
     ,@(when (note-accidental note)
	     `((:|accidental| ,(string-downcase (symbol-name (note-accidental note))))))
     ,@(when (note-staff note)
	     `((:|staff| ,(princ-to-string (note-staff note)))))
     ,@(when (note-notations note)
	     `((:|notations| ,(note-notations note))))))

(defmethod make-constructor-form ((note note))
  `(note ,(note-pitch-or-rest note)
	 ,(note-duration note)
	 ',(note-type note)
	 ',(note-accidental note)
	 :chordp ,(note-chordp note)
	 :tie ',(note-tie note)
	 :staff ,(note-staff note)
	 :notations ,(note-notations note)))

(defun note (pitch-or-rest duration type accidental &key chordp staff notations tie)
  (make-note :pitch-or-rest pitch-or-rest :duration duration :chordp chordp :staff staff
	     :accidental accidental :type type :notations notations :tie tie))

(set-pprint-dispatch 'note 'generic-pretty-printer 0 *pprint-xml-table*)
