;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(defpackage #:musicxml
  (:nicknames #:mxml)
  (:use #:cl #:pprint-xml)
  (:export
   #:from-lxml
   #:to-lxml
   #:make-constructor-form
   #:pitch
   #:rest*
   #:print-musicxml
   #:note
   #:time-modification
   #:c #:d #:e #:f #:g #:a #:b
   #:flat #:sharp #:quarter-sharp #:three-quarters-sharp
   #:16th #:eighth #:quarter #:half #:whole))

(in-package #:musicxml)

(defun print-musicxml (dom &key (stream t))
  (write-line
   "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>" stream)
  (write-line "<!DOCTYPE score-partwise PUBLIC" stream)
  (write-line "	\"-//Recordare//DTD MusicXML 2.0 Partwise//EN\"" stream)
  (write-line "	\"http://www.musicxml.org/dtds/partwise.dtd\">" stream)
  (ppxml:pprint-xml dom :stream stream))

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

(defun intern* (name)
  (intern name (find-package "MXML")))

(defstruct musicxml-object)

(defmethod print-object ((musicxml-object musicxml-object) stream)
  (write-string "#." stream)
  (prin1 (make-constructor-form musicxml-object) stream))

;;; pitch
(deftype pitch-step ()
  '(member c d e f g a b))

(defstruct (pitch (:include musicxml-object))
  (step nil :type pitch-step) alter octave)

(defmethod translate-from-lxml (dom (type (eql ':|pitch|)))
  (assoc-bind (step alter octave) (cdr dom)
    (make-pitch :step (intern* step)
		:alter (if (null alter) 0 (read-from-string alter))
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
(deftype accidental ()
  '(member nil sharp natural flat double-sharp sharp-sharp flat-flat
    natural-sharp natural-flat quarter-flat quarter-sharp three-quarters-flat
    three-quarters-sharp))

(deftype note-type ()
  '(member nil 16th eighth quarter half whole))

(defstruct (note (:include musicxml-object))
  pitch-or-rest duration chordp staff
  (accidental nil :type accidental)
  (type nil :type note-type)
  notations tie
  (time-modification nil :type (or null time-modification))
  beam-begin beam-continue beam-end)

(defun beam-element-p (dom)
  (and (consp dom)
       (consp (first dom))
       (eql :|beam| (caar dom))))

(defun decode-beams (list-of-beams)
  "Returns list of beams for start, continue and end."
  (flet ((beam-number (beam)
	   (parse-integer (third (car beam))))
	 (beam-type (beam)
	   (intern* (string-upcase (second beam)))))
    (let (start continue end)
      (dolist (beam list-of-beams)
	(ecase (beam-type beam)
	  (begin (push (beam-number beam) start))
	  (continue (push (beam-number beam) continue))
	  (end (push (beam-number beam) end))))
      (values (nreverse start) (nreverse continue) (nreverse end)))))

(defmethod translate-from-lxml (dom (type (eql ':|note|)))
  (assoc-bind* (duration chord rest pitch staff
			 accidental type notations
			 tie time-modification)
      dom
    (multiple-value-bind (beam-begin beam-continue beam-end)
	(decode-beams (remove-if-not 'beam-element-p dom))
      (make-note :pitch-or-rest (if rest (rest*) (from-lxml pitch))
		 :duration (parse-integer (second duration))
		 :chordp chord
		 :staff (and staff (parse-integer (second staff)))
		 :accidental (and accidental (intern* (string-upcase
						       (second accidental))))
		 :type (and type (intern* (string-upcase (second type))))
		 :notations (rest notations)
		 :tie (and tie (intern* (string-upcase (third (first tie)))))
		 :time-modification (and time-modification
					 (from-lxml time-modification))
		 :beam-begin beam-begin
		 :beam-continue beam-continue
		 :beam-end beam-end))))

(defmethod translate-to-lxml ((note note))
  `(:|note|
     ,@(when (note-chordp note) '(:|chord|))
     ,(translate-to-lxml (note-pitch-or-rest note))
     (:|duration| ,(princ-to-string (note-duration note)))
     ,@(when (note-tie note)
	     `(((:|tie| :|type|
		  ,(string-downcase (symbol-name (note-tie note)))))))
     ,@(when (note-type note)
	     `((:|type| ,(string-downcase (symbol-name (note-type note))))))
     ,@(when (note-accidental note)
	     `((:|accidental|
		 ,(string-downcase (symbol-name (note-accidental note))))))
     ,@(when (note-time-modification note)
	     (list (translate-to-lxml (note-time-modification note))))
     ,@(when (note-staff note)
	     `((:|staff| ,(princ-to-string (note-staff note)))))
     ,@(mapcar (lambda (n) `((:|beam| :|number| ,(princ-to-string n))
			     "begin"))
	       (note-beam-begin note))
     ,@(mapcar (lambda (n) `((:|beam| :|number| ,(princ-to-string n))
			     "continue"))
	       (note-beam-continue note))
     ,@(mapcar (lambda (n) `((:|beam| :|number| ,(princ-to-string n))
			     "end"))
	       (note-beam-end note))
     ,@(when (note-notations note)
	     `((:|notations| ,@(note-notations note))))))

(defmethod make-constructor-form ((note note))
  `(note ,(note-pitch-or-rest note)
	 ,(note-duration note)
	 ',(note-type note)
	 ',(note-accidental note)
	 :chordp ,(note-chordp note)
	 :tie ',(note-tie note)
	 :staff ,(note-staff note)
	 :notations ,(note-notations note)
	 :time-modification ,(note-time-modification note)
	 :beam-begin ,(note-beam-begin note)
	 :beam-continue ,(note-beam-continue note)
	 :beam-end ,(note-beam-end note)))

(defun note (pitch-or-rest duration type accidental
	     &key chordp staff notations tie
	     time-modification
	     beam-begin beam-continue beam-end)
  (make-note :pitch-or-rest pitch-or-rest :duration duration :chordp chordp
	     :staff staff :accidental accidental :type type
	     :notations notations :tie tie
	     :time-modification time-modification
	     :beam-begin beam-begin
	     :beam-continue beam-continue
	     :beam-end beam-end))

(set-pprint-dispatch 'note 'generic-pretty-printer 0 *pprint-xml-table*)

;;; time-modification
(defstruct (time-modification (:include musicxml-object))
  actual-notes normal-notes
  (normal-type nil :type note-type))

(defmethod translate-from-lxml (dom (type (eql ':|time-modification|)))
  (assoc-bind (actual-notes normal-notes normal-type) (cdr dom)
    (make-time-modification
     :actual-notes (parse-integer actual-notes)
     :normal-notes (parse-integer normal-notes)
     :normal-type (intern* (string-upcase normal-type)))))

(defmethod translate-to-lxml ((time-modification time-modification))
  `(:|time-modification|
     (:|actual-notes|
       ,(princ-to-string (time-modification-actual-notes time-modification)))
     (:|normal-notes|
       ,(princ-to-string (time-modification-normal-notes time-modification)))
     (:|normal-type|
       ,(string-downcase (symbol-name (time-modification-normal-type
				       time-modification))))))

(defmethod make-constructor-form ((time-modification time-modification))
  `(time-modification
    ,(time-modification-actual-notes time-modification)
    ,(time-modification-normal-notes time-modification)
    ',(time-modification-normal-type time-modification)))

(defun time-modification (actual-notes normal-notes normal-type)
  (make-time-modification :actual-notes actual-notes
			  :normal-notes normal-notes
			  :normal-type normal-type))

(set-pprint-dispatch 'time-modification 'generic-pretty-printer
		     0 *pprint-xml-table*)
