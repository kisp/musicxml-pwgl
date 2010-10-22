;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(defpackage #:enp2musicxml
  (:nicknames #:e2m)
  (:use #:cl #:musicxml)
  (:export
   #:enp2musicxml))

(in-package #:e2m)

;;;# enp2musicxml
(defun convert-note2pitch (note)
  (ecase note
    (60 (pitch 'c 0 4))
    (67 (pitch 'g 0 4))))

(defun convert-note2note (abs-dur unit-dur)
  (lambda (state note)
    (note (convert-note2pitch note)
	  (/ abs-dur unit-dur)
	  (abs-dur-name abs-dur)
	  nil
	  :chordp (not (mapcar-state-firstp state)))))

(defun convert-chord (unit-dur)
  (lambda (abs-dur chord)
    (multiple-value-bind (dur notes)
	(destructure-chord chord)
      (if (minusp dur)
	  (list (note (rest*) (/ abs-dur unit-dur) (abs-dur-name abs-dur) nil))
	  (mapcar-state (convert-note2note abs-dur unit-dur)
			notes)))))

(defun convert-measure (state measure)
  (labels ((previous ()
	     (mapcar-state-previous state))
	   (when-changed (reader)
	     (when (or (null (previous))
		       (not (equal (funcall reader (previous))
				   (funcall reader measure))))
	       (funcall reader measure))))
    (let* ((division (measure-quarter-division measure))
	   (unit-dur (/ 1/4 division)))
      `((:|measure| :|number| ,(ts (mapcar-state-index state)))
	,(attributes :divisions (when-changed #'measure-quarter-division)
		     :time (when-changed #'measure-time-signature)
		     :clef (when (mapcar-state-firstp state)
			     (list 'g 2)))
	,@(mapcan (convert-chord unit-dur)
		  (measure-abs-durs measure)
		  (measure-chords measure))
	,@(when (mapcar-state-lastp state)
		'((:|barline| (:|bar-style| "light-heavy"))))))))

(defun convert-part (part)
  `((:|part| :|id| "P1")
    ,@(mapcar-state #'convert-measure (part-measures part))))

(defun enp2musicxml (enp)
  `((:|score-partwise| #+nil :|version| #+nil "2.0")
    (:|identification|
      (:|encoding| (:|encoding-date| "2010-10-12")
	(:|software| "FOMUS v0.2.12")))
    (:|part-list|
      ((:|score-part| :|id| "P1")
       (:|part-name| "Violin")))
    ,@(mapcar #'convert-part (enp-parts enp))))

(defun abs-dur-name (abs-dur)
  (ecase abs-dur
    (1/8 'eighth)
    (1/4 'quarter)
    (1/2 'half)))

;;;# enp access
(defun enp-parts (enp) enp)

(defun part-measures (part) (first part))

(defun measure-time-signature (measure)
  (multiple-value-bind (list plist)
      (split-list-plist measure)
    (declare (ignore list))
    (getf plist :time-signature)))

(defun minimal-quarter-division (abs-dur)
  "Minimal division of a quarter note that is needed to represent
ABS-DUR. If ABS-DUR is greater than a quarter note a suitable division
is returned so that a sequence of quarter notes equivally divided
establish a grid that allows to represent ABS-DUR, which starts on a
grid point. This is always the case, because we never leave the grid."
  (let ((x (/ 1/4 abs-dur)))
    (* x (denominator x))))

(defun measure-quarter-division (measure)
  "Minimal division of a quarter note that is needed to represent all
\(absolute) durations within MEASURE."
  (reduce #'lcm
	  (measure-abs-durs measure)
	  :key #'minimal-quarter-division))

(defun chordp (enp)
  (or (atom enp)               ;only needed for tree abstraction below
      (and (second enp)
	   (atom (second enp)))))
(defun divp (enp) (not (chordp enp)))

(defun div-dur (enp)
  (declare ((satisfies divp) enp))
  (first enp))
(defun div-items (enp)
  (declare ((satisfies divp) enp))
  (second enp))

(defun chord-dur (enp)
  (declare ((satisfies chordp) enp))
  (first enp))

(defun destructure-chord (enp)
  (values (car enp)
	  (getf (cdr enp) :notes)))

(defun measure-abs-durs (measure)
  (labels ((rec (unit tree)
	     (if (chordp tree)
		 (list (* unit (chord-dur tree)))
		 (let ((unit (/ (* unit (abs (div-dur tree)))
				(reduce #'+ (div-items tree) :key #'first))))
		   (mapcan (lambda (tree) (rec unit tree))
			   (div-items tree))))))
    (multiple-value-bind (beats plist)
	(split-list-plist measure)
      (rec 1 (list (apply #'/ (getf plist :time-signature))
		   beats)))))

(defun measure-chords (measure)
  (multiple-value-bind (beats plist)
      (split-list-plist measure)
    (declare (ignore plist))
    (fringe* (make-node nil beats))))

;;;# tree abstraction
(defun make-leaf (obj) (list obj :leaf))
(defun make-node (obj nodes) (list obj nodes))
(defun nodep (tree) (divp tree))
(defun leafp (tree) (chordp tree))
(defun items (tree) (unless (leafp tree) (div-items tree)))
(defun pload (tree)
  (if (leafp tree)
      (car tree)
      (div-dur tree)))

(defun fringe (tree)
  (if (leafp tree)
      (list (pload tree))
      (mapcan #'fringe (items tree))))

(defun fringe* (tree)
  (if (leafp tree)
      (list tree)
      (mapcan #'fringe* (items tree))))

;;;# mapcar-state
(defstruct mapcar-state
  index lastp previous)

(defun mapcar-state-firstp (state)
  (= 1 (mapcar-state-index state)))

(defun mapcar-state (fn list)
  (labels ((rec (fn list index previous)
	     (if (null list)
		 nil
		 (let ((value
			(funcall fn
				 (make-mapcar-state :index index
						    :lastp (null (cdr list))
						    :previous previous)
				 (car list))))
		   (cons value (rec fn (cdr list) (1+ index) (car list)))))))
    (rec fn list 1 nil)))

;;;# utils
(defgeneric ts (obj))
(defmethod ts ((obj integer)) (princ-to-string obj))

(defun plistp (list)
  (labels ((rec (list state)
	     (if (and (null list)
		      (eql state :key))
		 t
		 (ecase state
		   (:key (when (keywordp (car list))
			   (rec (cdr list) :value)))
		   (:value (when (car list)
			     (rec (cdr list) :key)))))))
    (rec list :key)))

(defun append-list-plist (list plist)
  (declare (type list list)
	   (type (satisfies plistp) plist))
  (append list plist))

(defun split-list-plist (list)
  (let ((position (or (position-if #'keywordp list)
		      (length list))))
    (values (subseq list 0 position)
	    (subseq list position))))

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
