;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(defpackage #:enp2musicxml
  (:nicknames #:e2m)
  (:use #:cl #:musicxml)
  (:export
   #:enp2musicxml))

(in-package #:e2m)

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

(defun convert-measure (state measure)
  (labels ((previous ()
	     (mapcar-state-previous state))
	   (when-changed (reader)
	     (when (or (null (previous))
		       (not (equal (funcall reader (previous))
				   (funcall reader measure))))
	       (funcall reader measure))))
    `((:|measure| :|number| ,(ts (mapcar-state-index state)))
      ,(attributes :divisions (when-changed #'measure-divisions)
		   :time (when-changed #'measure-time-signature)
		   :clef (when (mapcar-state-firstp state)
			   (list 'g 2)))
      ,@(loop repeat (first (measure-time-signature measure))
	     collect (note (pitch 'c 0 4) 1 'quarter nil))
      ,@(when (mapcar-state-lastp state)
	      '((:|barline| (:|bar-style| "light-heavy")))))))

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

(defun enp-parts (enp) enp)

(defun part-measures (part) (first part))

(defun measure-time-signature (measure)
  (nth (1+ (position :time-signature measure))
       measure))

(defun measure-divisions (measure)
  (declare (ignore measure))
  1)

(defgeneric ts (obj))
(defmethod ts ((obj integer)) (princ-to-string obj))
