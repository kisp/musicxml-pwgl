;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(defpackage #:enp2musicxml
  (:nicknames #:e2m)
  (:use #:cl #:musicxml)
  (:export
   #:enp2musicxml))

(in-package #:e2m)

;; (defun mapcar-index (fn list)
;;   "Like MAPCAR, but call FN additionally with an index (counting from
;; 1) as first argument."
;;   (labels ((rec (fn list index)
;; 	     (if (null list)
;; 		 nil
;; 		 (cons (funcall fn index (car list))
;; 		       (rec fn (cdr list) (1+ index))))))
;;     (rec fn list 1)))

(defstruct mapcar-state
  index lastp)

(defun mapcar-state-firstp (state)
  (= 1 (mapcar-state-index state)))

(defun mapcar-state (fn list)
  (labels ((rec (fn list index)
	     (if (null list)
		 nil
		 (cons
		  (funcall fn
			   (make-mapcar-state :index index
					      :lastp (null (cdr list)))
			   (car list))
		  (rec fn (cdr list) (1+ index))))))
    (rec fn list 1)))

(defun convert-measure (state measure)
  `((:|measure| :|number| ,(princ-to-string (mapcar-state-index state)))
    ,@(when (mapcar-state-firstp state)
	    '((:|attributes|
	       (:|divisions| "1")
	       (:|time| (:|beats| "4") (:|beat-type| "4"))
	       (:|clef| (:|sign| "G") (:|line| "2")))))
    ,(note (pitch 'c 0 4) 1 'quarter nil)
    ,(note (pitch 'c 0 4) 1 'quarter nil)
    ,(note (pitch 'c 0 4) 1 'quarter nil)
    ,(note (pitch 'c 0 4) 1 'quarter nil)
    ,@(when (mapcar-state-lastp state)
	    '((:|barline| (:|bar-style| "light-heavy"))))))

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
