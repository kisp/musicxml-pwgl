;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(defpackage #:test-db
  (:nicknames #:tdb)
  (:use #:cl #:sqlite-orm #:alexandria)
  (:export
   #:make-entry
   #:show-foto
   #:nth-enp
   #:show-nth-foto
   #:replace-nth-foto))

(in-package #:tdb)

(defpclass test-case ()
  ((name            :accessor name            :initarg :name)
   (description     :accessor description     :initarg :description)
   (enp             :accessor enp             :initarg :enp)
   (musicxml        :accessor musicxml        :initarg :musicxml)
   (status          :accessor status          :initarg :status)
   (enp-screen-shot :accessor enp-screen-shot :initarg :enp-screen-shot)
   (score :accessor score :initarg :score)))

(defun make-entry (list)
  (destructuring-bind (name description enp musicxml status enp-screen-shot score)
      list
    (make-instance 'test-case
		   :score score
		   :enp-screen-shot enp-screen-shot
		   :status status
		   :musicxml musicxml
		   :enp enp
		   :description description
		   :name name)
    t))

(defun show-foto (test-case)
  (write-byte-vector-into-file
   (enp-screen-shot test-case)
   "/tmp/s.png" :if-exists :supersede)
  (sb-ext:run-program "/usr/bin/gnome-open" (list "/tmp/s.png"))
  nil)

(defun nth-enp (n)
  (enp (nth n (list-by-class 'test-case))))

(defun show-nth-foto (n)
  (show-foto (nth n (list-by-class 'test-case))))

(defun replace-nth-foto (n s)
  (setf (enp-screen-shot (nth n (list-by-class 'test-case)))
	s))
