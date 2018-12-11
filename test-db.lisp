;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

;;; This file is part of MusicXML-PWGL.

;;; Copyright (c) 2010 - 2011, Kilian Sprotte. All rights reserved.

;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.

;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(defpackage #:musicxml-pwgl.test-db
  (:nicknames #:tdb)
  (:use #:cl #:sqlite-orm #:alexandria)
  (:export
   #:make-entry
   #:show-foto
   #:nth-enp
   #:show-nth-foto
   #:replace-nth-foto
   #:list-test-cases
   #:name
   #:description
   #:enp
   #:musicxml
   #:status
   #:enp-screen-shot
   #:score
   #:apropos-test-case
   #:what-next
   #:skipped-tests-that-pass
   #:set-to-skip
   #:set-to-run
   #:make-entry*
   #:env
   #:duplicate))

(in-package #:musicxml-pwgl.test-db)

(defpclass test-case ()
  ((name            :accessor name            :initarg :name)
   (description     :accessor description     :initarg :description)
   (enp             :accessor enp             :initarg :enp)
   (musicxml        :accessor musicxml        :initarg :musicxml)
   (status          :accessor status          :initarg :status)
   (enp-screen-shot :accessor enp-screen-shot :initarg :enp-screen-shot)
   (score :accessor score :initarg :score)
   (env :accessor env :initarg :env :initform nil)))

(defmethod print-object ((obj test-case) stream)
  (handler-case
      (let ((info (format nil "~A ~A ~A" (store-object-id obj) (name obj)
                          (status obj))))
        (print-unreadable-object (obj stream :type t :identity t)
          (write-string info stream)))
    (error () (call-next-method))))

(unless (store-open-p)
  (let ((path (merge-pathnames
               "tests.db"
               (asdf:component-pathname (asdf:find-system :musicxml-pwgl)))))
    (assert (probe-file path))
    (open-store path)))

(defun list-test-cases ()
  (list-by-class 'test-case))

(defun find-all (fn list)
  (remove-if-not fn list))

(defun apropos-test-case (name)
  (find-all (lambda (test-case)
              (search name (name test-case) :test #'char-equal))
            (list-test-cases)))

(defun make-entry (list)
  (destructuring-bind (name description enp musicxml status enp-screen-shot score)
      list
    (make-entry* name description enp musicxml status enp-screen-shot score)))

(defun make-entry* (name description enp musicxml status enp-screen-shot score)
  (make-instance 'test-case
                 :score score
                 :enp-screen-shot enp-screen-shot
                 :status status
                 :musicxml musicxml
                 :enp enp
                 :description description
                 :name name)
  t)

(defun duplicate (test-case)
  (with-accessors ((name name) (description description) (enp enp) (musicxml musicxml)
                   (status status) (enp-screen-shot enp-screen-shot) (score score))
      test-case
    (make-entry* name description enp musicxml status enp-screen-shot score)))

(defun show-foto (test-case)
  (write-byte-vector-into-file
   (enp-screen-shot test-case)
   "/tmp/s.png" :if-exists :supersede)
  (sb-ext:run-program "/usr/bin/gnome-open" (list "/tmp/s.png"))
  nil)

(defun set-to-run (test-case)
  (setf (status test-case) :run))

(defun set-to-skip (test-case)
  (setf (status test-case) :skip))

(defun nth-enp (n)
  (enp (nth n (list-by-class 'test-case))))

(defun show-nth-foto (n)
  (show-foto (nth n (list-by-class 'test-case))))

(defun replace-nth-foto (n s)
  (setf (enp-screen-shot (nth n (list-by-class 'test-case)))
        s))

#+nil
(defun what-next ()
  (labels ((count-lines (string)
             (with-input-from-string (in string)
               (loop for i upfrom 0
                     for line = (read-line in nil)
                     while line
                     finally (return i)))))
    (let (res)
      (dolist (tc (list-test-cases))
        (handler-case
            (when (not (test::check-test-db-test-case tc))
              (push
               (list tc
                     (count-lines
                      (test::diff "/tmp/resc.xml" "/tmp/expc.xml")))
               res))
          (error nil)))
      (dolist (tc (sort res #'< :key #'second))
        (format t "~s~60t~s~%" (first tc) (second tc))))))

#+nil
(defun skipped-tests-that-pass ()
  (dolist (tc (list-test-cases))
    (when (and (eql :skip (status tc))
               (ignore-errors (test::check-test-db-test-case tc)))
      (format t "~s~%" tc))))

;; (tdb::make-entry* "organ staffs" "organ empty bar"
;;                   (read-from-string (alexandria:read-file-into-string "/tmp/enp.lisp"))
;;                   (alexandria:read-file-into-string "/tmp/e.xml")
;;                   :run nil nil)
