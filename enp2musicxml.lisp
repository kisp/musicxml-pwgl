;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(defpackage #:enp2musicxml
  (:nicknames #:e2m)
  (:use #:cl #:musicxml)
  (:export
   #:enp2musicxml))

(in-package #:e2m)

;;;# enp2musicxml
(defun convert-note2pitch (note)
  (case note
    (60 (pitch 'c 0 4))
    (67 (pitch 'g 0 4))
    (69 (pitch 'a 0 4))
    (71 (pitch 'b 0 4))
    (t (pitch 'g 0 0))))

(defun convert-note2note (abs-dur unit-dur)
  (lambda (state note)
    (note (convert-note2pitch note)
          (/ abs-dur unit-dur)
          (abs-dur-name abs-dur)
          nil
          :chordp (not (mapcar-state-firstp state)))))

(defun convert-chord (unit-dur)
  (lambda (fringe)
    (let ((abs-dur (caar fringe))
          (chord (first (third fringe))))
      (multiple-value-bind (dur notes)
          (destructure-chord chord)
        (if (minusp dur)
            (list (note (rest*) (/ abs-dur unit-dur)
                        (abs-dur-name abs-dur) nil))
            (mapcar-state (convert-note2note abs-dur unit-dur)
                          notes))))))

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
                  (measure-fringe measure))
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
  (mapcar #'caar (measure-fringe measure)))

(defun measure-fringe (measure)
  (labels ((rec (unit tree path pointers abs-durs)
             (if (chordp tree)
                 ;; chord
                 (list (list (cons (* unit (chord-dur tree))
                                   abs-durs)
                             path
                             pointers))
                 ;; div
                 (let* ((sum (reduce #'+ (div-items tree) :key #'first))
                        (unit (/ (* unit (abs (div-dur tree)))
                                 sum)))
                   (mapcan-state
                    (lambda (state tree)
                      (let ((path (cons (1- (mapcar-state-index state))
                                        path))
                            (pointers (cons tree pointers)))
                        (rec unit tree path
                             pointers
                             (cons (* unit sum)
                                   abs-durs))))
                    (div-items tree))))))
    (multiple-value-bind (beats plist)
        (split-list-plist measure)
      (let ((tree (list (list2ratio (getf plist :time-signature)) beats)))
        (rec 1 tree nil (list tree) nil)))))

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

(defun mapcan-state (fn list)
  (apply #'nconc (mapcar-state fn list)))

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

(defun power-of-two-p (x)
  (cond
    ((eql x 1) t)
    ((> x 1) (and (evenp x)
                  (power-of-two-p (truncate x 2))))
    (t (error "power-of-two-p called with ~S" x))))

(defun notable-dur-p (dur &optional (max-dots 3))
  (flet ((h (numer denom)
           (case numer
             ((1) (power-of-two-p denom))
             ((3) (and (> max-dots 0)
                       (>= denom 2) (power-of-two-p denom)))
             ((7) (and (> max-dots 1)
                       (>= denom 4) (power-of-two-p denom)))
             ((15) (and (> max-dots 2)
                        (>= denom 8) (power-of-two-p denom)))
             (t nil))))
    (h (numerator dur)
       (denominator dur))))

(defun tuplet-ratio (dur div-num)
  (let ((numer div-num))
    (labels ((find-lower-match (denom)
               (cond ((notable-dur-p (/ dur denom) 0)
                      (list numer denom))
                     ((>= denom 2)
                      (find-higher-match 3))))
             (find-higher-match (denom)
               (cond ((notable-dur-p (/ dur denom) 0)
                      (list numer denom))
                     (t
                      (find-higher-match (1+ denom))))))
      (assert (> div-num 1))
      (find-lower-match div-num))))

(defun list2ratio (list)
  (assert (null (cddr list)))
  (/ (first list) (second list)))

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
