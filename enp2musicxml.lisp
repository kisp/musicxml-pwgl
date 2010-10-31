;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(defpackage #:enp2musicxml
  (:nicknames #:e2m)
  (:use #:cl #:musicxml)
  (:export
   #:enp2musicxml))

(in-package #:e2m)

;;;# enp2musicxml
(defun accidental-alter (accidental)
  (ecase accidental
    (sharp 1)
    (flat -1)
    (natural 0)))

(defun decode-midi (note enharmonic)
  (declare (accidental enharmonic))
  (let ((diatonic-pitch (- note (accidental-alter enharmonic))))
    (values (ecase (mod diatonic-pitch 12)
              (0 'c) (2 'd) (4 'e)
              (5 'f) (7 'g) (9 'a) (11 'b))
            (accidental-alter enharmonic)
            (1- (floor diatonic-pitch 12)))))

(defun convert-note2pitch (note)
  (multiple-value-bind (step alter octave)
      (decode-midi
       (note-pitch note)
       (enp-note-accidental note))
    (pitch step alter octave)))

(defun enp-note-accidental (note)
  (labels ((natural-or-sharp (note)
             (if (diatonic-pitch-p note)
                 :natural
                 :sharp))
           (diatonic-pitch-p (midi)
             (and (integerp midi)
                  (member (mod midi 12)
                          '(0 2 4 5 7 9 11)))))
    (let ((enharmonic (and (consp note)
                           (getf (cdr note) :enharmonic))))
      (values
       (ecase (or enharmonic (natural-or-sharp note))
         (:natural 'natural)
         (:sharp 'sharp)
         (:flat 'flat))
       enharmonic))))

(defun convert-expression (expression)
  (ecase expression
    (:accent :|accent|)
    (:staccato :|staccato|)))

(defun convert-note2note (info unit-dur next-chord)
  (declare (type info info) ((or null chord) next-chord))
  (lambda (state note)
    (declare (type mapcar-state state)
             (type note* note))
    (let* ((abs-dur (info-abs-dur info))
           (time-modification (info-cumulative-tuplet-ratio info))
           (chord (info-chord info))
           (beam-continue (1-to-n (apply #'min (info-beaming info))))
           (beam-begin
            (set-difference
             (1-to-n (second (info-beaming info)))
             beam-continue))
           (beam-end
            (set-difference
             (1-to-n (first (info-beaming info)))
             beam-continue)))
      (multiple-value-bind (type dots)
          (abs-dur-name (* time-modification abs-dur))
        (multiple-value-bind (accidental explicit)
            (enp-note-accidental note)
          (note (convert-note2pitch note)
                (/ abs-dur unit-dur)
                type
                dots
                (when (or explicit (not (eql 'natural accidental)))
                  accidental)
                :chordp (not (mapcar-state-firstp state))
                :time-modification
                (unless (= 1 time-modification)
                  (time-modification (numerator time-modification)
                                     (denominator time-modification)
                                     nil))
                :tie-stop
                (and (chord-tied-p chord)
                     (not (note-attack-p note)))
                :tie-start
                (when next-chord
                  (and (chord-tied-p next-chord)
                       (let ((chord-note (find-note-in-chord note next-chord)))
                         (and chord-note
                              (not (note-attack-p chord-note))))))
                :beam-continue beam-continue
                :beam-end beam-end
                :beam-begin beam-begin
                :notations `(,@(when (and (info-beat-end-p info)
                                          (/= 1 (list2ratio (nth (1- (position (info-beat info) (info-pointers info)))
                                                                 (info-tuplet-ratios info)))))
                                     (list (tuplet 'stop 1)))
                               ,@(when (and (info-beat-start-p info))
                                       (let ((tuplet-ratio (nth (1- (position (info-beat info) (info-pointers info)))
                                                                (info-tuplet-ratios info))))
                                         (when (/= 1 (list2ratio tuplet-ratio))
                                           (list (tuplet 'start 1 (first tuplet-ratio) nil nil nil 'yes)))))
                               ,@(when (chord-expressions chord)
                                       `((:|articulations| ,@(mapcar #'convert-expression (chord-expressions chord))))))))))))

(defun convert-rest (info unit-dur)
  (let ((abs-dur (info-abs-dur info))
        (time-modification (info-cumulative-tuplet-ratio info)))
    (multiple-value-bind (type dots)
        (abs-dur-name (* time-modification abs-dur))
      (list (note (rest*) (/ abs-dur unit-dur)
                  type dots nil)))))

(defun convert-chord (unit-dur)
  (lambda (state info)
    (let ((chord (info-chord info))
          (next-chord (when (mapcar-state-next state)
                        (info-chord (mapcar-state-next state)))))
      (if (chord-rest-p chord)
          (convert-rest info unit-dur)
          (mapcar-state (convert-note2note info unit-dur next-chord)
                        (chord-notes chord))))))

(defun convert-measure (clef)
  (lambda (state measure)
    (declare (type mapcar-state state)
             (type measure measure))
    (labels ((previous ()
               (mapcar-state-previous state))
             (when-changed (reader)
               (when (or (null (previous))
                         (not (equal (funcall reader (previous))
                                     (funcall reader measure))))
                 (funcall reader measure))))
      (let* ((division (measure-quarter-division measure))
             (unit-dur (/ 1/4 division))
             (infos (measure-infos measure))
             (next-measure (mapcar-state-next state)))
        `((:|measure| :|number| ,(ts (mapcar-state-index state)))
          ,(attributes :divisions (when-changed #'measure-quarter-division)
                       :time (when-changed #'measure-time-signature)
                       :clef (when (mapcar-state-firstp state) clef))
          ,@(mapcan-state (convert-chord unit-dur)
                          (append infos
                                  (when next-measure
                                    (list (measure-first-info next-measure))))
                          :repeat (length infos))
          ,@(when (mapcar-state-lastp state)
                  '((:|barline| (:|bar-style| "light-heavy")))))))))

(defun convert-part (state part)
  `((:|part| :|id| ,(format nil "P~A" (mapcar-state-index state)))
    ,@(mapcar-state (convert-measure (part-initial-clef part))
                    (part-measures part))))

(defun part2score-part (state part)
  `((:|score-part| :|id| ,(format nil "P~A" (mapcar-state-index state)))
    (:|part-name|
      ,(string-capitalize
        (string (getf (cdr part) :instrument 'violin))))))

(defun enp2musicxml (enp)
  `((:|score-partwise| #+nil :|version| #+nil "2.0")
    (:|identification|
      (:|encoding| (:|encoding-date| "2010-10-12")
        (:|software| "FOMUS v0.2.12")))
    (:|part-list| ,@(mapcar-state #'part2score-part enp))
    ,@(mapcar-state #'convert-part (enp-parts enp))))

(defun abs-dur-name (abs-dur)
  "The musicxml name of ABS-DUR and the number of dots."
  (flet ((lookup (dur)
           (ecase dur
             (1/16 '16th)
             (1/8 'eighth)
             (1/4 'quarter)
             (1/2 'half)
             (1 'whole))))
    (ecase (numerator abs-dur)
      (1 (values (lookup abs-dur) 0))
      (3 (values (lookup (/ abs-dur 3/2)) 1))
      (7 (values (lookup (/ abs-dur 7/4)) 2))
      (15 (values (lookup (/ abs-dur 15/8)) 3)))))

;;;# enp access
(defun enp-parts (enp) enp)

(defun part-initial-clef (part)
  (ecase (getf (cdr part) :staff :treble-staff)
    (:treble-staff (list 'g 2))
    (:alto-staff (list 'c 3))))

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

(defun %chordp (enp)
  (and (second enp)
       (atom (second enp))))

(defun %divp (enp) (not (%chordp enp)))

(defun %notep (enp) (or (member :enharmonic (cdr enp))
                        (member :attack-p (cdr enp))))

(defun %measurep (enp)
  (multiple-value-bind (list plist)
      (split-list-plist enp)
    (declare (ignore list))
    (member :time-signature plist)))

(deftype chord () '(and cons (satisfies %chordp)))
(deftype div () '(and cons (satisfies %divp)))
(deftype note* () '(or (integer 0)
                    (and (cons (integer 0) cons)
                     (satisfies %notep))))
(deftype measure () '(and cons (satisfies %measurep)))

(defun div-dur (enp)
  (declare (div enp))
  ;; rational for time-signatures, which we introduce here when we
  ;; wrap the beats
  (the (rational (0)) (first enp)))

(defun div-items (enp)
  (declare (div enp))
  (second enp))

(defun enp-dur (enp)
  (if (typep enp 'div)
      (div-dur enp)
      (chord-dur enp)))

(defun div-items-sum (enp)
  (the (integer 1)
    (reduce #'+ (div-items enp) :key #'enp-dur)))

(defun tuplet-ratio (enp)
  (declare (div enp))
  (let ((dur (div-dur enp))
        (sum (div-items-sum enp)))
    (list (the (integer 1) sum)
          (the (integer 1)
            (cond ((= sum 1)
                   sum)
                  ((<= sum dur)
                   dur)
                  (t
                   (* dur (expt 2 (truncate (log (/ sum dur) 2))))))))))

(defun chord-dur (enp)
  (declare (chord enp))
  (the (integer 1) (truncate (abs (first enp)))))

(defun chord-rest-p (enp)
  (declare (chord enp))
  (minusp (car enp)))

(defun chord-tied-p (enp)
  (declare (chord enp))
  (floatp (car enp)))

(defun chord-notes (enp)
  (declare (chord enp))
  (getf (cdr enp) :notes))

(defun chord-expressions (enp)
  (declare (chord enp))
  (getf (cdr enp) :expressions))

(defun find-note-in-chord (note chord)
  (declare (type note* note)
           (type chord chord))
  (find-if (lambda (chord-note)
             (= (note-pitch note)
                (note-pitch chord-note)))
           (chord-notes chord)))

(defun note-pitch (note)
  (declare (type note* note))
  (if (atom note) note (car note)))

(defun note-attack-p (note)
  (declare (type note* note))
  (and (consp note)
       (getf (cdr note) :attack-p)))

(defun measure-abs-durs (measure)
  (mapcar #'info-abs-dur (measure-infos measure)))

(defstruct (info (:print-object print-info))
  abs-durs path pointers beaming)

(defun print-info (obj stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "abs-durs ~W~_~
                    notated-durs ~W~_~
                    path ~W~_~
                    chord ~W~_~
                    tuplet-ratios ~W~_~
                    beat: start ~W end ~W"
            (info-abs-durs obj)
            (info-notated-durs obj)
            (info-path obj)
            (info-chord obj)
            (info-tuplet-ratios obj)
            (info-beat-start-p obj)
            (info-beat-end-p obj))))

(defun info-abs-dur (info)
  (car (info-abs-durs info)))

(defun info-notated-dur (info)
  (car (info-notated-durs info)))

(defun info-chord (info)
  (car (info-pointers info)))

(defun info-tuplet-ratios (info)
  (mapcar #'tuplet-ratio (cdr (info-pointers info))))

(defun info-notated-durs (info)
  (mapcar #'*
          (info-abs-durs info)
          (info-cumulative-tuplet-ratios info)))

(defun info-cumulative-tuplet-ratio (info)
  (let ((former-code (reduce #'* (info-tuplet-ratios info)
                             :key #'list2ratio))
        (new-code (car (info-cumulative-tuplet-ratios info))))
    (assert (= former-code new-code))
    new-code))

(defun info-cumulative-tuplet-ratios (info)
  (reduce (lambda (a b) (cons (* a (first b)) b))
          (info-tuplet-ratios info)
          :key #'list2ratio
          :initial-value '(1)
          :from-end t))

(defun info-beat-start-p (info)
  (every #'zerop (butlast (info-path info))))

(defun info-beat-end-p (info)
  (flet ((one-smaller (x y)
           (= 1 (- y x))))
    (every* #'one-smaller
            (butlast (info-path info))
            (mapcar (lambda (div) (length (div-items div)))
                    (rest (butlast (info-pointers info)))))))

(defun info-beat (info)
  (car (last (info-pointers info) 2)))

(defun measure-infos (measure)
  (declare (type measure measure))
  (labels ((rec (unit tree path pointers abs-durs)
             (if (typep tree 'chord)
                 ;; chord
                 (list (make-info :abs-durs (cons (* unit (chord-dur tree))
                                                  abs-durs)
                                  :path path
                                  :pointers pointers))
                 ;; div
                 (let* ((sum (div-items-sum tree))
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
      (let* ((tree (list (list2ratio (getf plist :time-signature)) beats))
             (infos (rec 1 tree nil (list tree) nil)))
        (infos-compute-beaming infos)))))

(defun notated-dur2beam-number (dur)
  "The maximum number of beams that can be used with DUR."
  (multiple-value-bind (name dots)
      (abs-dur-name dur)
    (declare (ignore name))
    (let ((zero-dots-dur (/ dur
                            (ecase dots
                              (0 1)
                              (1 3/2)
                              (2 7/4)
                              (3 15/8)))))
      (max 0
           (- (truncate (log (denominator zero-dots-dur) 2))
              2)))))

(defun infos-compute-beaming (infos)
  (let* ((notated-durs (mapcar #'info-notated-dur infos))
         (beam-numbers (mapcar #'notated-dur2beam-number notated-durs))
         (dur-constraints
          (map-neighbours #'list
                          (append (list 0)
                                  (map-neighbours #'min beam-numbers)
                                  (list 0))))
         (grouping-constraints
          (mapcar (lambda (info)
                    (list (if (info-beat-start-p info)
                              0
                              most-positive-fixnum)
                          (if (info-beat-end-p info)
                              0
                              most-positive-fixnum)))
                  infos))
         (beaming
          (mapcar (lambda (a b)
                    (list (min (first a) (first b))
                          (min (second a) (second b))))
                  dur-constraints
                  grouping-constraints)))
    (loop for info in infos
       for b in beaming
       do (setf (info-beaming info) b))
    infos))

(defun measure-first-info (measure)
  (first (measure-infos measure)))

;;;# mapcar-state
(defstruct mapcar-state
  index lastp previous next)

(defun mapcar-state-firstp (state)
  (= 1 (mapcar-state-index state)))

(defun mapcar-state (fn list &key repeat)
  (labels ((rec (fn list index previous)
             (cond ((null list)
                    nil)
                   ((and repeat (> index repeat))
                    nil)
                   (t
                    (let ((value (funcall
                                  fn
                                  (make-mapcar-state
                                   :index index
                                   :lastp (null (cdr list))
                                   :previous previous
                                   :next (cadr list))
                                  (car list))))
                      (cons value
                            (rec fn (cdr list) (1+ index) (car list))))))))
    (rec fn list 1 nil)))

(defun mapcan-state (fn list &key repeat)
  (apply #'nconc (mapcar-state fn list :repeat repeat)))

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

(defun list2ratio (list)
  (assert (null (cddr list)))
  (/ (first list) (second list)))

(defun map-neighbours (fn list)
  (loop for a in list
     for b in (cdr list)
     collect (funcall fn a b)))

(defun 1-to-n (n)
  (declare (type (integer 0)))
  (unless (zerop n)
    (loop for i from 1 to n collect i)))

(defun every* (fn a b)
  (assert (= (length a)
             (length b)))
  (every fn a b))
