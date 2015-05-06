(in-package dwn)

;DWN.ARRANGE2.LISP

;the arrangement phenotype-format is a list of "regions"

;a region is inteded to be a general-purpose temporal object, much like those
;rectangles in maquette, which can represent a note, or a more complicated structure

;a default property of a region is the CHANNEL. The no-collisions criterion is meant to prevent temporally
;overlapping regions on the same channel.


;*** conventions ***

;pitches should be in keynums, e.g. 60 (= C3) or 64.5 (E3 quarter-sharp)

;times should be encoded in RATIOS (not sure of all code currently embraces this policy)
;i.e. 1/4 = quarternote



;*****************
;****** region structure: (start-beat length channel .. <properties>)
;****** except, it's not a LISP structure ... so that the G.A. doesn't get slowed down? so that printouts are easier to read?
;*****************

(defun region-start (region) (first region))
(defun region-len (region) (second region))
(defun region-end (region) (+ (first region) (second region)))
(defun region-chan (region) (third region))
(defun region-prop (region n) (nth (+ 2 n) region))    ;starts at 1

;but usually:
(defun region-pitch (region) (let ((p (fourth region))) (if (listp p) (car p) p)))
(defun region-vel (region) (fifth region))

(defun make-region (start len &optional (chan 1) &rest properties)
  `(,start ,len ,chan ,@properties))


(defparameter *output-tempo* 60)



;*****************
;*** region generating functions

(defun make-even-melody (props length &key (start 0) (channel 1) (legato t) (lengthen-last-note 0))
  (loop for PL in props
        for k from 1
        for st from start by length
        collect (apply 'make-region 
                       `(,st 
                         ,(+ (if (or legato (= length 1/8) (< length 2/20))
                                 length
                               (if (ratiop length)
                                   (if (= (numerator length) 1)
                                       (/ length 2)
                                     (/ 1 (denominator length)))
                                 (floor (* .25 length))))
                             (if (= k (length props))
                                 lengthen-last-note
                               0))
                         ,channel
                         ,@(list! PL)))))



(defun make-chord (pitches &key (start 0) (len 1/4) (channel 1))
  (loop for p in pitches
        collect (make-region start len channel p)))

;*****************
;*** arrange operations


(defun arr-time-shift (arr delta)
  (if (= delta 0)
      arr
    (loop for region in arr
        collect (cons (+ (car region) delta)
                      (cdr region)))))

(defun arr-transpose (arr delta) 
  (loop for region in arr
        collect (subs-posn (copy-list region) 3 (+ (region-pitch region) delta))))

(defun arr-set-velocities (arr vel)
  (loop for region in arr
        collect `(,@(first-n region 4)
                  ,vel)))

(defun set-channel (region chan)
  `(,(first region)
    ,(second region)
    ,chan
    ,@(nthcdr 3 region)))

(defun arr-set-channel (arr chan)
  (loop for region in arr
        collect (set-channel region chan)))

(defun arr-select (arr start end)
  (arr-time-shift (remove-if-not #'(lambda (region)
                                     (and (or (null start)
                                              (>= (region-start region) start))
                                          (or (null end)
                                              (< (region-end region) end))))
                                 arr)
                  (- (or start 0))))


(defmethod append-arrangements ((arr1 list) (arr2 list) &key (overlap 0) (ceiling t))
  (let* ((extent1 (/ (funcall (if ceiling
                                  #'ceiling
                                #'identity)
                              (* (arr-end arr1) 4)) 4))

         (shifted-arr2 (mapcar #'(lambda (a) `(,(+ (- extent1 overlap) (first a))    
                                          ,@(cdr a)))
                               arr2)))
    (values
     
     (append arr1 ; (cutout-to-fit arr1 shifted-arr2) 
             shifted-arr2)
     (- extent1 overlap))))



;**** measurements

(defmethod arr-start (arr)
  (least-of (mapcar 'region-start arr)))

(defmethod arr-end (arr)
  (greatest-of (mapcar 'region-end arr)))

(defun regions-at-time (arr time)
  (loop for region in arr
        if (and (<= (region-start region) time)
                (> (region-end region) time))
        collect region))




;*********
;*** region cooking.

;;; groups regions into single regions if they start at the same time
(defun group-pitches-by-start (arr)
  (loop for region-list in (demix arr #'region-start t)
        collect `(,@(subseq (car region-list) 0 3)

                  ,(flat (mapcar #'region-pitch region-list))

                  ;take maximum velocity
                  ,(if (some 'fifth region-list)
                       (apply 'max (remove-if 'null (flat (mapcar #'region-vel region-list)))) 
                                        ;flat to account for leftover dynamic velocity values (eg '(50 100 50))
                     100))))



(defun make-ratio-list (regions)
; uses negative numbers to fill in the gaps between regions
; shortens notes on overlaps. ignores channel.
  (loop with time = 0
        with result
        for R on (sort regions #'< :key 'region-start)
        for this = (car R)
        for next = (cadr R)
        do
        
        ;insert rest if necessary (negative ratio)
        (when (> (region-start this) time)
          (push (- time (region-start this)) result))

        ;insert note (positive ratio) shortened if there is an overlap
        (let ((adjusted-length (if (and next
                                        (> (region-end this) (region-start next)))
                                   (- (region-len this) (- (region-end this) (region-start next)))
                                 (region-len this))))
          (unless (= adjusted-length 0)
            (push adjusted-length result))
          (setf time (+ (region-start this) adjusted-length)))

        
        finally return (nreverse result)))



(defparameter *perserve-channel-default* t)


(om::defmethod* arrange->voice ((arr-1 list) &optional (tempo 60) (time-sig '(4 4)))
  :icon 141
  ;ignores channel
  (let ((arr (group-pitches-by-start arr-1)))
    (om::mki 'voice
         :tree (mktree (or (make-ratio-list arr) '(-1)) time-sig)
         :chords (mapcar #'(lambda (region)
                             (om::mki 'chord
                                  :lmidic (list! (region-prop region 1))
                                  :lvel (list (or (when (fifth region)
                                                    (if (atom (fifth region))
                                                        (fifth region)
                                                      (greatest-of (fifth region))))  
                                                  ;;; there may be an envelope on a single note.
                                                  100))
                                  :lchan (list ;(if (> (first (region-prop region 1)) 60) 1 3)
                                               (region-chan region)
                                               )))
                         arr)
         :tempo (or tempo *output-tempo*))))  

 ;;; NB sets channels to 1 for playback, unless preserve-channel is t
(om::defmethod* arrange->poly ((arr list) &optional tempo ( time-sig '(4 4)) (preserve-channel *perserve-channel-default*))
  :icon 141          
  ;demixes by channel
  (prog1
      (om::mki 'poly
               :voices (loop for ch in (demix arr ;(time-shift-to-zero arr) 
                                              #'region-chan t)
                             collect (arrange->voice (if preserve-channel
                                                         ch
                                                       (arr-set-channel ch 1))  ;;; set channel to 1 for playback!
                                                     tempo time-sig)))
    ;(om-beep)
    ))



(om::defmethod* voice->arrange ((self om::voice))
 :icon 141
  (let ((ratios (om::tree2ratio (om::tree self))))
    (loop for length in (remove-if #'(lambda (r) (< r 0)) ratios)
          for onset in (mapcar 'first
                               (remove-if #'(lambda (pair) (< (second pair) 0))
                                          (mat-trans (list (om::butlast (om::dx->x 0 (om::om-abs ratios)))
                                                           ratios))))
          for chord in (om::chords self)
          if (> length 0)
          collect (make-region onset
                               length
                               (car (om::lchan chord))
                               (om::om/ (om::lmidic chord) 100)
                               (car (om::lvel chord))))))


(om::defmethod* poly->arrange ((self om::poly))
 :icon 141
 (mapcan #'voice->arrange
         (om::voices self)))



;(om::make-om-version arrange->voice a->v ((arr-1 list) &optional (tempo 60) (time-sig '(4 4))) 141)
;(om::make-om-version arrange->poly a->p ((arr list) &optional tempo ( time-sig '(4 4)) (preserve-channel *perserve-channel-default*)) 141)
;(om::make-om-version voice->arrange v->a ((self om::voice)) 141)
;(om::make-om-version poly->arrange p->a ((self om::poly)) 141)



(defmethod ordinal-channels ((self poly))
  (loop for k from 1
        for voice in (om::voices self)
        do (om::set-channel voice k))
  self)



;;;; check for collisions
;;;; (also counts exceeding the domain of the arrangement)

;(defmethod count-collisions ((self sp-arrange) &optional extent)
;  (count-collisions (pheno self) (/ (arr-extent self) (arr-resolution self))))

;(defmethod count-collisions ((self om-specimen) &optional extent)
;  (count-collisions (pheno self) (greatest-of (mapcar #'region-end (pheno self)))))

(defmethod count-collisions ((self list) &optional extent)
  (loop with count = 0
        for sub on (sort self #'< :key #'first)
        do
        (let ((end (+ (region-start (car sub))
                      (region-len (car sub)))))
            ;first check terminal end of arrangment
          (when (and extent(> end extent))  
            (incf count (- end extent)))
            
            ;then, collisions with other regions
          (loop with stop
                for other-region in (cdr sub)
                do 
                (if (>= (region-start other-region)
                        end)
                      ;done with this region
                    (setf stop t)
                  (if (= (region-chan (car sub))
                         (region-chan other-region))
                      (incf count (- end
                                     (region-start other-region)))))
                until stop))
        finally return count)) 




