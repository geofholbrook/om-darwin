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


;**** arrangements can begin with a header, like this:
;** (:header (:this 5 :that tada) <regions>)
;**********

(defun region-start (region) (first region))
(defun region-len (region) (second region))
(defun region-end (region) (+ (first region) (second region)))
(defun region-chan (region) (third region))
(defun region-prop (region n) (nth (+ 2 n) region))    ;starts at 1

;but usually:
(defun region-pitch (region) (fourth region))
(defun region-vel (region) (fifth region))

(defun make-region (start len &optional (chan 1) &rest properties)
  `(,start ,len ,chan ,@properties))


(defparameter *output-tempo* 90)



;*****************
;*** region generating functions

(defun make-arr (starts pitches &key lengths channels time-sig)
  (let ((regions (loop for start on starts
                       for pitch on pitches
                       with chan = channels
                       with len = lengths
                       
                       collect (make-region (car start) 
                                            (or (car len) 
                                                (- (or (cadr start) 1/16) (car start)))
                                            (or (car chan) 1)
                                            (car pitch))
                       
                       do 
                       (when len (setf len (cdr len)))
                       (when chan (setf chan (cdr chan))))))
    (if time-sig
        `(:header (:time-sig ,(if (atom (car time-sig))
                                  (create-list (ceiling (arr-end regions) 
                                                        (/ (car time-sig) (cadr time-sig)))
                                               time-sig)
                                time-sig))
                   ,@regions)
      regions)))


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



(defun has-header-p (arr)
  (equalp (car arr) :header))

(defun arr-header (arr)
  (when (has-header-p arr)
    (first-n arr 2)))

(defun arr-regions (arr)
  (if (has-header-p arr)
      (nthcdr 2 arr)
    arr))





;*****************
;*** arrange operations



(defun map-regions (fun arr &rest lists)
  (append (arr-header arr)
          (loop for reg in (arr-regions arr)
                for args in (or (mat-trans lists)
                                (create-list (length (arr-regions arr)) nil))
                collect (apply fun `(,reg ,@args)))))

(defun arr-process-pitches (arr fun)
  (append (arr-header arr)
          (loop for reg in (arr-regions arr)
                collect `(,@(first-n reg 3)
                          ,(funcall fun (region-pitch reg))))))

(defun arr-time-shift (arr delta)
  (if (= delta 0)
      arr
    (loop for region in (arr-regions arr)
          collect (cons (+ (car region) delta)
                        (cdr region)))))

(defun arr-transpose (arr delta) 
  (append (arr-header arr)
          (loop for region in (arr-regions arr)
                collect (subs-posn (copy-list region) 3 (+ (region-pitch region) delta)))))

(defun arr-set-velocities (arr vel)
  (loop for region in (arr-regions arr)
        collect `(,@(first-n region 4)
                  ,vel)))

(defun set-channel (region chan)
  `(,(first region)
    ,(second region)
    ,chan
    ,@(nthcdr 3 region)))

(defun arr-set-channel (arr chan)
  (append 
   (arr-header arr)
   (loop for region in (arr-regions arr)
         collect (set-channel region chan))))

(defun arr-channel-filter (arr chan)
  (append
   (arr-header arr)
   (remove-if-not #'(lambda (reg) (= (region-chan reg) chan))
                  (arr-regions arr))))

(defun arr-select (arr start end)
  (remove-if-not #'(lambda (region)
                     (and (or (null start)
                              (>= (region-start region) start))
                          (or (null end)
                              (< (region-end region) end))))
                 (arr-regions arr)))


(defmethod append-arrangements ((arr1 list) (arr2 list) &key (overlap 0) (ceiling nil))
  (let* ((extent1 (/ (funcall (if ceiling
                                  #'ceiling
                                #'identity)
                              (* (arr-end (arr-regions arr1)) 4)) 4))

         (shifted-arr2 (mapcar #'(lambda (a) `(,(+ (- extent1 overlap) (first a))    
                                          ,@(cdr a)))
                               (arr-regions arr2))))
    (values
     
     (let ((ts1 (get-arr-property arr1 :time-sig))
           (ts2 (get-arr-property arr2 :time-sig))
           (regions (append (arr-regions arr1) ; (cutout-to-fit arr1 shifted-arr2) 
                                              shifted-arr2)))
       (if ts1
         `(:header (:time-sig ,(append ts1 ts2)) ,@regions)
         regions))

     (- extent1 overlap))))


(defmethod pile-arrangements ((arr1 list) (arr2 list))
  (append (arr-header arr1)
          (sort (append (arr-regions arr1)
                        (arr-regions arr2))
                #'<
                :key #'region-start)))




(defmethod combine-rhythms-and-pitches ((rhythm-arr list) (pitches list))
  (append (arr-header rhythm-arr)
          (loop for region in (arr-regions rhythm-arr)
                for pitch in pitches
                collect (make-region (region-start region)
                                     (region-len region)
                                     (region-chan region)
                                     pitch))))


;**** measurements

(defmethod arr-start (arr)
  (least-of (mapcar 'region-start (arr-regions arr))))

(defmethod arr-end (arr)
  (greatest-of (mapcar 'region-end (arr-regions arr))))

(defun regions-at-time (arr time)
  (loop for region in (arr-regions arr)
        if (and (<= (region-start region) time)
                (> (region-end region) time))
        collect region))




;*********
;*** region cooking.

;;; groups regions into single regions if they start at the same time
(defun group-pitches-by-start (arr)
  (loop for region-list in (demix (arr-regions arr) #'region-start t)
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

(defun get-arr-property (arr prop)
  (when (has-header-p arr)
    (om::find-keyword prop (second arr))))
      
;; TODO
;;(defun set-arr-property (arr prop)
  
  



(om::defmethod* arrange->voice ((arr-1 list) &optional tempo (time-sig '(4 4)))
  :icon 141
  ;ignores channel
  (let ((arr (group-pitches-by-start (arr-regions arr-1))))
    (om::mki 'voice
         :tree (mktree (or (make-ratio-list arr) '(-1)) (or (get-arr-property arr-1 :time-sig)
                                                            time-sig))
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
               :voices (loop for ch-regions in (demix (arr-regions arr) ;(time-shift-to-zero arr) 
                                                      #'region-chan t)
                             for ch = (append (arr-header arr) ch-regions)
                             collect (arrange->voice (if preserve-channel
                                                         ch
                                                       (arr-set-channel ch 1))  ;;; set channel to 1 for playback!
                                                     tempo time-sig)))
    ;(om-beep)
    ))



(om::defmethod* voice->arrange ((self om::voice))
 :icon 141
  (let ((ratios (om::tree2ratio (om::tree self))))
    `(:header (:time-sig ,(mapcar 'first (second (om::tree self))))
      ,@(loop for length in (remove-if #'(lambda (r) (< r 0)) ratios)
              for onset in (mapcar 'first
                                   (remove-if #'(lambda (pair) (< (second pair) 0))
                                              (mat-trans (list (om::butlast (om::dx->x 0 (om::om-abs ratios)))
                                                               ratios))))
              for chord in (om::chords self)
              if (> length 0)
              collect (make-region onset
                                   length
                                   (car (om::lchan chord))
                                   (car (om::lmidic chord))
                                   (car (om::lvel chord)))))))


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
        for sub on (sort (arr-regions self) #'< :key #'first)
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





;***************************************
;***** dealing with verticalities ******
;***************************************


(defun starts-and-ends (arr)
  ;outputs a sorted and demixed list of 3-element terminal indicators of the form ({:start or :end} <time> <region>)
  ;pretty organized way of doing things ... not necessarily efficient.
  (sort (demix (loop for region in (arr-regions arr)
                     collect (list :start (region-start region) region)
                     collect (list :end (region-end region) region))
               #'second)
        #'< :key #'cadar))  ;cadar: first <time> of a list of indicators

;try: (pprint (starts-and-ends '((5 2 1 68) (0 2 1 65) (2 3 1 65) (2 2 2 60) (0 2 2 62))))


(defmethod convert-to-chord-list ((self list) &optional silences domain)
;organize regions of time-block into "time-slices", exactly as necessary to include all vertical simultaneities
;most units of time-block will occur more than once 
 
;**** for now does not record length of slices ****

  (let ((start-ends (starts-and-ends self))
        current-chord
        result)
    (if (and silences
             (> (cadar (first start-ends)) (or (first domain) 0)))
        (push :silence result))
    (loop for point on start-ends
          do (loop for se in (car point)
                   do (case (first se)
                        (:start (push (third se) current-chord))
                        (:end (setf current-chord
                                    (remove (third se) current-chord :count 1 :test 'equalp)))))

          if (or current-chord  ;don't put in a :silence if this is the end of the block
                 (and silences (or (cdr point)
                                   (and domain (< (cadar (car point)) (second domain))))))   
                      
          do (push (or current-chord :silence) result)
          finally return (nreverse result))))

(defmethod convert-to-attack-list ((self list))
  (demix (arr-regions self) #'region-start t))


;;;; simpler, probably faster.

(defun get-vertical-diads (arr)
  (loop with sounding = nil
        for region in (sort (arr-regions arr) #'< :key #'region-start)
        
        do
        (setf sounding (remove-if #'(lambda (r)
                                      (<= (region-end r) (region-start region)))
                                  sounding
                                  ))

        append (loop for other in sounding
                     collect (sort (list region other) #'< :key #'region-chan))
                                      
        do (push region sounding)))



;;;;

(om::defmethod* om::Objfromobjs ((self list) (type om::poly)) 
  (arrange->poly self))
        





