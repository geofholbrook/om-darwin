(in-package dwn)

;DWN.ARRANGE.LISP

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
(defun region-pitch (region) (fourth region))
(defun region-vel (region) (fifth region))

(defun make-region (start len chan &rest properties)
  `(,start ,len ,chan ,@properties))



;***********
; arrange (list of regions) operations
;***********

(defun time-shift (arr delta)
  (loop for region in arr
        collect (cons (+ (car region) delta)
                      (cdr region))))

(defun arr-transpose (arr delta) 
  (loop for region in arr
        collect (subs-posn (copy-list region) 3 (+ (region-pitch region) delta))))



 ;; globals?

(defvar *ch->instr* nil)            
(defvar  *instrument-ranges* nil)    







(defun chan->instrument (chan)
  (if *ch->instr*
      (second (assoc chan
                     *ch->instr*))
    chan))

(defun basic-arrange-phenotyper (self)
  (let ((channels (arr-channels self))
        (properties (arr-property-ranges self))
        (region-len-range (arr-len-range self)))
    (loop for geno-region in (group-list (geno self) (+ (length properties) 3) :linear)

          collect (let ((chan (if (and (list channels) (equalp (car channels) 'set)) 
                                  (nth (mod-to-range (region-chan geno-region) `(1 ,(length (cdr channels)))) channels)
                                (mod-to-range (region-chan geno-region) (if (atom channels)
                                                                            `(1, channels)
                                                                          channels)))))

                    `(,(/ (mod-to-range (region-start geno-region) `(0 ,(arr-extent self))) (arr-resolution self))
                
                      ,(/ (mod-to-range (region-len geno-region) region-len-range) (arr-resolution self))
                  
                      ,chan
                  
                      ,@(loop for gene in (nthcdr 3 geno-region)
                              for prop in properties
                              for range in (subst-if (and *instrument-ranges*
                                                          (second (assoc (chan->instrument chan)
                                                                         *instrument-ranges*)))
                                                     #'(lambda (elt) (equalp elt :i-range))
                                                     properties)
                              for k from 0
                              collect (if (or (= k 0) (equalp prop :i-range))
                                          (mod-to-range gene range (if (member chan '(9 10))
                                                                       1
                                                                     (/ (approx self) 2)) 
                                                        t) ;float
                                        (mod-to-range gene range))
                                ))))))


                                                    

(om::defmethod! s-arrange ((regions number) (extent number) (resolution number) (channels t) (properties list) 
                       (region-len-range list) &key (approx 2) class)
  :icon 703 :initvals '(15 12 4 (11 14) ((60 72)) (2 3))
  (om::mki (or class 'sp-arrange)
       :len (* regions (+ (length properties) 3)) 
       :range `(1 ,*arrange-gene-size*)
       :extent extent
       :resolution resolution
       :channels channels
       :properties properties
       :len-range region-len-range
       :approx approx))




;*****************
;*** region generating functions

(defun make-even-melody (props start length &key (channel 1) (legato t))
  (loop for PL in props
        for st from start by length
        collect (apply 'make-region 
                       `(,st 
                         ,(if (or legato (= length 1/8) (< length 2/20))
                              length
                            (if (ratiop length)
                                (if (= (numerator length) 1)
                                    (/ length 2)
                                  (/ 1 (denominator length)))
                              (floor (* .25 length))))
                         ,channel
                         ,@(list! PL)))))



(defun cutout-to-fit (cuttee superseder &key (allow-before t) allow-after)
  ;;; (both are arrangements ... remove any region from cuttee that would overlap with superseder on the same channel)
  (let ((channel-starts (when allow-before
                          (loop for chan from chMin to chMax
                                collect (least-of (mapcar 'region-start
                                                          (remove-if-not #'(lambda (region) (= (region-chan region) chan))
                                                                         superseder))))))
        (channel-ends (when allow-after
                        (loop for chan from chMin to chMax
                                collect (greatest-of (mapcar 'region-end
                                                             (remove-if-not #'(lambda (region) (= (region-chan region) chan))
                                                                            superseder)))))))
    
  ;note : a region allowed-before will shorten the length of a note to make it fit, but
  ;a region allowed-after will not have it's start time moved up to make it fit!!!
     
  ; could use a rewrite. confusing.
            
    (loop for region in cuttee
          for all-clear = (and (null (nth (region-chan region)
                                          channel-starts))
                               (null (nth (region-chan region)
                                          channel-ends)))

          for starts-before = (and (not all-clear)
                                   allow-before
                                   (< (region-start region)
                                      (nth (region-chan region)
                                           channel-starts)))

          if (or all-clear
                 starts-before
                 (and allow-after
                      (> (region-start region)
                         (nth (region-chan region)
                              channel-ends))))   

          collect (if (and (not all-clear)
                           starts-before
                           (> (region-end region)
                              (nth (region-chan region)
                                   channel-starts)))
                      (om::alter-nth region 1 (- (nth (region-chan region)
                                                  channel-starts)
                                             (region-start region)))
                    region))))
                      
                   


(defmethod append-arrangements ((arr1 list) (arr2 list) &key (overlap 0))
  (let* ((extent1 (/ (ceiling (* (greatest-of (mapcar 'region-end arr1)) 4)) 4))
         (shifted-arr2 (mapcar #'(lambda (a) `(,(+ (- extent1 overlap) (first a))    
                                          ,@(cdr a)))
                               arr2)))
    (values
     
     (append arr1 ; (cutout-to-fit arr1 shifted-arr2) 
             shifted-arr2)
     (- extent1 overlap))))

(defmethod append-arrangements ((arr1 sp-arrange-pheno-mixin) (arr2 sp-arrange-pheno-mixin) &key (overlap 0))
  (append-arrangements (pheno arr1) (pheno arr2) :overlap overlap))

(defmethod append-arrangements ((arr1 list) (arr2 sp-arrange-pheno-mixin) &key (overlap 0))
  (append-arrangements arr1 (pheno arr2) :overlap overlap))

(defmethod append-arrangements ((arr1 sp-arrange-pheno-mixin) (arr2 list) &key (overlap 0))
  (append-arrangements (pheno arr1) arr2 :overlap overlap))

(defun append-arr (&rest arr-list)
  (reduce #'append-arrangements arr-list))



;***************
;*** conversion

;; both arrange->poly and arrange->cseq assume that properties are in the order (pitch velocity)

(om::defmethod* arrange->cseq ((arr list) &optional (tempo 120))
  :icon 141
  ;;; setting the "inside" slot of a single chord-seq might be more effecient, hmmmmm?
  (reduce #'merger
          (mapcar #'(lambda (region)
                      (om::mki 'chord-seq
                           :lmidic (list (om* (or (fourth region) 60) 100))
                           :lvel (list (or (when (fifth region)
                                             (if (atom (fifth region))
                                                 (fifth region)
                                               (greatest-of (fifth region))))  
                                                       ;;; there may be an envelope on a single note.
                                           100))
                           :lonset (list (om* (first region) (/ 60000 tempo)))
                           :ldur (list (om* (second region) (/ 60000 tempo)))
                           :lchan (list (third region))))
                  (om::sort. (phenotype arr)
                         '< 'first))))

(om::defmethod* arrange->cseq ((arr sp-arrange-pheno-mixin) &optional (tempo 120))
;;; setting the "inside" slot of a single chord-seq might be more effecient, hmmmmm?
  (arrange->cseq (pheno arr) tempo))



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
            


(defun group-pitches-by-start (arr)
  (loop for region-list in (demix arr #'region-start t)
        collect `(,@(subseq (car region-list) 0 3)
                  ,(flat (mapcar #'region-pitch region-list))
                  ,(if (some 'fifth region-list)
                       (greatest-of (remove-if 'null (flat (mapcar #'region-vel region-list)))) 
                                        ;flat to account for leftover dynamic velocity values (eg '(50 100 50))
                     100))))
        
       
(defun time-shift-to-zero (arr)
  (time-shift arr (- (loop for region in arr minimize (region-start region)))))
        

(defvar *arr-channel-mapping* nil)

(defun map-channel (chan)
  (if *arr-channel-mapping*
      (second (assoc chan *arr-channel-mapping*))
    chan))

(om::defmethod* arrange->voice ((arr-1 list) &optional (tempo 120) (time-sig '(4 4)))
  :icon 141
  ;ignores channel
  (let ((arr (group-pitches-by-start arr-1)))
    (om::mki 'voice
         :tree (mktree (make-ratio-list arr) time-sig)
         :chords (mapcar #'(lambda (region)
                             (om::mki 'chord
                                  :lmidic (list! (om* (region-prop region 1) 100))
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

(om::defmethod* arrange->voice ((arr sp-arrange-pheno-mixin) &optional (tempo 120) (time-sig '(4 4)))
  :icon 141
  (arrange->voice (pheno arr) tempo))



(om::defmethod* arrange->poly ((arr list) &optional tempo ( time-sig '(4 4)))
  :icon 141          
  ;demixes by channel
  (om::mki 'poly
       :voices (loop for ch in (demix arr ;(time-shift-to-zero arr) 
                                      #'region-chan t)
                     collect (arrange->voice ch tempo time-sig))))

(om::defmethod* arrange->poly ((arr sp-arrange-pheno-mixin) &optional tempo ( time-sig '(4 4)))
  :icon 141
  (arrange->poly (pheno arr) tempo))




(om::defmethod* arrange->xylo ((arr list) &optional tempo)
  :icon 141          

;;; just make every region channel 1
;;; looks messy but plays back fine.
  (om::mki 'poly
       :voices (loop for ch in (list (mapcar #'(lambda (region)
                                           `(,(region-start region)
                                             ,(region-len region)
                                             1
                                             ,@(nthcdr 3 region)))
                                       arr))
                     collect (arrange->voice ch tempo))))


(om::defmethod* arrange->xylo ((arr sp-arrange-pheno-mixin) &optional tempo)
  :icon 141
  (arrange->xylo (pheno arr) tempo))





;;;;; ==========

(defun swap-modes (arr original-mode new-mode &key domain range)
  (loop for region in arr
        collect (if (and (or (not domain)
                             (withinp (region-start region) domain))

                         (or (not range)
                             (withinp (region-pitch region) range))

                         (member (mod (region-pitch region) 12)
                                 original-mode))

                    (subs-posn region
                               3
                               (om::closest-within-pc-field (region-pitch region) 
                                                        (list (nth (position (mod (region-pitch region) 12)
                                                                             original-mode)
                                                                   new-mode))))
                  region)))
  





;;;; check for collisions
;;;; (also counts exceeding the domain of the arrangement)

(defmethod count-collisions ((self sp-arrange) &optional extent)
  (count-collisions (pheno self) (/ (arr-extent self) (arr-resolution self))))

(defmethod count-collisions ((self om-specimen) &optional extent)
  (count-collisions (pheno self) (greatest-of (mapcar #'region-end (pheno self)))))

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
                       
(om::defmethod! no-collisions ()
  :icon 702
  (c-spec #'count-collisions 0))



;;;;; prevent timing changes!

(defmethod freeze-arrangement ((self sp-arrange))
  (let ((new (true-copy self)))
    (setf (freeze new) (list (+ (length (fourth (pheno-args self)))  ;number of properties
                                3)
                             '(0 1 2)))
    new))

(defmethod unfreeze-arrangement ((self sp-arrange))
  (let ((new (true-copy self)))
    (setf (freeze new) nil)
    new))


;TIME-BLOCKS 
;to do with using c-dynamic


#|
;;; commenting this out, seems to be redundant with new version of get-time-blocks in dwn.om.lisp

(defmethod get-extent ((self sp-arrange-pheno-mixin)) 
  (greatest-of (mapcar #'region-end (pheno self))))

(defmethod get-extent ((self sp-arrange))
  (arr-extent self))

(defmethod get-time-blocks ((self sp-arrange-pheno-mixin) resolution &optional overlap mode)
; pheno is a list of regions 
  (let ((extent (get-extent self)))
    (let ((block-width (/ extent resolution)))
      (flet ((clip-to-block (region index)
               (let ((start (max (region-start region) (* index block-width)))
                     (end (min (region-end region) (* (1+ index) block-width))))
                 `(,start ,(- end start) ,@(cddr region)))))

        (let ((result (create-list resolution nil)))    ;list of nils
          (loop for region in (pheno self)
                do (loop for index 
                         from (min (1- resolution)
                                   (floor (region-start region)     
                                          block-width)) 
                         to (min (1- resolution)
                                 (floor (- (region-end region) .00001)   
              ; -.00001 so that a region doesn't get included in a block that starts where the unit ends. (stupid band-aid)
                                        block-width))
                         do (unless (> index (1- resolution))
                              (push (clip-to-block region index) (nth index result))))
                finally return result))))))
 
|#

;***************************************
;***** dealing with verticalities ******
;***************************************


(defun starts-and-ends (time-block)
  ;outputs a sorted and demixed list of 3-element terminal indicators of the form ({:start or :end} <time> <region>)
  ;pretty organized way of doing things ... not necessarily efficient.
  (sort (demix (loop for region in time-block
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
;+
(defmethod convert-to-chord-list ((self sp-arrange) &optional silences domain)
  (convert-to-chord-list (pheno self) silences (or domain `(0 ,(arr-extent self)))))



(defmethod convert-to-attack-list ((self list))
  (demix self #'region-start t))
;+
(defmethod convert-to-attack-list ((self om-specimen))
  (convert-to-attack-list (pheno self)))



;;; look out, these might be screwed up, not alt-pheno anymore.

(om::defmethod! c-simult ((self criterion) &optional pitches-only silences)
  :icon 702
  (om::mki 'simple-criterion
       :eval-fun (if pitches-only
                     #'(lambda (pheno &optional index) 
                         (evaluate (mapcar #'(lambda (chord)
                                               (if (atom chord)
                                                   chord
                                                 (mapcar #'(lambda (region)
                                                             (fourth region))
                                                         chord)))
                                           (convert-to-chord-list pheno silences)) 
                                   self index))

                   #'(lambda (pheno &optional index)
                       (evaluate (convert-to-chord-list pheno silences) 
                                 self index)))))


(om::defmethod! c-attacks ((self criterion) &optional pitches-only)
  :icon 702
  (om::mki 'simple-criterion
       :eval-fun (if pitches-only
                     #'(lambda (pheno &optional index) 
                         (evaluate (mapcar #'(lambda (chord)
                                               (mapcar #'(lambda (region)
                                                           (fourth region))
                                                       chord))
                                           (convert-to-attack-list pheno)) self index))
                   #'(lambda (pheno &optional index)
                       (evaluate (convert-to-attack-list pheno) self index)))))


;compat
(om::defmethod! a-chords ((self criterion) &optional pitches-only)
  :icon 702
  (c-simult self pitches-only))


#|

(om::defmethod! c-attacks ((self criterion) &optional pitches-only)
  :icon 702
  (c-alt-pheno self (if pitches-only
                        #'(lambda (spec) 
                            (mapcar #'(lambda (chord)
                                               (mapcar #'(lambda (region)
                                                           (fourth region))
                                                       chord))
                                           (convert-to-attack-list spec)))
                      #'(lambda (spec)
                          (convert-to-attack-list spec)))))



(om::defmethod! c-simult ((self criterion) &optional pitches-only)
  :icon 702
  (c-alt-pheno self (if pitches-only
                        #'(lambda (spec) 
                            (mapcar #'(lambda (chord)
                                               (mapcar #'(lambda (region)
                                                           (fourth region))
                                                       chord))
                                           (convert-to-chord-list spec)))
                      #'(lambda (spec)
                          (convert-to-chord-list spec t)))))


|#

          
                   
    

;******************
;******************


;******************
;******************

(om::defmethod! c-channel ((crit criterion))
  :icon 702
  (c-post-pheno (c-element crit 0 nil)  ;;; nil means don't provide the index
                #'(lambda (pheno)
                    (demix pheno #'region-chan t))))

(defun separate-into-phrases (arr thresh)
  (let (result
        temp
        previous-start)
    (loop for region in arr
          do 
          (if (and previous-start
                   (> (- (region-start region)
                         previous-start)
                      thresh))
              (progn
                (push (nreverse temp) result)
                (setf temp (list region)))
            (push region temp))
          (setf previous-start (region-start region))
          finally
          do
          (push (nreverse temp) result))
    (nreverse result)))





(om::defmethod! c-all-in-range ()
  :icon 702
  (c-attacks (c-note #'(lambda (region &optional index)
                         (region-in-range region)))))    ;;; this is defined in wetink.lisp ... defines instruments and their ranges

(defun denominators (arr)
  (mapcar #'(lambda (region)
              (list (denominator (region-start region))
                    (denominator (region-end region))
                    (denominator (region-len region))))
          arr))





;some modifying criteria meant for arrange-pheno specimen types

(om::defmethod! c-chan-filter ((crit criterion) chans-1)
  :icon 702
  (let ((chans (list! chans-1)))
    (c-post-pheno crit #'(lambda (pheno)
                           (remove-if-not #'(lambda (region)
                                              (memberp (region-chan region) chans))
                                          pheno))))) 


(om::defmethod! c-prop ((prop number) (value t) &optional (goal 1))
  (c-post-pheno (c-pitch value :unsigned goal)
                #'(lambda (pheno)
                    (loop for region in pheno
                          collect (list (region-prop region prop))))))
                    

;*******************************************
;***** sp-multi : combining specimens ******
;*******************************************


(om::defclas sp-multi (om-specimen sp-arrange-pheno-mixin)
  ((specimens :initform nil)))

(om::defmethod! s-multi ((specimens list))
  :icon 703
  (om::mki 'sp-multi :specimens specimens))

(defmethod phenotype ((self sp-multi))
  ;;; new may 1st 2011 : sorts by region start!!! necessary for some functions, ex. c-dynamic
  (sort (loop for sp in (specimens self)
              append (phenotype sp))
        #'<
        :key 'region-start))

(defmethod geno ((self sp-multi))
  (loop for sp in (specimens self)
        append (geno sp)))

(defmethod set-geno ((self sp-multi) geno)
  (let ((new (true-copy self))
        (geno-1 (true-copy geno)))
    (setf (specimens new)
          (loop for sp in (specimens new)
                collect
                (set-geno sp (loop repeat (length (geno sp))
                                   collect (pop geno-1)))))
    (clear-pheno new)
  new))

(defmethod rnd-genotype ((self sp-multi))
  (loop for sp in (specimens self)
        append (rnd-genotype sp)))

(defmethod count-collisions ((self sp-multi) &optional extent)
  (count-collisions (pheno self)))   ;;; extent is not applicable

(defmethod mutate ((self sp-multi) &rest args)
  (loop repeat (rrnd 1 3)
        for index = (rrnd 0 (1- (length (specimens self))))
        do 
        (mutate (nth index (specimens self)))
        (push (om::mki 'mutation
                   :location index
                   :alteration (last-mutation (nth index (specimens self))))
              (last-mutation self))))

(defmethod c-specimens ((crit criterion) &key select)
  (om::mki 'spec-criterion
       :eval-fun #'(lambda (multi &optional index)
                     (om::average (mapcar #'(lambda (sp)
                                          (evaluate sp crit))
                                      (if select
                                          (loop for s in select collect (nth s (specimens multi)))
                                        (specimens multi)))
                              nil))
       :goal 0))

;************************************************************
;***** sp-harmonizer : parallel harmonizing by channel ******
;************************************************************


;uses *fixed-sequence* ... so, more than one of these cannot be done simultaneously!

(om::defclas sp-harmonizer (sp-chords sp-arrange-pheno-mixin)
  ((harm-channels :initform nil)
   (ranges :initform nil)))



(om::defmethod! s-harmonizer ((fixed list) (ranges list) (harm-channels list) (approx number))  
  ; fixed is meant to be a monophonic dwn-arrange
  ; ranges are in semitones, relative to original note(s)
  :icon 702 :initvals `(((0 1/4 1 60)) ((1 11)) (2) 2)          
  (set-fixed-sequence fixed)
  (om::mki 'sp-harmonizer 
       :len (* (length fixed) (length ranges))
       :range `(0 ,(round (* (greatest-of (mapcar #'(lambda (r) (- (second r) (first r))) ranges)) 
                      approx 
                      .5)))
       :cardinality (length ranges)
       :approx approx
       :harm-channels harm-channels
       :ranges ranges))

(om::defmethod! s-harmonizer ((fixed sp-arrange-pheno-mixin) (ranges list) (harm-channels list) (approx number))
  (s-harmonizer (pheno fixed) ranges harm-channels approx))

(defmethod phenotype ((self sp-harmonizer))
  (let ((delta-chords (loop for gene-group in (group-list (geno self) (cardinality self) :linear)
                            collect (loop for gene in gene-group
                                          for range in (ranges self)
                                          collect (mod-to-range gene range (/ (approx self) 2) t)))))
    (append *fixed-sequence*
            (loop for region in *fixed-sequence*
                  for delta-chord in delta-chords
                  append (loop for delta in delta-chord
                               for chan in (harm-channels self)
                               collect `(,@(first-n region 2)
                                         ,chan
                                         ,(+ (region-pitch region) delta)
                                         ,@(nthcdr 4 region)))))))







;*******************************************
;*******************************************


; sorting the phenotype
(defun arr-sort (arr &optional demix)
  (let ((sorted (sort arr #'(lambda (r1 r2)
                              (if (= (region-start r1) (region-start r2))
                                  (< (region-chan r1) (region-chan r2))
                                (< (region-start r1) (region-start r2)))))))
    (if demix (demix sorted #'third)
      sorted)))




(defun set-all-velocities (arr vel)
  (loop for region in arr
        collect (om::alter-nth region 4 vel)))



;converting a simple list of lists of pitches into an arrangement
(defun chords->arr (chord-list &optional (duration 1/4) (channels '(1)) (velocity 100))
  (loop for chord in chord-list
        for time from 0 by duration
        append (loop for note in chord
                     for index from 0
                     collect (make-region time
                                          duration
                                          (nth (om::om-clip index 0 (1- (length channels)))
                                               channels)
                                          note
                                          velocity))))



;;; score-making utilities (making polys that are usuable as score, without tedious changes in finale)



;remove-trailing-tie : suitable for PIZZICATO

(defun remove-trailing-tie (region)   ;
  (if (and (> (floor (region-end region) 1/4)
              (floor (region-start region) 1/4))   ;;; spans more than one beat
           (not (= (mod (region-end region) 1/4) 0)))
      (om::alter-nth region 1  ;region length
                 (- (region-len region)
                    (mod (region-end region) 1/4)))
    region))  ;;; does not end on a beat
;+
(defun arr-remove-trailing-ties (arr &key channels)
  (loop for region in arr
        if (or (null channels)
               (member (region-chan region) channels))
        collect (remove-trailing-tie region)
        else
        collect region))


#|
(defmethod substitute-beats ((tree list) substitutions &optional channels)
  (list (first tree)
        (loop for measure in (second tree)
              collect (progn
                        (loop for subst in substitutions
                              do (nsubstitute (second subst) (first subst) (second measure)))
                        measure))))
                 

  
(defmethod substitute-beats ((self poly) substitutions &optional channels)
  (om::mki 'poly 
       :voices (loop for voice in (voices poly)
                     if (or (null channels)
                            (member (first (flat (lchan (first (chords voice)))))
                                    channels))
                     collect (substitute-beats   

|#                                 
                             


(defun double-instrument (arr orig-chan doubling-chan)
   (append arr
           (loop for region in arr
                 if (= (region-chan region) orig-chan)
                 collect (om::alter-nth region 2 doubling-chan))))


(defun legatofy (arr &key channels)
  (loop for sub on (sort arr '< :key 'region-start)
        for next-diff = (find-if #'(lambda (next-region)
                                     (and (= (region-chan next-region)
                                             (region-chan (car sub)))
                                          (> (region-start next-region) (region-start (car sub)))))
                                 (cdr sub))

        if (and next-diff (or (null channels)
                              (memberp (region-chan (car sub)) channels)))
        
        collect
        (om::alter-nth (car sub) 1 (- (region-start next-diff) (region-start (car sub))))
       
        else
        collect (car sub)))



;this depends on the max/msp ability to convert noteons below pitch 21 (lowest note of the piano) into program changes
;if you want program changes above 20, you are out of luck for now!

(defparameter *marcato-length-threshold* 1/12)
(defmethod add-program-changes ((arr list) &optional (long-pgm 0) (short-pgm 12) (channels '(11 12 13 14)))
  (append arr
          (loop for region in arr
                if (member (region-chan region) channels)
                collect (om::alter-nth region 3 (+ (if (> (region-len region) *marcato-length-threshold*)
                                                   long-pgm  ;    default: normal arco
                                                 short-pgm  ;   default:  marcato
                                                 )
                                               (if (= (floor (region-pitch region))
                                                      (region-pitch region))
                                                   0
                                                 0.5))))))   ;max patch is such that the program noteon has to be on the same port
                                                             ;as the note that it accompanies ..so I have to give it a .5 if the pitch has a .5

(defmethod add-program-changes ((arr om-specimen) &optional (long-pgm 0) (short-pgm 12) (channels '(11 12 13 14)))
  (add-program-changes (pheno arr) long-pgm short-pgm channels))



(defmethod initial-program-changes ((arr list) programs channels)
  (append arr (loop for pgm in programs
                    for chn in channels
                    collect (let ((note? (find-if #'(lambda (r)
                                                      (and (= (region-chan r) chn)
                                                           (= (region-start r) 0)))
                                                  arr)))
                              (if note?
                                  (om::alter-nth note? 3 (+ pgm
                                                        (if (= (floor (region-pitch note?))
                                                               (region-pitch note?))
                                                            0
                                                          0.5)))
                                `(0 1/4 ,chn ,pgm))))))

(defmethod initial-program-changes ((arr om-specimen) programs channels)
  (initial-program-changes (pheno arr) programs channels))



(defmethod remove-program-changes (arr)
  (remove-if #'(lambda (region) (< (region-pitch region) 21))
             arr))


;;;;;;; converting from MIDI to arrange


(defmethod midi->arrange ((self list) &optional (quarter-duration 1000))  
  ;mf-info format : (midi-number (pitch) , onset-time(ms), duration(ms), velocity, channel)
  :icon 148
  (loop for midinote in (flat self 1)  ;; since mf-info is demixed into channels
        collect `(,(/ (second midinote) quarter-duration 4)
                  ,(/ (third midinote) quarter-duration 4)
                  ,(fifth midinote)
                  ,(first midinote)
                  ,(fourth midinote))))

(defmethod midi->arrange ((self midifile) &optional (tempo 60))
  :icon 148
  (midi->arrange (mf-info self)))

           

(defun translate-finale-qt (arr channels)
  (loop for region in arr
        if (memberp (region-chan region) channels)
        collect (om::alter-nth region 3 (+ (/ (- (region-pitch region) 60) 2) 60))
        else
        collect region))





;;;;;;;;; splitting out a channel

(defun get-one-channel (arr chan)
  (remove-if-not #'(lambda (region)
                     (= (region-chan region) chan))
                 arr))



;;;;;;;;; measuring arrangement density

;;; added may 2 2011 (election day!)
(defun arr-density (arr &optional num-channels)
 ;;; you can lie and say num-channels = 1, even if there are more channels
 ;;; causing this function to return "notes per beat" regardless of the number of channels
 ;;; if you don't give num-channels at all, this function finds out how many different channels are in arr,
 ;;; which is quite slow here because it isn't optimized! gh.
  (let ((beat-span (- (ceiling (* (region-start (last-elem arr)) 4))
                      (floor (* (region-start (first arr)) 4)))))
    (/ (length arr)
       beat-span
       (or num-channels
           (length (remove-duplicates arr :key 'region-chan))))))





;;;; this should exist already!

(defun ratios->arrange (ratios &optional (bogus-pitch 60) (bogus-chan 1))
  (loop with time = 0
        for ratio in ratios
        if (> ratio 0) 
        collect (make-region time ratio bogus-chan bogus-pitch)
        do
        (incf time (abs ratio))))



;;;; from acanthes-orch code (should be in om-geof, probably)

(defun closest-within-pc-field (note field)
  ;;;; really stupid method! easy to program though ...
  (loop for k in '(0 1 -1 2 -2 3 -3 4 -4 5 -5 6)
        if (member (mod (+ note k) 12) field)
        return (+ note k)))


                                        



;;; UNUSED, probably

#|

(om::defclas sp-arrange-pheno-mixin () ())   ;;; because some classes other than sp-arrange might have a arrange-format phenotype



;;; so you can hook up an evolute (or specimen factory, or optimize-specimen) to a score object directly

(defmethod objfromobjs ((self sp-arrange-pheno-mixin) (type poly))
  (arrange->poly (pheno self) *output-tempo*))
;+
(defmethod objfromobjs ((self sp-arrange-pheno-mixin) (type voice))
  (arrange->voice (pheno self) *output-tempo*))
;+
(defmethod objfromobjs ((self sp-arrange-pheno-mixin) (type chord-seq))
  (arrange->cseq (pheno self) *output-tempo*))




;***********
;;;**** OM objects
;***********

(om::defclass! sp-arrange (om-specimen sp-arrange-pheno-mixin)    
  ((arr-extent :initform 10           :initarg :extent            :accessor arr-extent)
   (arr-resolution :initform 1        :initarg :resolution        :accessor arr-resolution)
   (arr-channels :initform '(2 14)    :initarg :channels          :accessor arr-channels)
   (arr-property-ranges :initform nil :initarg :properties        :accessor arr-property-ranges)
   (arr-len-range :initform '(1 4)    :initarg :len-range         :accessor arr-len-range))
  (:default-initargs
    :len 30
    :range `(1 ,*arrange-gene-size*))
  (:icon 703))

(defmethod phenotype ((self sp-arrange))
  (basic-arrange-phenotyper self))    ;the function is left here to be available to inheritors of sp-arrange that might specialize phenotype


|#
