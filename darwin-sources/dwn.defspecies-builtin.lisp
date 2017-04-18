(in-package dwn)

(defspecies music-mixin (specimen)
  :operon-name note
  :phenotyper
  ;OM default voice
  (loop for k from 0 to 3
        collect (make-region (/ k 4) 1/4 1 (+ 60 k))))

(defmethod species-concatenator ((self music-mixin)) #'append-arrangements)

(defmethod finalizer ((self music-mixin)) #'arrange->poly)

(defspecies melody (music-mixin)   
  :species-slots (range '(6000 7200))
  :operon-slots (pitch :range (if (= (length (range self)) 2)
                                  `(,@(range self) 100)
                                (range self)))
                                
  :phenotyper (make-even-melody (mapcar 'pitch (operons self))
                     1/16))

(defspecies even-melody (melody)
  :species-slots (note-value 1/16)
  :phenotyper (make-even-melody (mapcar 'pitch (operons self))
                                (note-value self)))

(defspecies grid-melody (even-melody)
  :species-slots (len-range '(1 4)) ;; measured in units
  :operon-slots (units :range (len-range self))
  :phenotyper (loop for op in (operons self)
                    with start = 0
                    for len = (* (units op) (note-value self))
                    collect (make-region start len 1 (pitch op))
                    do (incf start len)))


(defspecies ga-chord (melody)
  :phenotyper
  (loop for note in (notes self)
                     collect (make-region 0 1/4 1 (pitch note))))

(defspecies ga-chord-seq (music-mixin)
  :operon-name chord
  :species-slots (cardinality 3) (range '(6000 7200)) (note-value 1/8)
  :operon-slots (notes :range `(,@(range self) 100) :cardinality (cardinality self))
  :phenotyper (loop for op in (operons self)
                    for start from 0 by (note-value self)
                    append (loop for note in (notes op)
                                 collect (make-region start (note-value self) 1 note))))


(defspecies arrangement (music-mixin)

  :species-slots
  (range '(6000 7200))
  (resolution 1/16)
  (extent 2)        ;in whole notes
  (length-range '(1/4 1))  ;in whole notes
  (channel-range '(1 4))

  :operon-slots
  (start :range (list 0 (extent self) (resolution self)))
  (len :range `(,@(length-range self) ,(resolution self)))
  (channel :range (channel-range self))
  (pitch :range `(,@(range self) 100))

  :phenotyper
  (loop for op in (operons self)
        collect (make-region (start op) (len op) (channel op) (pitch op))))
  

(defspecies dx-melody (music-mixin)      
  :species-slots
  (start 4800)
  (melodic-range '(-300 300))

  :operon-slots (interval :range `(,@(melodic-range self) 100))
  :phenotyper
  (make-even-melody (dx->x (start self) 
                           (mapcar #'interval (operons self)))
                    1/16))


;;; this was in new-arrangers.lisp which was in the users folder of dwn-demo-ws
;;; but OM loads those files before loading user libraries for the WS, so no user-library-dependent code can be in there.
;;; (this should be changed)

(defspecies trills (arrangement)
  :phenotyper   ;;; change only the pheno

  (loop for op in (operons self)
        append (loop for start from (start op) by (resolution self)
                     repeat (floor (len op) (resolution self))
                     for k from 0
                     collect (make-region start (resolution self)
                                          (channel op)
                                          (+ (pitch op) (if (evenp k)
                                                            0 100))))))






            

            


