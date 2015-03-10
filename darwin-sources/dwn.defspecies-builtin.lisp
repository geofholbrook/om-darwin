(in-package dwn)

(defspecies music-mixin (specimen)
  :phenotyper
  (loop for k from 0 to 3
        collect (make-region (/ k 4) 1/4 1 (+ 60 k))))

(defmethod species-concatenator ((self music-mixin)) #'append-arrangements)

(defmethod finalizer ((self music-mixin)) #'arrange->poly)

(defspecies melody (music-mixin)         
  :species-slots (range '(60 72))
  :operon-slots (pitch :range (range self))           
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


(defspecies arrangement (music-mixin)

  :species-slots
  (range '(60 72))
  (resolution 1/16)
  (extent 2)        ;in whole notes
  (length-range '(1 16))  ;in whole notes
  (channel-range '(1 4))

  :operon-slots
  (start :range (list 0 (extent self) (resolution self)))
  (len :range `(,@(length-range self) ,(resolution self)))
  (channel :range (channel-range self))
  (pitch :range (range self))

  :phenotyper
  (loop for op in (operons self)
        collect (make-region (start op) (len op) (channel op) (pitch op))))
  






(defspecies dx-melody (music-mixin)      
  :species-slots
  (start 48)
  (melodic-range '(-3 3))

  :operon-slots (interval :range (melodic-range self))
  :phenotyper
  (make-even-melody (dx->x (start self) 
                           (mapcar #'interval (operons self)))
                    1/16))






            

            


