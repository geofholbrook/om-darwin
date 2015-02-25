(in-package dwn)


;;; DEMO FOR DEFSPECIES

;;; USE:


        
(defspecies arrangement (specimen)

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
  (pitch  :range (range self))

  :phenotyper
  (loop for op in (operons self)
        collect (make-region (start op) (len op) (channel op) (pitch op))))
  

(defmethod finalize ((self arrangement))
  (arrange->poly (phenotype self)))


(defspecies melody (arrangement)         
  :species-slots (range '(60 72))
  :operon-slots (pitch :range (range self))           
  :phenotyper (make-even-melody (mapcar 'pitch (operons self))
                     1/16)
)


(defspecies dx-melody (arrangement)      
  :species-slots
  (start 48)
  (melodic-range '(-3 3))

  :operon-slots (interval :range (melodic-range self))
  :phenotyper
  (make-even-melody (dx->x (start self) 
                           (mapcar #'interval (operons self)))
                    1/16))






            

            


