(in-package dwn)


;;; DEMO FOR DEFSPECIES

;;; USE:

#|
  (defspecies <name-of-species>
              
              [ :operon-initarg <initarg> ]  ;;; this is the keyword used to specify the number of operons for a (sub)species
                                             ;;; defaults to :num-operons.   number of operons defaults to 8.

              :species-slots 
              ;;; these define characteristics of a subspecies, for example the length or range of a melody ... does not mutate

              { (<name of slot> <default-value>) }*

              :specimen-slots

              ;;; not implemented: slots that mutate but are not part of the repeating operon

              { (<name-of-slot> :range <range> [ :cardinality <card-range> ]) }*

              :operon-slots
              ;;; these are the properties of a single operon (the repeating data structure), which are mutated
              ;;; keywords are :range (a decoder understood by mod-to-range) and :cardinality, for a varying number of values
              ;;; the same range

              { (<name-of-slot> :range <range> [ :cardinality <card-range> ]) }*

              

              :phenotyper
              ;;; code for creating a phenotype from the genotype (not the raw genotype)
              ;;; implied is the first line, (defmethod phenotyper ((self <name-of-species)))
              ;;; good practice is to output the "arrange" data type, in some cases this is assumed.

              <phenotyper-body>
  
  )

|#
              
(defspecies melody 
            
            :species-slots
            (range '(0 127))
            
            :operon-slots
            (pitch :range (range self))
            
            :phenotyper
            (make-even-melody (mapcar 'pitch (operons self))
                              1/16))

;test
;(setf m1 (make-instnace 'melody))
;(run m1 #'(lambda (m) (count-if 'evenp (mapcar 'pitch (operons m)))) 10 :finalizer #'(lambda (s) (mapcar 'pitch (operons s))))

(defmethod finalize ((self melody))
  (arrange->poly (phenotype self)))

(defspecies dx-melody
            
            :species-slots
            (start 48)
            (melodic-range '(-3 3))

            :operon-slots
            (interval :range (melodic-range self))

            :phenotyper
            (make-even-melody (dx->x (start self) 
                                     (mapcar #'interval (operons self)))
                              1/16))





            

            


