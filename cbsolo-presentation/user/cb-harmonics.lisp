(in-package dwn)


(defspecies cb-harmonics (music-mixin)
  :operon-slots 
  (pos :range `(:set ,@(mapcar 'first om::*nat-harmonic-guide*)))
  (corde :range '(0 3))

  :phenotyper
  
  (loop for op in (operons self)
        for start from 0 by 1/8
        collect (make-region start 
                             1/8 
                             1 
                             (om* (+ (nth (corde op)
                                          om::*cb-open-strings*)
                                     (second (assoc (pos op)
                                                    om::*nat-harmonic-guide*)))
                                  100))))
                          
  