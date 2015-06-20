(in-package dwn)


(defspecies cb-harmonics (music-mixin)
  :operon-slots 
  (nth-harmonic :range `(0 ,(1- (length om::*nat-harmonic-guide*))))
  (corde :range '(0 3))

  :phenotyper
  
  (loop for op in (operons self)
        for start from 0 by 1/16
        collect (make-region start 
                             1/16 
                             1 
                             (om* (+ (nth (corde op)
                                          om::*cb-open-strings*)
                                     (second (nth (nth-harmonic op)
                                                  om::*nat-harmonic-guide*)))
                                  100))))
                          
  