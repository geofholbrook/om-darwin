(in-package dwn)

(defspecies overtones (melody) 
  :species-slots (fundamental 55) (range '(1 16))

  :operon-slots 
  (partial :range (range self))

  :phenotyper
  (make-even-melody (loop for op in (operons self)
                          collect (f->mc (* (fundamental self)
                                            (partial op))))
                    1/16))