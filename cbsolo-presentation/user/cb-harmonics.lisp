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




(defmethod prep-for-output ((spec cb-harmonics))
  
  (let ((pheno (phenotype spec)))
    (let ((voice (arrange->voice 
                  (append (arr-header pheno)
                          (loop for op in (operons spec)
                                for reg in (arr-regions pheno)

                                collect (make-region (region-start reg)
                                                     (region-len reg)
                                                     1  ;diamond
                                                     (om* (+ (nth (corde op)
                                                                  om::*cb-open-strings*)
                                                             (pos op))
                                                          100))

                                collect (make-region (region-start reg)
                                                     (region-len reg)
                                                     2  ;open string
                                                     (om* (nth (corde op)
                                                               om::*cb-open-strings*)
                                                          100)))))))
      (mki 'om::voice 
           :tree (om::tree voice)
           :chords (loop for chord in (om::chords voice)
                         collect (om::add-extra chord
                                                (mki 'om::head-extra :thehead "a")
                                                '(0)
                                                t))))))
    

                             
  
                          
  