(in-package dwn)

;inheritance test (good so far)
(defspecies melody2 (melody))


(defspecies trills (arrangement)
  :phenotyper   ;;; change only the pheno

  (loop for op in (operons self)
        append (loop for start from (start op) by (resolution self)
                     repeat (floor (len op) (resolution self))
                     for k from 0
                     collect (make-region start (resolution self)
                                          (channel op)
                                          (+ (pitch op) (if (evenp k)
                                                            0 1))))))
                                          
        


