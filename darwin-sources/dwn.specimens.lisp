(in-package dwn)



;;; »»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»
;;; »»»»»»» SP-LIST : basic specimen »»»»»»»»»»»»
;;; »»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»

(defparameter *sp-list-max-mutations* 3)


; basic specimen whose genotype is a list of integers.
; mutation alters 1 value of the list, repeated from 1 to *sp-list-max-mutations* times

(defclass sp-List (specimen)
     
  ((len :initform 10 :accessor len :initarg :len)
   (range :initform '(60 72) :accessor range :initarg :range)
   (freeze :initform nil :accessor freeze :initarg :freeze)))

 ;freeze, if not nil, is of the form (a b), prevents mutation of any gene whose position modulo a is not b
 ;b can be a list.

 ;freeze can also be t, which means that it cannot be mutated at all.



(defmethod rnd-genotype ((self sp-list))
  (loop repeat (len self) collect (rrnd (range self))))


(defmethod mutate-once! ((self sp-list) &rest args)  ;;; destructive
  (let* ((spot (loop for spt = (rrnd 0 (1- (len self)))

                     ;;; keep choosing spots until we find one that's not 'frozen' (see freeze format above)

                     until (not (and (freeze self)
                                     (member (mod spt (first (freeze self)))
                                             (om::list! (second (freeze self))))))

                     finally return spt))
                                 
         (new-value (rrnd (range self)))
         (alteration (- new-value (nth spot (geno self)))))
    
    (if (= alteration 0)
        (apply #'mutate-once! self args) ;; try again

      (progn
        (setf (geno self)
              `(,@(subseq (geno self) 0 spot)
                ,new-value
                ,@(subseq (geno self) (1+ spot))))
        
        (push (mki 'mutation
                   :alteration alteration
                   :location spot)

              (last-mutation self))))))    
   ;;; when is 'last-mutation' cleared? 
   ;;; it's meant to be used in conjuction with the stored phenotype, to calculate the new phenotype
   ;;; faster, right? That means it should be cleared with the phenotype is calculated and stored. (check)


(defmethod mutate ((self sp-list) &rest args)
  (unless (equalp (freeze self) t)
    (loop repeat (rrnd 1 *sp-list-max-mutations*)
          do (mutate-once! self))))




(defmethod crossover ((spec1 sp-list) (spec2 sp-list))
  (let ((new (true-copy spec1))
        (cross-geno (true-copy (geno spec2)))
        (spot (random (1- (length (geno spec1))))))

    (rplacd (nthcdr spot (geno new))
            (nthcdr (1+ spot) cross-geno))

    (values new
            (make-instance 'mutation
                           :alteration :crossover
                           :location spot))))




;;; »»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»
;;; »»»»»»»»»»»»»»» PHENOTYPING »»»»»»»»»»»»»»»»»
;;; »»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»


(om::defclas sp-pheno (specimen)
  ((phenotyper :initform nil)
   (pheno-args :initform nil)
   (pheno :initform nil)))


(defmethod phenotype ((self sp-pheno))
  (when (and (phenotyper self)
             (null (pheno self)))
    (setf (pheno self)
          (apply (phenotyper self) `(,self ,@(pheno-args self)))))
  (pheno self))


; evaluate the phenotype, not the genotype
(defmethod eval-subject ((self sp-pheno)) (pheno self))

(defmethod clear-pheno ((self sp-pheno))
  (setf (slot-value self 'pheno) nil))

(defmethod mutate :after ((self sp-pheno) &rest args)
  (clear-pheno self))









