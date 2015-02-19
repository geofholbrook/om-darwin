(in-package dwn)


(defparameter *kill-your-parents* nil)

;;; »»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»
;;; »»»»»»» POPULATION / ENVIRONMENT »»»»»»»»»»»»
;;; »»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»


(om::defclas population (dwn-object)
  ((environment :initform nil)
   ;(worst-fitness :initform nil)    ;;; don't remember why this was here...
   (specimens :initform nil)
   (initial-model :initform nil)
))

;have to do this to avoid stack overflow (circular reference b/n specimen and population)
(defmethod true-copy ((self population))
  self)

(om::defclas environment (dwn-object)
  ((criterion :initform nil)
   (capacity :initform *default-capacity*)
   (litter-size :initform *default-litter-size*)
   ))

(defmethod initialize-instance :after ((self population) &rest args)
  (setf (environment self)
        (make-instance 'environment)))

(defmethod contents ((self population))
  (loop for S in (specimens self)
        collect (contents S)))

;;; »»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»

(defmethod environment ((self specimen))
  (environment (population self)))

(defmethod criterion ((self specimen))
  (criterion (environment (population self))))

;;; »»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»

(defun make-population (crit model)
  (let ((pop (make-instance 'population :initial-model model)))
    (setf (criterion (environment pop)) crit) 
    (setf (fitness model)
          (fraction-value (evaluate model crit)))
    (add-specimen pop model)
    (loop repeat (1- (capacity (environment pop)))
          do (add-specimen pop (true-copy model)))   ;(add-random-specimen pop))
    (sort-specimens pop)
    pop))


;;; »»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»
;;; »»»»»»» population ops »»»»»»»»»»»»»»»»»»»»»»
;;; »»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»



(defmethod add-specimen ((self population) (specimen specimen))
  (setf (population specimen) self)
  (push specimen (specimens self)))


(defmethod add-random-specimen ((self population))
  (let ((new (true-copy (initial-model self))))

    (setf (geno new) (rnd-genotype new))

    (setf (fitness new)
          (fraction-value (evaluate new (criterion (environment self)))))   

    (add-specimen self new)))


(defmethod sort-specimens ((self population))
  (setf (specimens self)
        (sort (specimens self) #'> :key #'(lambda (s) (fitness s)))))


(defmethod iterate ((self population))
"destructively enacts one generation on a population"

  ; duplication stage
  (let ((offspring (loop for parent in (specimens self)
                         do 
                         (setf (parent parent) nil)
                         append (loop repeat (litter-size (environment self))
                                      collect 
                                      (let ((new (true-copy parent)))
                                        (mutate new)
                                        ;(if (< (random 100) *cross-chance*)
                                        ;             (crossover parent (choose-other (specimens self) parent))
                                        (setf (fitness new)
                                              (d::evaluate new (criterion (environment self))))
                                        new)))))

    (when *kill-your-parents*
      (setf (specimens self) (loop repeat (capacity (environment self))
                                   collect (pop offspring))))
            

  ; sorting and elimination stage
    (loop for child in offspring
          do
          (unless (and (specimens self) (> (fitness child) (fitness (first (specimens self)))))
            (loop for sub on (specimens self)
                  while (cdr sub)
                  until (< (fitness (cadr sub)) (fitness child))
                  finally
                  (rplacd sub (cons child (cdr sub)))
                  (pop (specimens self)))))))



;;; »»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»
;;; »»»»»»» TOP LEVEL »»»»»»»»»»»»»»»»»»»»»»»»»»»
;;; »»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»


(defmethod best ((self population))
  (car (last (specimens self))))

(defvar *best* nil)

(defmethod run ((self population) &optional num-generations)

;;  (setf *cross-chance* 0)    ;;; problem with (cross)

  (loop for generation from 0 
        until (and num-generations (> generation num-generations))
        do 
        (iterate self)
        
          ;      (when (< (fitness (om::last-elem (specimens pop)))
          ;               0.1)
          ;        (setf *cross-chance* 50))
        
        (when (and *display-interval*
                   (= (mod generation *display-interval*) 0))
          
          (setf *c-list-verbose* t)
          (evaluate (best self) (criterion (environment self)))
          (setf *c-list-verbose* nil)

          (print (list generation (fitness (best self))   ;(geno (best self))
                       )))
        
        
        until (= (fitness (best self)) 0)
        
        finally 
        return (progn
                 (setf *best* (best self))
                 ;(print (geno (best self)))
                 (best self))))
    
    

;;; »»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»


;;; non-GA linear method for quickly getting a satisfactory specimen

(defparameter *optimizer-display-interval* 1)


(defmethod focus->real-index (i focus)
  (if focus
      (multiple-value-bind (q r) (floor i (length (second focus)))
        (+ (* q (first focus))
           (nth r (second focus))))
    i))

(defmethod optimize-specimen ((spec sp-list) (crit criterion) &optional (speed 1) (mutate-only nil))
; mutate-only is of the form (modulo (<list of remainders)) to show which genes may be modified
; ex. mutate-only = (4 (0 1)) means only (0 1 4 5 8 9 ...) can be modified.

  (let* ((new (true-copy spec))

        ;number of genes that may be modified
         (valid-genes (if mutate-only
                          (* (length (geno spec)) (/ (length (second mutate-only))
                                                     (first mutate-only)))
                        (length (geno spec)))))
               
    (loop for valid-index from 0 to (1- valid-genes)
          for index = (focus->real-index valid-index mutate-only)
          
          do 

          (when (= (mod index *optimizer-display-interval*) 0)
            (print (format nil "optimizing gene #~D (~D) of ~D" valid-index index valid-genes)))

          (loop for value from (first (range spec)) to (second (range spec)) by speed
                with best-values and best-fitness     ;;; best values will be tied for most fit
                do

                (subs-posn (geno new) index value)
                (clear-pheno new)

                (let ((fitness (evaluate new crit)))
                  (if (or (null best-values) (< fitness best-fitness))
                      (setf best-values (list value)
                            best-fitness fitness)
                    (if (= fitness best-fitness)
                        (push value best-values))))

                finally do

                (setf (nth index (geno new)) 
                      (nth-random best-values))))
    
    (print (format nil "optimized fitness = ~D" (evaluate new crit)))
    
    new))
                         
          





                
