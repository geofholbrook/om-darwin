(in-package dwn)

(defparameter *capacity* 10)             ;;; number of specimens that survive each generation
(defparameter *litter-size* 5)           ;;; number of copies made of each specimen
(defparameter *longevity* 50)            ;;; number of generations a specimen can survive while not in first place

(defparameter *display-interval* 1)      ;;; how often, in generations, the current best fitness (lowest score) is displayed



(defvar *mutation-mode*)
(setf *mutation-mode* '(:delta 3))       ;;; mutation type, followed by arguments

;;; :delta means a change of one element by between 1 and the first argument, up or down. no range restriction
;;; :range means a different number is chosen within the given range (first and second arguments)

(defvar *mutation-func* 'm.diffdx)
(defvar *mutation-args* '(0.5))
(defvar *mutation-rpt* 1)                ;;; maximum number of point mutations

(defvar *gene-range* '(0 255))           ;;; numeric range for a nucleotide (unit of the raw genotype)

(defvar *fix-offset* 1)

(defvar *most-recent-result* nil)
(defun get-recent-result () *most-recent-result*)


(defmethod random-raw-genotype (n) (loop repeat n collect (rrnd *gene-range*)))

(defmethod evaluate ((self t) (crit function) &rest args)
  (let ((result 
         (funcall crit self)))
    (if (numberp result)
        result
      (if result 0 1))))  ;;; i.e., true means 0, which means good.


(defmethod mutate ((self list))
  (let ((new (copy-list self)))
    (loop repeat (rrnd 1 *mutation-rpt*)
          do (setf new 
                   (apply *mutation-func* new *mutation-args*)))
    new))

(defmacro mutate! (thing)
  `(setf ,thing (mutate ,thing)))


;;; this is a catch-all mutation function. it will apply a complementary mutation some of the time 
;;; (frequency determined by fix-chance, or *mutation-args*)

(defun m.diffdx (lis fix-chance)
  (let ((fix? (weighted-coin (* fix-chance 100)))
        (spot (rrnd 0 (1- (length lis)))) 
        tmp)
        
    ;store original value
    (setf tmp (nth spot lis))

    ;primary alteration
    (setf (nth spot lis) 
          (rnd-other (nth spot lis) *gene-range*))

    ;'correction' alteration
    (when (and fix? (< spot (- (length lis) *fix-offset*)))
      (setf (nth (+ spot *fix-offset*) lis)
            (mod-to-range (- (nth (+ spot *fix-offset*) lis)
                             (- (nth spot lis) tmp))
                          *gene-range*)))
    
    lis))
    
   

;; for OM
(defun set-mutation-mode (mode) (setf *mutation-mode* mode))
(defun set-fix-offset (x) (setf *fix-offset* x))
(defun set-gene-range (range) (setf *gene-range* range))



;;; assumes lists are the same length. takes first part of self, second part of other
(defmethod cross ((self list) (other list)) 
  (let ((spot (rrnd 1 (1- (length self)))))
    (copy-list (append (subseq self 0 spot)
                       (subseq other spot)))))



;;;; MAIN ITERATION

;; note: each individual is represented as a list: (<fitness>  <specimen> <age>)

;;; destructively iterates population
(defmethod iterate ((population t) (criterion function))
  (let* ((crosses (when (> (length population) 1)
                    (loop repeat *capacity*
                          collect (let* ((index1 (random (length population)))
                                         (index2 (rnd-other index1 (list 0 (1- (length population)))))
                                         (crossed (cross (second (nth index1 population))
                                                         (second (nth index2 population))))
                                         (fitness (evaluate crossed criterion)))
                                    (list fitness crossed 0)))))

         (offspring (loop for entry in (append population crosses)
                          append (loop repeat *litter-size*
                                       collect
                                       (let* ((mutated (mutate (second entry)))
                                              (fitness (evaluate mutated criterion)))
                                         (list fitness mutated 0))))))

                     ;(print (format nil "number of unique fitnesses: ~D"
                     ;               (length (remove-duplicates (mapcar 'first (append population crosses offspring))))))

                       
    (let ((sorted (sort (append population 
                                offspring 
                                crosses)
                        #'(lambda (s1 s2)
                            (if (= (car s1) (car s2))
                                (> (caddr s1) (caddr s2))
                              (< (car s1) (car s2)))))))

          ;;; sorts first by fitness, then by age ... older specimens survive so that they can't survive
          ;;; by just alternating between equivalent raw genotypes. if it weren't for this problem,
          ;;; really the younger specimens should survive!

          (loop for sp in sorted
                              
                for k from 0
                with fitnesses = ()  
                until (= (length result) *capacity*)

                              
                ;;; testing for duplicates using fitness
                ;;; since different raw genotypes can have the same phenotype
                ;;; this could potentially pare the population down to 1 if the fitness is low integers or something ...
                      
                if (or (= k 0) ;current best
                       (and (< (caddr sp) ;age
                               *longevity*)
                            (not (member (car sp) fitnesses))))
                      
                collect sp into result
                do (push (car sp) fitnesses)
                finally return result))))



(defmethod population-from-model ((model list) (criterion function))
  (loop repeat *capacity*
        collect 
        ;;; each 'entry' is a list: (<fitness>  <specimen> <age>)
        (let ((spec (random-raw-genotype (length model))))
          (list (evaluate spec criterion) spec 0))))



(defmethod run ((model t) (criterion function) (max-generations number) &key (finalizer #'identity))
  (let ((best
         (let ((population (population-from-model model criterion)))
                       
      
           (loop for generation from 0 to max-generations
                 until (or (> generation max-generations) 
                           (= (first (first population)) 0))   ;;; perfect specimen

                 do 

                 (print generation)

                 (setf population
                       (iterate population criterion))
         
                 ;;; verbosity
                 (when (= (mod generation *display-interval*) 0)
                   (print (format nil "generation #~D -- fitness ~D (runner up is ~D) size ~D" 
                                  generation 
                                  (first (first population))
                                  (first (second population))
                                  (length population))))

                 finally 
                 return (progn 
                          (print population)
                          (first population))))))

    (setf *most-recent-result* (second best))

    (values (funcall finalizer (second best))  ;specimen
            (first best)   ;fitness 
            )))






#|

 (defmethod mutate ((self list) &rest args)
   (let ((new (copy-list self)))
     (case (car *mutation-mode*)
       (:delta
        (let ((max-delta (second *mutation-mode*)))
          (incf (nth (rrnd 0 (1- (length new))) new)
                (* (rrnd 1 max-delta) (if (= (rrnd 1 2) 1) 1 -1)))))
      
       (:range 
        (let ((spot (rrnd 0 (1- (length new)))))
          (setf (nth spot new) 
                (rnd-other (nth spot new) (list (second *mutation-mode*)
                                                (third *mutation-mode*))))))

       (:dx-fix     ;;; for dx phenotypers, change the following value to offset a random change
        (let ((spot (rrnd 0 (1- (length new))))
              (min (second *mutation-mode*))
              (max (third *mutation-mode*))
              (fix-chance (fourth *mutation-mode*)))
          (let ((new-value (rnd-other (nth spot new) (list min max))))
             
           
            (when (and (< spot (1- (length new)))
                       (weighted-coin fix-chance))
              (let ((corrected (- (nth (1+ spot) new)
                                  (- new-value (nth spot new)))))
                (when (withinp corrected min max)
                  (setf (nth (1+ spot) new) corrected))))

            (setf (nth spot new) new-value)))))
     new))

|#