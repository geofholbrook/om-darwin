
(in-package om)

(defmethod! c-list (&rest cs)
  :icon 702
  #'(lambda (x)
      (loop for crit in cs
            sum (evaluate x crit))))


(defmethod! c-print ()
  :icon 702
  #'(lambda (x)
      (print x)
      1.0))


(defmethod! c-pc (test-value)
  :icon 702
  (criterion nil :pitch-class test-value nil))


(defmethod! c-pitch (test-value)
  :icon 702
  (criterion nil :pitch test-value nil))


(defmethod! c-melodic (test-value &optional signed?)
  :icon 702
  (criterion nil 
             (if signed?
                 :signed-melodic
               :melodic)
             test-value 
             nil))


(defmethod! c-rate ((self d::criterion) rate)
  :icon 702
  (criterion (d::evaluator self)
             (d::subject self)
             (d::test-value self)
             rate))

(defmethod! c-weight ((self d::criterion) weight &optional (expt d::*default-expt*) (index-expt d::*default-index-expt*))
  :icon 702
   (criterion (d::evaluator self)
              (d::subject self)
              (d::test-value self)
              (d::rate self)
              weight expt index-expt))
  





(in-package dwn)

(om::defmethod! add (&rest functions)
  :icon 702

  (let ((funs (remove-if-not #'functionp functions)))
    (lambda (spec)
      (apply '+ (mapcar #'(lambda (crit)
                            (evaluate spec crit))
                        funs)))))

(om::defmethod! each-elt (&rest functions)
  :icon 702 

  (lambda (spec)
    (loop for elt in spec
          sum (funcall (apply #'add functions)
                       elt))))

(om::defmethod! each-unit ((decoder list) &rest functions)
  :icon 702 

  (lambda (spec)
    (loop for elt in (p.codes spec decoder)
          sum (funcall (apply #'add functions)
                       elt))))

(om::defmethod! each-atom (&rest functions)
  :icon 702 

  (lambda (spec)
    (loop for elt in (flat spec)
          sum (funcall (apply #'add functions)
                       elt))))

(om::defmethod! each-sub ((len integer) &rest functions)
  :icon 702

  (lambda (spec)
    (loop for sub on spec
          while (>= (length sub) len)
          sum (funcall (apply #'add functions)
                       (first-n sub len)))))

(om::defmethod! each-pc (&rest functions)
  :icon 702 

  (lambda (spec)
    (loop for elt in (mapcar #'(lambda (p) (mod p 12)) (flat spec))
          sum (funcall (apply #'add functions)
                       elt))))

(om::defmethod! with-pheno ((phenotyper function) &rest functions)
  :icon 702

  (lambda (spec)
    (funcall (apply #'add functions)
             (funcall phenotyper spec))))

(om::defmethod! zero () :icon 702 (lambda (spec) 0.0))

(om::defmethod! test () :icon 702 (lambda (spec) (print spec) 0.0))


(defun fun-and (&rest functions)
  (lambda (x) (every #'(lambda (fun) (funcall fun x)) functions)))

(om::defmethod* om::om+ ((fun1 function) (fun2 function))
  (add fun1 fun2))



(om::defmethod! often (freq &rest functions)
  :icon 702
  ;for enforcing a frequency of truth of a given function. applies an 'and' operator if more than one function is given
  ;converting to an integer count and taking the floor of the desired ratio means that "as close as possible" will give a fitness of 0 (ideal)
  (lambda (spec)
    (floor (abs (- (* freq (length spec))
                 (count-if #'(lambda (elt)
                                  (funcall (apply #'d::fun-and functions)
                                           elt))
                              spec))))))


                    
(om::defmethod! often-sub (freq width &rest functions)
  :icon 702
  ;for enforcing a frequency of truth of a given function. applies an 'and' operator if more than one function is given
  ;converting to an integer count and taking the floor of the desired ratio means that "as close as possible" will give a fitness of 0 (ideal)
  (lambda (spec)
    (floor (abs (- (* freq (length spec))
                   (loop for sub on spec
                         count (funcall (apply #'d::fun-and functions)
                                        (subseq sub 0 width))))))))






 
