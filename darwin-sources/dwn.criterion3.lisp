
(in-package dwn)

;;; make-criterion follows the guideline given on page 29 of Holbrook DMA thesis.

(defparameter *default-weight* 1)
(defparameter *default-expt* 1.1)
(defparameter *default-index-expt* 1.2)

(om::defclas criterion ()
             ((evaluator) 
              (subject)
              (test-value) 
              (rate) 
              (weight :initform *default-weight*)
              (exponent :initform *default-expt*)
              (index-exponent :initform *default-index-expt*)))


(defun applicable-average (lis)
  (om::average (correct-boolean (remove :n/a lis))))


(defun dynamic-rate (rate lis)
  ;;; dynamic part not implemented
  ;;; lis is a list of boolean values (or :n/a)
  (if (numberp rate)
      (let ((lis-1 (remove :n/a lis)))
        (let ((rate-1 (/ (round (* rate (length lis-1)))
                         (length lis-1))))
          (offby rate-1 (/ (count t lis-1)
                           (length lis-1)))))))


(defun dynamic-proximities (goal lis)
  ;;; dynamic part not implemented
  
  (if (numberp goal)
      (mapcar #'(lambda (sub)
                  (offby sub goal))
              lis)))



(defmethod get-subfitnesses ((self specimen) (crit criterion))
  (get-subfitnesses (phenotype self) crit))

(defmethod get-subfitnesses ((self t) (crit criterion))
  (flet ((agrees (subj test-value)
           (if (equalp (car test-value) :set)
               (memberp subj (cdr test-value))
             (equalp subj test-value)))

         (boolean-to-fitness (eval)
           (cond
            ((equalp eval t) 0)
            ((null eval) 1)
            (t eval)))
         
         (is-set-p (x) (and (listp x) (equalp (car x) :set))))

    

    (if (subject crit)
        (let* ((subject-list (get-subject-list self (subject crit)))
               (sub-evals (if (evaluator crit)
                              (mapcar (evaluator crit) subject-list)
                            subject-list)))
          (if (test-value crit)
              (if (rate crit)

                  ;;; ### proximity to goal rate of agreement or membership [ of evaluator values ]   LINES 9 10 14 15
                  (dynamic-rate (rate crit)
                                (mapcar #'(lambda (sub)
                                            (if (is-set-p (test-value crit))
                                                (memberp sub (cdr (test-value crit)))
                                              (equalp sub (test-value crit))))
                                        sub-evals))

                (if (is-set-p (test-value crit))

                    ;;; ### rate of non-membership [ of evaluator values ] LINES 8 13
                    (dynamic-rate 1 (mapcar #'(lambda (sub) 
                                                (memberp sub (cdr (test-value crit))))
                                            sub-evals))

                  ;;; ### disagreements [ of evaluator values ]  LINES 7 12
                  (mapcar #'(lambda (sub) (offby sub (test-value crit))) sub-evals)))

            ;;; no test-value
            (if (rate crit)

                ;;; ### proximity to goal rate of success  LINE 11
                (dynamic-rate (rate crit) sub-evals)  ;; evaluator should return boolean value

              (if (evaluator crit)

                  ;;; ### average evaluator value, or failure rate  LINES 3 4
                  (mapcar #'boolean-to-fitness sub-evals)

                (break "error: an evaluator or a test-value must accompany the subject argument")))))
        
        ;;; no subject
        (if (test-value crit)
            (if (is-set-p (test-value crit))
            
                ;;; ### single membership test (true -> 0; false -> 1) LINE 6
                (if (member (funcall (evaluator crit) self)
                            (test-value crit))
                    0
                  1)

              ;;; ### evaluator value disagreement     LINE 5
              (offby (funcall (evaluator crit) self) (test-value crit )))

          ;;; ### evaluator value, or single test (true -> 0; false -> 1)  LINES 1 2
          (boolean-to-fitness (funcall (evaluator crit) self))))))
            
              

(defmethod evaluate ((self t) (crit criterion) &rest args)
  (expt (* (loop for sub in (list! (get-subfitnesses self crit))
                 sum (expt sub (index-exponent crit)))
           (weight crit))
        (exponent crit)))

(defmethod evaluate ((self t) (crit list) &rest args)
  (evaluate self (apply 'om::c-list crit)))


(defmethod correct-boolean ((output (eql t))) 0)
(defmethod correct-boolean ((output number)) output)
(defmethod correct-boolean ((output (eql nil))) 1)
(defmethod correct-boolean ((output list)) (mapcar 'correct-boolean output))


(defun adjacent-pairs-by-channel (arr)
  (loop for chan in (demix arr 'region-chan)
        append (loop for sub on chan
                     if (cdr sub)
                     collect (first-n sub 2))))


(defmethod get-subject-list ((self om::chord-seq) (subject-keyword t))
  (break)
 (case subject-keyword
    (:chord (om::inside self))

    (:adjacent (loop for sub on (om::inside self)
                     if (cdr sub)
                     collect (first-n sub 2)))
    
    (:pitch (flat (mapcar #'om::lmidic (om::inside self))))

    (:pitch-class (mapcar #'(lambda (midic) (mod midic 1200))
                          (flat (mapcar #'om::lmidic (om::inside self)))))


    (:signed-melodic (x->dx (get-subject-list self :pitch)))

    (:melodic (om-abs (get-subject-list self :signed-melodic)))

    ))


(defun get-subject-list-flat (self subject-keyword)
  (case subject-keyword
    (:elements self)
    (:adjacent-elements (loop for sub on self 
                                  while (cdr sub) 
                                  collect (first-n sub 2)))
    (:melodic (om-abs (x->dx self)))
    (:signed-melodic (x->dx self))))

(defmethod get-subject-list ((self list) (subject-keyword t))   ;;; hope it's an arrangement?

  (if (and (atom (car self))
           (not (equalp (car self) :header)))
      ;flat list
      (get-subject-list-flat self subject-keyword)
    (let ((regions (arr-regions self)))
      (case subject-keyword
        (:elements self)
        (:adjacent-elements (loop for sub on self 
                                  while (cdr sub) 
                                  collect (first-n sub 2)))

        (:regions regions)
    
        (:pitch (flat (mapcar 'region-pitch regions)))
        (:pitch-class (flat (mapcar #'(lambda (r)
                                        (second (multiple-value-list 
                                                 (om// (om/ (region-pitch r) 100) 12))))
                                    regions)))

        (:adjacent (adjacent-pairs-by-channel regions))

    
        (:signed-melodic (loop for pair in (adjacent-pairs-by-channel regions)
                               collect (- (region-pitch (second pair))
                                          (region-pitch (first pair)))))

        (:melodic (om-abs (get-subject-list regions :signed-melodic)))


        (:signed-harmonic (loop for pair  in (get-vertical-diads regions)
                                collect (- (region-pitch (second pair))
                                           (region-pitch (first pair)))))
     
        (:harmonic (om-abs (get-subject-list regions :signed-harmonic)))

        (:attacks (demix regions #'region-start))

        ))))

(defmethod get-subject-list ((self specimen) (subject-keyword t))
  (case subject-keyword
    (:operons (operons self))
    (otherwise (get-subject-list (phenotype self)
                                 subject-keyword))))
    

;direct
(defmethod! om::criterion ((evaluator t) (subject t) (test-value t) (rate t) 
                     &optional weight exponent index-exponent)
  :icon 702
  (special-make-criterion :direct () (spec) 
    (funcall evaluator spec)))



(defmethod compare-to-test-value ((eval-result number) (test-value t))
  (if test-value
      (offby eval-result test-value)
    eval-result))

(defmethod compare-to-test-value ((eval-result t) (test-value (eql nil))) eval-result)


(defmethod! om::criterion ((evaluator t) (subject t) (test-value t) (rate t) 
                     &optional weight exponent index-exponent)
  :icon 702
  :initvals (list nil nil nil nil)

  :menuins '((1 (("regions" :regions)
                 ("adjacent" :adjacent)
                 ("nthcdr" :nthcdr)
                 ("pitch" :pitch)
                 ("pitch-class" :pitch-class)
                 ("melodic" :melodic)
                 ("signed-melodic" :signed-melodic))))

  (mki 'criterion 
       :evaluator evaluator
       :subject subject
       :test-value test-value
       :rate rate))

(om::defclas with-criterion (criterion) ())

(defmethod! om::with-pheno ((evaluator t) (subject t))
  :icon 702

  (mki 'with-criterion 
       :evaluator evaluator
       :subject subject))

(defmethod evaluate ((self t) (crit with-criterion) &rest args)
  (evaluate (funcall (subject crit) (phenotype self))
            (evaluator crit)))

