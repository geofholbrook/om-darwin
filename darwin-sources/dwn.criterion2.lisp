
(in-package dwn)

;;; make-criterion follows the guideline given on page 29 of Holbrook DMA thesis.

(defparameter *default-weight* 1)
(defparameter *default-expt* 1.1)
(defparameter *default-index-expt* 1.2)

(om::defclas criterion ()
             ((kind)
              (evaluator) 
              (subject)
              (test-value) 
              (rate) 
              (weight :initform *default-weight*)
              (exponent :initform *default-expt*)
              (index-exponent :initform *default-index-expt*)
              (fun)))


(defmethod correct-boolean ((output t))
  (if (numberp output)
      output
    (if output 0 1)))

(defmethod correct-boolean ((output list)) (mapcar 'correct-boolean output))


(defmethod evaluate ((self t) (crit criterion) &rest args)
  (expt (* (apply '+ (list! (om^ (correct-boolean (funcall (fun crit) self))
                                 (index-exponent crit))))
           (weight crit))
        (exponent crit)))

;;; with-subject-loop provides _subj and _length

(defmacro special-make-criterion (kind class lambda-list &body body)
  `(mki ',(or class 'criterion)
        :kind ,kind 
        :evaluator evaluator
        :subject subject
        :test-value test-value
        :rate rate
        :weight (or weight *default-weight*)
        :exponent (or exponent *default-expt*)
        :addend-exponent (or index-exponent *default-index-expt*)
        :fun #'(lambda ,lambda-list
                 ,@body
                 )))

;direct
(defmethod! om::crit ((evaluator function) (subject t) (test-value t) (rate t) 
                     &optional weight exponent index-exponent)
  :icon 704
  (special-make-criterion :direct () (spec) 
    (funcall evaluator spec)))

(defmethod om::crit ((evaluator function) (subject (eql :operons)) (test-value t) (rate t)  
                     &optional weight exponent index-exponent)
  :icon 704
  (special-make-criterion :iterator () (spec)
    (loop for op in (operons spec)
          collect (funcall evaluator op))))



                                

;(defcrit simple-iterator (:fun :iterator nil nil)
;         (with-subject-loop (_input)
;                            sum (funcall %evaluator _subj)
;                            finally return (/ _subj _length)))
          
               


         

