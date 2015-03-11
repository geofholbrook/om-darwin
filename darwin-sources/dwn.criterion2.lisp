
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



(defmethod correct-boolean ((output (eql t))) 0)
(defmethod correct-boolean ((output number)) output)
(defmethod correct-boolean ((outpu (eql nil))) 1)
(defmethod correct-boolean ((output list)) (mapcar 'correct-boolean output))


(defmethod evaluate ((self t) (crit criterion) &rest args)
  (expt (* (apply '+ (list! (om^ (correct-boolean (funcall (fun crit) self))
                                 (index-exponent crit))))
           (weight crit))
        (exponent crit)))


(defmacro special-make-criterion (kind class lambda-list &body body)
  `(mki ',(or class 'criterion)
        :kind ,kind 
        :evaluator evaluator
        :subject subject
        :test-value test-value
        :rate rate
        :weight (or weight *default-weight*)
        :exponent (or exponent *default-expt*)
        :index-exponent (or index-exponent *default-index-expt*)
        :fun #'(lambda ,lambda-list
                 ,@body
                 )))

(defun adjacent-pairs-by-channel (arr)
  (loop for chan in (demix arr 'region-chan)
        append (loop for sub on chan
                     if (cdr sub)
                     collect (first-n sub 2))))

(defmethod get-subject-list ((arr list) (subject-keyword t))   ;;; hope it's an arrangement?
  (case subject-keyword
    (:region arr)
    
    (:pitch (mapcar 'region-pitch arr))
    (:pitch-class (mapcar #'(lambda (r)
                              (mod (region-pitch r) 12)) arr))

    (:adjacent (adjacent-pairs-by-channel arr))

    (:signed-melodic (loop for pairs in (adjacent-pairs-by-channel arr)
                           collect (- (region-pitch (second pairs))
                                      (region-pitch (first pairs)))))

    (:melodic (om-abs (get-subject-list arr :signed-melodic)))))

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


(defmethod! om::criterion ((evaluator t) (subject symbol) (test-value t) (rate t) 
                     &optional weight exponent index-exponent)
  :icon 702
  :initvals (list nil nil nil nil)

  :menuins '((1 (("regions" :regions)
                ("adjacent" :adjacent)
                ("pitch" :pitch)
                ("pitch-class" :pitch-class)
                ("melodic" :melodic)
                ("signed-melodic" :signed-melodic))))

  (special-make-criterion :iterator () (spec)
    (loop for elt in (get-subject-list spec subject)
          collect (compare-to-test-value (funcall (or evaluator #'identity) elt)
                                         test-value))))









                                

;(defcrit simple-iterator (:fun :iterator nil nil)
;         (with-subject-loop (_input)
;                            sum (funcall %evaluator _subj)
;                            finally return (/ _subj _length)))
          
               


         

