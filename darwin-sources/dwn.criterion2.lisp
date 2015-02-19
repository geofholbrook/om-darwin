
(in-package dwn)

;;; make-criterion follows the guideline given on page 29 of Holbrook DMA thesis.




;;; or this should be a macro ...

(defmethod make-criterion ((evaluator function) (subject (eql nil)) (test-value t) (rate (eql nil)))
  #'(lambda (spec)
      (let ((output (funcall evaluator spec)))
        (cond ((null output) 1.0)
              ((equal output t) 0.0)
              ((numberp output) output)
              (t (break "invalid evaluator output")))))) 

