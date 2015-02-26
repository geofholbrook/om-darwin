
(in-package dwn)

;;; make-criterion follows the guideline given on page 29 of Holbrook DMA thesis.




;;; or this should be a macro ...



;;; reference: arguments are _evaluator _subject _test-value _rate _input
;;; with-subject-loop provides _subj and _length

(defcase direct (:fun nil nil nil)
         (let ((result (funcall _evaluator _input)))
           (if (numberp result)
               result
             (if result 0 1))))

(defcase simple-iterator (:fun :iterator nil nil)
         (with-subject-loop (_input)
                            sum (funcall %evaluator _subj)
                            finally return (/ _subj _length)))
          
               


         

