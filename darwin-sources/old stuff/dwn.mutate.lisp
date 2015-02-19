(in-package darwin)(defun change-by-gdef (location genotype gdef &optional canned)"mini universal mutation-function that makes a random change to a specified location (see super-nth function). this is an importatn function for definitions.lisp, to define the mutation function for a given 'genus' returns a three-element list : the new genotype, the location altered, and the change made. This extra information can be used to make subsequent sympathetic alteration to the genotype"     ;(declare (notinline change-by-gdef))  (if location     (let* ((spot (if (listp (car location))                   (rrnd (car location))                   (if (equalp (car location) 'choose)                     (rrnd 0 (1- (length genotype)))                     (car location))))           (new-element (change-by-gdef                          (cdr location)                         (nth spot genotype)                         (if (equalp (car gdef) 'lis)                           (third gdef)               ; lis as the first element means that the format is (lis num-elements <smaller gdef>                           (nth spot gdef))                         canned)))                    ; no keyword as the first element refers to a mixed-list, in which case                                                      ; the gdef is a list of smaller gdefs      (list (alter-nth spot genotype                       (first new-element))            (cons spot (second new-element))            (third new-element)))    (if (listp genotype)      (if (find-if-not #'atom genotype)        (break "expecting a num-thingie -- problem with location argument <change-by-gdef>")      ; returns the list of numbers (genotype) altered by a fixed amount      ; for example, transposing a chord              (let ((delta (make-different                       0                                            ;returns allowable range (such that no number exits its own range)                      (do (result                         ;holds the current smallest range                           (sub-geno genotype (cdr sub-geno))                           (range (if (equalp (car gdef) 'lis)                                    (list (second (third gdef)))                                    (mapcar #'second (car gdef)))                                  (if (equalp (car gdef) 'lis)                                    range                                    (cdr range))))                          ((null sub-geno)                           result)                         (setf result (range-intersection      ; sets result to the new range if it is smaller                                      (mapcar #'(lambda (border) (- border (car sub-geno)))                                              (car range))                                      result)))                      nil                      canned)))          `(,(mapcar #'(lambda (item)                          (+ item delta))                     genotype)            nil            ,delta)))      (multiple-value-bind (new-num delta)                            (make-different genotype (second gdef) nil canned)        (list new-num nil delta)))))