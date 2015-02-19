(in-package darwin)

;-----
(defclas sp-redblue (specimen)
  ((len :initform 27)
   (range :initform '(0 10))))

(defmethod rnd-genotype ((self sp-redblue))
  (loop repeat (len self) collect (list (rrnd (range self))
                                        (rrnd (range self)))))



(defmethod mutate ((self sp-redblue) &rest args)
  (let* ((spot (rrnd 0 (1- (len self))))
         (spot2 (rrnd 0 1))
         (new-value (rrnd (range self)))
         (alteration (- new-value (nth spot2 (nth spot (geno self))))))
    
    (setf (geno self)

          `(,@(subseq (geno self) 0 spot)

            ,(if (= spot2 0)
                 (list (first (nth spot self)) new-value)
               (list new-value (second (nth spot self))))

            ,@(subseq (geno self) (1+ spot))))

    (mki 'mutation
         :alteration alteration
         :location (list spot spot2))))




(defun super-curve (x)
  (* (sqrt x) 3.1622))  ;;;; (* (sqrt x) (sqrt 10))


;;;1st degree dx reduction
(defun dx-reduce (lis)
  (let (result
        temp
        (count 1))
    (loop for sub on lis
          while (cddr sub)
          do   
          (push (abs (- (cadr sub) (car sub))) temp)
          (incf count)
          (when (= count 3) 
            (push (approx-decimals (coerce (average temp) 'float) 2) result)
            (setf count 0)
            (setf temp nil))
          finally return (nreverse result))))

(defun dx-reduce (lis)
  (let (result
        temp
        (count 1))
    (loop for sub on lis
          while (cddr sub)
          do   
          (push (abs (- (cadr sub) (car sub))) temp)
          (incf count)
          (when (= count 3) 
            (push (approx-decimals (coerce (average temp) 'float) 2) result)
            (setf count 0)
            (setf temp nil))
          finally return (nreverse result))))

;;;2nd degree dx reduction
(defun dx2-reduce (lis)    
  (let (result
        (temp (list (super-curve (abs (- (cadr lis) (car lis))))))
        (count 2))
    (loop for sub on lis
          while (cddr sub)
          do   
          (push (* (super-curve (abs (- (caddr sub) (car sub)))) .75) temp)   ;;; weigh 2-step interval less than 1-step
          (push (super-curve (abs (- (caddr sub) (cadr sub)))) temp)
          (incf count)
          (when (= count 3) 
            (push (approx-decimals (coerce (average temp) 'float) 2) result)
            (setf count 0)
            (setf temp nil))
          finally return (nreverse result))))

;;; 2nd degre dx reduction with isolation between triplets
(defun dx2-isol-reduce (lis)
  (loop for sub on lis by #'cdddr
        collect
        (average (list
                  (* (super-curve (abs (- (caddr sub) (car sub)))) .75)   ;;; weigh 2-step interval less than 1-step
                  (super-curve (abs (- (caddr sub) (cadr sub))))
                  (super-curve (abs (- (cadr sub) (car sub))))))))
          


(defun avg-reduce (lis)
  (loop for sub on lis by #'cdddr
        collect (average (subseq sub 0 3))))



(defun super-sd (lis) 
  (let ((result (copy-list lis)))
    (loop until (= (length result) 1)
          do (setf result (dx2-reduce result))
          finally return (car result))))

(defun da-reduce (lis ops)
  (let ((result (copy-list lis)))
    (loop for op in (reverse ops)
          do (setf result (if (equalp op 'd)
                              ;(dx2-isol-reduce result)  
                              (dx2-reduce result)
                            (avg-reduce result)))
          finally return 
          (if (= (length result) 1)
              (car result)
            result))))


#|   ;;; old, very slow!!!
(defun frac-trial (depth iters)
  (let* ((c (mki 'basic-criterion :eval-fun #'super-sd :goal 10))
         (pop (make-population 'sp-Data c :dimensions (list (expt 3 depth)) :range '(0 10))))

    (run pop iters)

    (tree (best pop))
    ))
|#

(defun fast-trial (depth iters ops)
  (let* ((c (mki 'simple-criterion 
                 :eval-fun #'(lambda (lis)
                              (da-reduce lis ops))
                 :goal 10))
         (pop (make-population 'sp-list c :len (expt 3 depth) :range '(0 10))))

    (run pop iters)

    (best pop))
    )

(defun comb-trial (depth iters ops-list &optional goal-list weight-list expt-list)
  (let* ((c (mki 'criterion-list
                 :criteria
                 (mapcar #'(lambda (ops goal weight expt)
                             (mki 'simple-criterion 
                                  :eval-fun #'da-reduce
                                  :eval-args (list ops)
                                  :goal goal
                                  :weight weight
                                  :exponent expt))
                         ops-list
                         (or goal-list (loop repeat (length ops-list) collect 10))
                         (or weight-list (loop repeat (length ops-list) collect 1))
                         (or expt-list (loop repeat (length ops-list) collect 1))
                         )))

         (pop (make-population 'sp-list c :len (expt 3 depth) :range '(0 10))))

    (run pop iters)

    (best pop))
    )






;-----
(defclas sp-redblue (specimen)
  ((len :initform 27)
   (range :initform '(0 10))))

(defmethod rnd-genotype ((self sp-redblue))
  (loop repeat (len self) collect (list (rrnd (range self))
                                        (rrnd (range self)))))



(defmethod mutate ((self sp-redblue) &rest args)
  (let* ((spot (rrnd 0 (1- (len self))))
         (spot2 (rrnd 0 1))
         (new-value (rrnd (range self)))
         (new-pair (if (= spot2 1)
                       (list (first (nth spot (geno self))) new-value)
                     (list new-value (second (nth spot (geno self))))))
         (alteration (- new-value (nth spot2 (nth spot (geno self))))))
    
    (setf (geno self)

          `(,@(subseq (geno self) 0 spot)

            ,new-pair

            ,@(subseq (geno self) (1+ spot))))

    (mki 'mutation
         :alteration alteration
         :location (list spot spot2))))

;--------

(defvar *pyth-lookup* nil)

(defun pyth (a b)
  (sqrt (+ (sqr a)
           (sqr b))))


(defun set-pyth-lookup ()
  (setf *pyth-lookup* nil)
  (loop for a from 0 to 10
        do
        (loop for b from 0 to 10
              do
              (push (list (list a b) (pyth a b)) *pyth-lookup*))))

(set-pyth-lookup)
      

;;; pythagorean distance -- takes two lists of two elements each

(defun distance (p1 p2)
  (let ((a (abs (- (first p1) (first p2))))
        (b (abs (- (second p1) (second p2)))))
    (or (second (assoc (list a
                             b) 
                       *pyth-lookup*
                       :test #'equalp
                       ))
        (pyth a b))))


(defun 2D-dx-reduce (lis)
  (let (result
        temp
        (count 1))
    (loop for sub on lis
          while (cdr sub)
          do   
          (push (distance (cadr sub) (car sub)) temp)
          (incf count)
          (when (= count 3) 
            (push (approx-decimals (coerce (average temp) 'float) 2) result)
            (setf count 0)
            (setf temp nil))
          finally return (nreverse result))))

;;;2nd degree dx reduction
(defun 2D-dx2-reduce (lis)    
  (let (result
        (temp (list (super-curve (distance (cadr lis) (car lis)))))
        (count 2))
    (loop for sub on lis
          while (cddr sub)
          do   
          (push (* (super-curve (distance (caddr sub) (car sub))) .75) temp)   ;;; weigh 2-step interval less than 1-step
          (push (super-curve (distance (caddr sub) (cadr sub))) temp)
          (incf count)
          (when (= count 3) 
            (push (approx-decimals (coerce (average temp) 'float) 2) result)
            (setf count 0)
            (setf temp nil))
          finally return (nreverse result))))

;;; 2nd degree dx reduction with isolation between triplets
(defun 2D-dx2-isol-reduce (lis)
  (loop for sub on lis by #'cdddr
        collect
        (average (list
                  (* (super-curve (distance (caddr sub) (car sub))) .75)   ;;; weigh 2-step interval less than 1-step
                  (super-curve (distance (caddr sub) (cadr sub)))
                  (super-curve (distance (cadr sub) (car sub)))))))          


(defun 2D-average (lis)
  (list (average (mapcar #'first lis))
        (average (mapcar #'second lis))))

(defun 2D-avg-reduce (lis)
  (loop for sub on lis by #'cdddr
        collect (2D-average (subseq sub 0 3))))

;;; also works for 1D
(defun 2D-range (lis)
  (list (apply #'max (om::flat lis))
        (apply #'min (om::flat lis))))

(defun 2D-range-reduce (lis)
  (loop for sub on lis by #'cdddr
        collect (2D-range (subseq sub 0 3))))


(defun 2D-da-reduce (lis ops)
  (let ((result (copy-list lis)))
    (loop for op in (reverse ops)
          do (setf result (if (equalp op 'd)
                              ;(2D-dx2-isol-reduce result)  
                              (if (listp (car result))
                                  (2D-dx2-reduce result)
                                (dx2-reduce result))
                            (if (equalp op 'r)
                                (2D-range-reduce result)
                              (if (listp (car result))
                                  (2D-avg-reduce result)
                                (avg-reduce result)))))
          finally return 
          (if (= (length result) 1)
              (car result)
            result))))



(defun corelation-da-reduce (lis ops)
 (da-reduce
  (loop for item in lis
        collect (- (first item) (second item)))
  ops))

(defun 2D-comb-trial (depth iters ops-list &optional goal-list weight-list expt-list)
  (let* ((c (mki 'criterion-list
                 :criteria
                 (mapcan #'(lambda (fun)
                             (mapcar #'(lambda (ops goal weight expt)
                                         (mki 'simple-criterion 
                                              :eval-fun fun   ;; <<-----
                                              :eval-args (list ops)
                                              :goal goal
                                              :weight weight
                                              :exponent expt))
                                     ops-list
                                     (or goal-list (loop repeat (length ops-list) collect 10))
                                     (or weight-list (loop repeat (length ops-list) collect 1))
                                     (or expt-list (loop repeat (length ops-list) collect 1))
                                     ))
                         (list #'2D-da-reduce #'corelation-da-reduce))))
                                                  
         (pop (make-population 'sp-redblue c :len (expt 3 depth))))

    (run pop iters)

    (best pop))
    )









