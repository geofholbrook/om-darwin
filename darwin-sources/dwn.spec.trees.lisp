(in-package dwn)

;;; GA TREES
;;;;;;;;;;;;;;;;

;;; all possible compositions (ordered partitions) of an integer    (not actually used?)
(defun all-compositions (n)
  (or (loop for k from 1 to n
            append (loop for next in (all-compositions (- n k))
                         collect (cons k next)))
      '(nil)))

;;; this also works:
(defun all-compositions (n)
  (if (> n 1)
      (loop for next in (all-compositions (1- n))
            append (list (cons 1 next)
                         (cons (1+ (car next)) (cdr next))))
    '((1))))

;;;;;;;;;;;;;;;;;;;;



(defun pad-with-zeroes (n bits)
  (append (create-list (max 0 (- bits (length n))) 0)
          n))

;;; change min-bits to num-bits
(defun binary-expansion (num &optional (min-bits 1))
  (labels ((rec (n)
               (when (> n 0)
                 (multiple-value-bind (quotient remainder) (floor n 2)
                   (append (rec quotient) (list remainder))))))
    (pad-with-zeroes (rec num) min-bits)))


(defun additive-from-bits (bits)
  (loop with result = '(1)
        for bit in bits
        do
        (setf result
              (if (= bit 1)
                  `(,@result 1)
                `(,@(butlast result) ,(1+ (last-elem result)))))
        finally return result))


(defspecies ga-simple-tree ()

  :species-slots 
  (div-range '(1 5))
  (time-sig '(4 4))

  :operon-slots
  (division :range (div-range self))
  (on-off :range (list 0 (1- (expt 2 (1- (second (div-range self)))))))    
  ;;;; 2^(n-1) possibilities, barring rests or ties!
  ;;;; 0 is always useless anyway, results in expansion (1 0 0 ...) (no division)

  :phenotyper
  (om::reducetree `(om::? ((,(time-sig self) ,(let ((main (make-tree (operons self) (second (div-range self)))))
                                                (if (atom main) 
                                                    (list main)
                                                  (second main))))))))


;;; this has to appear after defspecies, because it defines the class
(defmethod branch-from-operon ((op ga-simple-tree-operon))
  (additive-from-bits 
   (last-n (binary-expansion (on-off op) (1- (division op))) 
           (1- (division op)))))


(defun make-tree (operons max-div)
  (labels ((make-branch (num max-depth gene-position)
             (let ((op (nth gene-position operons)))
               (if (or (null op)
                       (= (division op) 1)   ;;; no division
                       (< num 0)             ;;; num represents a rest (so, no division!)
                       (= max-depth 0)
                       )
                   num
                 (list num (loop for sub in (branch-from-operon op)
                                 sum (floor (abs sub)) into branch-pos      ;;; remove rest/tie data
                                 collect (make-branch sub 
                                                      (1- max-depth)
                                                      (+ (* gene-position max-div)
                                                         branch-pos))))))))

    (make-branch 1 2 0)))
 

(defmethod finalizer ((self ga-simple-tree))
  #'(lambda (tree)
      (mki 'om::voice :tree tree)))

(defun concat-trees (t1 t2)
  (list 'om::? (append (second t1)
                       (second t2))))

(defmethod species-concatenator ((self ga-simple-tree)) 'concat-trees)



;;;;; 

(defspecies ga-tree (ga-simple-tree)

  :species-slots
  (allow-rests t)
  (allow-ties t)

  :operon-slots
  (rests :range (list 0 (1- (expt 2 (second (div-range self))))))  ;;; on-off should be this way also
  (tied :range '(:set nil t)))

#|
(defmethod correct-branch ((op ga-tree-operon) branch)
  ; problem remains: tied note when a rest precedes it ... would have to handled outside this function. voice seems to deal with it anyway.

  (let ((
  
  (let ((frst (car branch)))
    (when (= (length branch) 1) 
      (setf frst (/ frst (abs frst))))   ;;; preserves sign, type

    (when (and (floatp frst) (< frst 0)) 
      (setf frst (abs frst)))            ;;; convert tied rest to tied note



             (if (< (first branch 0))
          (if (floatp
|#
          

(defmethod branch-from-operon ((op ga-tree-operon))
  (let ((rests? (allow-rests (owner op)))
        (ties? (allow-ties (owner op))))
    (if (not (or rests? ties?))
        (call-next-method)
      (loop with rests = (and rests?
                              (om- (om* (last-n (binary-expansion (rests op) (division op)) 
                                                (division op)) 2) 1))  ;;; bits become -1 and 1
            with pos = 0
            for part in (additive-from-bits 
                         (last-n (binary-expansion (on-off op) (1- (division op))) 
                                 (1- (division op))))
        
            collect (if rests? (* (nth pos rests) part) part)
            into parts

            do (incf pos part) ;ex (1 3 1) --> (0 1 4)

            finally return (cons (if (and ties? (tied op))
                                                        (float (car parts))   ;haha
                                                      (car parts))
                                                    (cdr parts))))))
       

        

            
        

        

        













