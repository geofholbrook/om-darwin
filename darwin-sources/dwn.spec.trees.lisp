(in-package dwn)

;;; GA TREES
;;;;;;;;;;;;;;;;

;;; all possible compositions (ordered partitions) of an integer 
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




(defun make-tree (operons max-div)

  (labels ((make-branch (num max-depth gene-position)
             (let ((op (nth gene-position operons)))
               (if (or (null op)
                       (= (division op) 1) 
                       (= max-depth 0))
                   num
                 (list num (loop for sub in (additive-from-bits 
                                             (last-n (binary-expansion (on-off op) (1- (division op))) 
                                                     (1- (division op))))
                                 sum sub into branch-pos
                                 collect (make-branch sub 
                                                      (1- max-depth)
                                                      (+ (* gene-position max-div)
                                                         branch-pos))))))))

    (make-branch 1 2 0)))
 



(defspecies ga-tree ()

  :species-slots 
  (div-range '(1 5))
  (time-sig '(4 4))

  :operon-slots
  (division :range (div-range self))
  (on-off :range (list 1 (expt 2 (1- (second (div-range self))))))    
        ;;;; 2^(n-1) possibilities, barring rests or ties!
        ;;;; 0 is always useless anyway, results in expansion (1 0 0 ...) (no division)

  :phenotyper
  `(om::? ((,(time-sig self) ,(let ((main (make-tree (operons self) (second (div-range self)))))
                                (if (atom main) 
                                    (list main)
                                  (second main)))))))

(defmethod finalize ((self ga-tree)) 
  (mki 'om::voice :tree (phenotype self)))

(defun concat-trees (t1 t2)
  (list 'om::? (append (second t1)
                       (second t2))))

(defmethod species-concatenator ((self ga-tree)) 'concat-trees)

;;;;; 













