(in-package dwn)



;;; all possible subdivisions of an integer
(defun split (n)
  (or (loop for k from 1 to n
            append (loop for next in (split (- n k))
                         collect (cons k next)))
      '(nil)))


;;; this also works:
(defun split (n)
  (if (> n 1)
      (loop for next in (split (1- n))
            append (list (cons 1 next)
                         (cons (1+ (car next)) (cdr next))))
    '((1))))

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




(defun make-tree (operons)
  (labels ((make-branch (num max-depth)
             (let ((op (pop operons)))
               ;(print (list num (division op) (binary-expansion (on-off op) (1- (division op))) max-depth))
               (if (or (null op)
                       (= (division op) 1) 
                       (= max-depth 0))
                   num
                 (list num (loop for sub in (additive-from-bits 
                                             (last-n (binary-expansion (on-off op) (1- (division op))) 
                                                     (1- (division op))))
                                 collect (make-branch sub (1- max-depth))))))))
    (make-branch 1 2)))
  

  
  


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
  `(? ((,(time-sig self) ,(let ((main (make-tree (operons self))))
                            (if (atom main) 
                                (list main)
                              (second main)))))))









