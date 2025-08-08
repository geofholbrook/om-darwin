(in-package darwin)

(defvar *defcrit-list* nil)

#|
(solve (chordseq 10 x 3 x (48 72))
  (pheno
   (note (melodic (2 4) abs)
         (pitch class (set 1 3 4 6 7 9 10))
         (no-cross-overs))
   (interval (not (harmonic 0 class))
             (50% (harmonic 7)))
   (chord (acoustic-dissonance (0.5 nil)))))

|#


any list that begins with a number is a goal range

any list that begins with the symbol "set" is a goal set


;;; vs. 50% of the intervals are a perfect fifth

;;; (interval (harmonic class 7)))

;;; (  WHAT to evaluate    HOW to evaluate     GOAL frequency (optional, default 100%) )


(pheno
 (chord 0.5 (interval >0 (harmonic class 7))
        ))))

(defcrit pheno (&rest args)
  `(mki 'criterion-list
        :pre-fun #'phenotype
        :criteria ,@args))

(defcrit chord (freq sub)
  `(mki 'stepper :goal ,freq :sub-crit
        ,(read-criterion sub)))

#|
(pheno
 (50% (interval (harmonic class 7))))

(pheno
 (50% (pitch class 1))
 (50% (pitch class (set 3 5))))

(pheno
 (pitch class ((50% 1) (50% (set 3 5)))))

|#



(defmacro defcrit (name args &body body)
  `(progn
     (push ,name *defcrit-list*)
     (defmacro ,(read-from-string (string-append "crit-" (prin1-to-string name)))
               ,args
       ,@body)))

#|
(defmacro solve (&rest args)
  (let ((criteria (first args))
        (gestalt (third args)))
|#  
    

(defmacro evalcrit (form)
  `(,(read-from-string (string-append "crit-" (prin1-to-string (car form))))
    ,@(cdr form)))

(defun add-crit- 

(defmacro solve (&body body)
  `(progn
     ,(loop for form in body
            collect
            (if (assoc (car form) *defcrit-list*)
                (cons (read-from-string (string-append "crit-" (prin1-to-string (car form))))
                      (cdr form))
              form)

                

(defmacro define-basic-criterion (name)
  `(defcrit ,name (&rest args)
     (let ((symbols (remove-if-not #'symbolp args))
           (value (find-if #'(lambda (elt) (or (numberp elt) (listp elt))) args))
           (fun (function ,name)))   
       `(mki 'basic-criterion :eval-fun ,fun
             :comparison-fun 
             #'(lambda (x goal)
                 (basic-compare (serial-apply 
                                 x
                                 ,@(if (member 'abs symbols) (list #'abs))
                                 ,@(if (member 'class symbols) (list #'(lambda (n) (mod n 12)))))
                                 goal))
             :goal ',value))))

(defcrit no-cross-overs ()
  `(mki 'basic-criterion 
        :eval-fun #'harmonic
        :goal (0 nil)))

(define-basic-criterion melodic)
(define-basic-criterion harmonic)

(defun serial-apply (thing &rest funs)
  (loop for fun in funs
        with result = thing
        do (setf result (funcall fun result))
        return result))


                          
                         

