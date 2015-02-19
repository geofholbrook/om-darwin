(in-package dwn)

;;;;;;;;;;;;;;;;
;;; just for convenience

(om::defmethod! phenotype ((self t))
  :icon 700   
  nil)

(om::defmethod! evaluate ((self t) (crit t) &optional index)
  :icon 704  
  nil)

;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(om::defclas dwn-object () 
  ((attached-data :initform nil)))   ;;; what is 'attached-data'?

(defmethod contents ((self t)) self)




;;; »»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»
;;; »»»»»»»»» SPECIMEN »»»»»»»»»»»»»»»»»»»»»»»»»»
;;; »»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»


(om::defclas specimen (dwn-object)
  ((parent :initform nil)
   (population :initform nil)
   (environment :initform nil)  ;;; used?
   (geno :initform nil)
   (fitness :initform nil)
   (last-mutation :initform nil)))  ;;; might be used for optimization


;;; specialize this for new types of specimen
(defmethod rnd-genotype ((self specimen)) nil)

;;; set genotype to a random setting
(defmethod initialize-instance :after ((self specimen) &rest args)
  (unless (geno self)
    (setf (geno self) (rnd-genotype self))))

;;; _what_ will be evaluated (used?)
(defmethod eval-subject ((self specimen)) (geno self))

;;; convenient for viewing a specimen (used?)
(defmethod contents ((self specimen)) (geno self))




;;; »»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»
;;; »»»»»»»»» CRITERION »»»»»»»»»»»»»»»»»»»»»»»»»
;;; »»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»


(om::defclas criterion (dwn-object)
  ((weight :initform 1.0)
   (exponent :initform 1.0)
   (goal :initform 0.0)
   (tolerance :initform 0.0)
   ))


;;; 0.0 means perfect (higher values mean more errors)
(defmethod evaluate ((self specimen) (crit t) &optional index) 0.0)


(defmethod evaluate :around ((self t) (crit criterion) &optional index)
  (let ((result (call-next-method self 
                                  (or crit (criterion (environment (population self))))
                                  index)))
    (when (fraction-value result)
      (weigh (fraction-value result)
             (weight crit)
             (exponent crit)
             (convert-if-bpf (goal crit) index)
             (tolerance crit)))))
  
(defun weigh (result &optional weight exponent goal tolerance)
  (* (expt (max 0
                (- (abs (- (if (numberp result)
                     result
                     (if result 1 0))
                           (or goal 0)))
                   (or tolerance 0)))
           (or exponent 1))
     (or weight 1)))




;;; »»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»
;;; »»»»»»»»» MUTATION »»»»»»»»»»»»»»»»»»»»»»»»»»
;;; »»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»


                      
(defmethod mutate ((self specimen) &rest args) nil)   ; do nothing

(defmethod mutate :around ((self specimen) &rest args)
  (setf (last-mutation self) nil)
  (call-next-method)
  (when (last-mutation self)
    (setf (fitness self) nil)))



(defmethod crossover ((spec1 specimen) (spec2 specimen)) 
  (values (true-copy spec1) nil))

(defmethod crossover :around ((spec1 specimen) (spec2 specimen)) 
  (multiple-value-bind (result mut-spec) 
      (call-next-method)

    (setf (fitness result) nil)
    (setf (parent result) (list spec1 spec2))
    (setf (last-mutation result) mut-spec)

    result))



(om::defclas mutation (dwn-object)
  ((alteration :initform nil)
   (location :initform nil)))

(defmethod contents ((self mutation))
  (if (location self)
    (cons (location self) (contents (alteration self)))
    (alteration self)))














