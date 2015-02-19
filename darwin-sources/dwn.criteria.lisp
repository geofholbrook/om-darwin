



(in-package dwn)

;;; »»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»
;;; »»»»»»»»»» simple-criterion »»»»»»»»»»»»»»»»»
;;; »»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»

(om::defclas simple-criterion (criterion)
  ((eval-fun :initform #'(lambda (s &optional index) 0.0))))
                                 ;important: all eval-funs must allow the optional argument "index"

(defmethod evaluate ((self t) (crit simple-criterion) &optional index)
  (funcall (eval-fun crit)
           self
           index))

(defmethod evaluate ((self specimen) (crit simple-criterion) &optional index)
  (funcall (eval-fun crit)
           (eval-subject self)
           index))

;;; »»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»
;;; »»»»»»»»»» custom-phenotyping »»»»»»»»»»»»»»»
;;; »»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»


;;; calls an alternate phenotyper _instead_ of the specimen phenotyper
(om::defclas alt-pheno-criterion (criterion) 
  ((alt-phenotyper :initarg :pheno :initform #'identity)
   (sub-criterion :initarg :crit)))

(defmethod evaluate ((self specimen) (crit alt-pheno-criterion) &optional index)
  (evaluate (funcall (alt-phenotyper crit) self) (sub-criterion crit) index))
;;; note: a specialized evaluate method may call evaluate again on something else, 
;;; provided that the goal is zero (which it is by default)
;;; because the :around method is called twice in this case, recursively


;;; calls an alternate phenotyper _after_ the specimen phenotyper
(om::defclas post-pheno-criterion (alt-pheno-criterion) ())
  
(defmethod evaluate ((self list) (crit post-pheno-criterion) &optional index)
  (evaluate (funcall (alt-phenotyper crit) self) (sub-criterion crit) index))

(defmethod evaluate ((self sp-pheno) (crit post-pheno-criterion) &optional index)
  (evaluate (funcall (alt-phenotyper crit) (phenotype self)) (sub-criterion crit) index))

(defmethod evaluate ((self specimen) (crit post-pheno-criterion) &optional index)
  (evaluate (funcall (alt-phenotyper crit) self) (sub-criterion crit) index))


;;; »»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»
;;; »»»»»»»»» list-criterion »»»»»»»»»»»»»»»»»»»»
;;; »»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»

(defvar *c-list-verbose* nil)

(defmethod c-list-verbose ((on-off t)) 
  (setf *c-list-verbose* on-off))

(om::defclas list-criterion (criterion)
  ((criteria)
   (name :initform nil)))


(defmethod evaluate ((self t) (crit list-criterion) &optional index)
  (let ((results (loop for c in (criteria crit)
                       for result = (evaluate self c index)
                       if (fraction-value result) collect (fraction-value result))))
    (when (and (name crit) *c-list-verbose*)
      (print (format nil "c-list ~D : ~D" (name crit) results)))
    (apply '+ results)))

;;; why was this specialized? it's the same code
(defmethod evaluate ((self specimen) (crit list-criterion) &optional index)
  (let ((results (loop for c in (criteria crit)
                       for result = (evaluate self c index)
                       if (fraction-value result) collect (fraction-value result))))
    (when (and (name crit) *c-list-verbose*)
      (print (format nil "c-list ~D : ~D" (name crit) results)))
    (apply '+ results)))




;;; »»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»
;;; »»»»»»»»» specimen-criterion »»»»»»»»»»»»»»»»
;;; »»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»


(om::defclas spec-criterion (simple-criterion)
         ())

(defmethod evaluate ((self specimen) (crit spec-criterion) &optional index)
  (funcall (eval-fun crit)
           self
           index))






