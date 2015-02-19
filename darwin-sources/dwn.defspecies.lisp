(in-package dwn)

; phenotypers and specimen makers.

; s-dot creates random specimen, appropriate to the corresponding p-dot

;===============================
;===============================

;;; "CODES"

; a decoder is a list where each element is of the form '(:set a b c) or '(min max) or '(min max divisor)
; or (min max divisor float?) ...  somewhat like the arguments to dwn::mod-to-range
; i.e. '(:set a b c)  or '(0 11) or '((0 0.9) 10 t)

; it can be a nested list, and can be formatted to be passed to expand-lst

; so: '((60 72) (:set :up :down) (3 8) (8* ((1 3)))) 
; could yield '(69 :down (2 1 3 2 2 3 1 1))

; an operon is a group of nucleotides that is understood by one decoder
; a nucleotide is a single element of the raw genotype

;;; defspecies creates a chromosome structure, which should be used to examine a genotype in decoded format
;;; should define an s.<name> function, and a decoder parameter called *<name>-decoder*


(defparameter *default-num-operons* 8)

(om::defclas operon ()
  ((owner))) ;; specimen to which this operon belongs (parent would be confusing in the genetic algorithm context)

(om::defclas specimen ()
  ((operons)   
   (pheno :initform nil)  
   (raw-genotype :initarg :raw :initform nil)
   (decoder)))    

(defgeneric update-geno (self))
(defgeneric phenotype (self))

(defmethod phenotype ((self t)) self) ;; so that code will work in cases where there is no phenotype

(defmethod update ((self specimen))
  (update-geno self)
  (setf (pheno self)
        nil)
  self)

(defmethod mutate ((self specimen))
  (let ((copy (clos::copy-standard-instance self)))
    (mutate! (raw-genotype copy))
    (update copy)))

(defmethod cross ((self specimen) (other specimen))
 (let ((crossed (clos::copy-standard-instance self)))
   (setf (raw-genotype crossed)
         (cross (raw-genotype self)
                (raw-genotype other)))
   (update crossed)))
    

(defun raw+model (raw model)
"make a new specimen that is like the model (same species, same species-slot values) but with new raw-genotype"
  (let ((spec (clos::copy-standard-instance model)))
    (setf (raw-genotype spec) raw)
    (update spec)))

(defmethod population-from-model ((model specimen) (criterion function))
  (loop repeat *capacity*
        collect (let ((spec (raw+model (random-raw-genotype (length (raw-genotype model)))
                                       model)))
                  (list (evaluate spec criterion) spec 0))))

;;; pretty slick!
(defmethod run ((model specimen) (criterion function) (max-generations number) &key (finalizer #'finalize))
  (update model) ;;; make sure pheno is nil, etc.
  (setf *most-recent-result*
        (run (raw-genotype model)
             #'(lambda (raw) (funcall criterion (raw+model raw model)))
             max-generations
             :finalizer #'(lambda (raw) (funcall finalizer (raw+model raw model))))))





(defmethod finalize ((self specimen)) self)
       

;;; how best to encapsuate the various parts of this macro, which is getting too long ... ?
(defmacro defspecies (species-name &body args)

  ;0. parse arguments
  ;1. create specimen class
  ;2. create operon class
  ;3. specialize phenotype method
  ;4. specialize update-geno method   ... updates (structured) genotype to reflect raw genotype
  ;5. initialize-instance: set the decoder based on operon structure, create raw genotype
  ;6. 
  
  (flet ((symbol+ (&rest substrings)
           (intern (string-upcase (apply #'om::string+ substrings))))

         (om-symbol+ (&rest substrings)
           (intern (string-upcase (apply #'om::string+ substrings))
                   'om)))    
    
           ;;; created symbols must be uppercase

    (let* ((namestring (prin1-to-string species-name))
           (operon-initarg 'num-operons)
           species-slots 
           operon-slots 
           (phenotyper-body '(identity self)))

      (loop for arg in args
            with target = :species-slots
            do
            (if (keywordp arg)
                (setf target arg)
              (case target
                (:operon-initarg (setf operon-initarg arg))
                (:phenotyper (setf phenotyper-body arg))
                (:species-slots (push arg species-slots))
                (:operon-slots (push arg operon-slots))))

            finally do
            (setf species-slots (reverse species-slots))
            (setf operon-slots (reverse operon-slots)))

      `(progn

         ;;; create specimen class
         (om::defclas! ,species-name (specimen)

                       ((,operon-initarg :initform ,*default-num-operons*)   ;;; default for number of operons is 8

                      ;species-slots -- custom parameters that do not get mutated
                        ,@(mapcar #'(lambda (slotdef)
                                      (if (atom slotdef)
                                          (list slotdef)
                                        (list (first slotdef) :initform (second slotdef))))
                                  species-slots)))

         ;;; create operon class
         (om::defclas ,(symbol+ namestring "-operon") (operon)
                      ,(mapcar #'(lambda (slotdef) 
                                   (list (first slotdef))) 
                               operon-slots))

         ;;; specialize the phenotype method
         (defmethod phenotype ((self ,species-name))
           (or (pheno self)
               (setf (pheno self)
                     ,phenotyper-body)))

         ;;; specialize the update-geno method    
         (defmethod update-geno ((self ,species-name))
           (setf (operons self)
                 (loop for list-operon in (p.codes (raw-genotype self) (decoder self))
                       collect (mki ',(symbol+ namestring "-operon")
                                    ,@(loop for slot in operon-slots
                                            for k from 0
                                            collect (om::make-keyword (first slot))
                                            collect `(nth ,k list-operon))
                                    :owner self))))

         ;;; create random-specimen maker
         (defmethod initialize-instance ((self ,species-name) &rest rest)
           (call-next-method)

           ;;; set decoder
           (setf (decoder self)
                 (list ,@(loop for slotdef in operon-slots

                               for range = (om::find-keyword :range (cdr slotdef))
                               for card = (om::find-keyword :cardinality (cdr slotdef))

                               collect (if card
                                           (if (atom card)
                                               (create-list card range)

                                             `(:length ,card ,@(create-list (second card) range)))  
                                         ;;; adds the cardinality gene, then the number of genes
                                         ;;; corresponding to upper range of cardinality 

                                         range))))

           (unless (raw-genotype self)
             (setf (raw-genotype self) 
                   (s.codes (,operon-initarg self) (decoder self))))
           
           (update self))

         ;; make om creation method

         (defmethod! ,(om-symbol+ "make-" namestring) ((,operon-initarg number))
           :initvals '(,*default-num-operons*)
           :icon 703
           (make-instance ',species-name ,(om::make-keyword operon-initarg) ,operon-initarg))

           ))))

;;; for fun more than convenience:

(defmacro maperons (spec &body body)
  `(mapcar #'(lambda (op)
               ,@body)
           (operons ,spec)))




(defun nucleotides-per-operon (decoder)
  (if (equalp (first decoder) :length)
      (nucleotides-per-operon (cdr decoder))
    (loop for d in (om::expand-lst decoder)
          sum (if (some #'listp d)
                  (nucleotides-per-operon d)
                1))))


(defun s.codes (num-operons decoder)
  (loop repeat (* num-operons 
                  (nucleotides-per-operon decoder))
        collect (rrnd *gene-range*)))


(defmacro popn (symbol n)
  (let ((rep (gensym)))
  `(let (,rep)
     (loop repeat ,n
           do (push (pop ,symbol) ,rep)
           finally return (nreverse ,rep)))))



(defun mtr (num &rest args)
  (if (equalp (car args) :set)
      (apply 'mod-to-range num `((set ,@(cdr args))))
    (apply 'mod-to-range num (list (list (first args) (second args)) (or (third args) 1) (fourth args)))))


;;; holy confusing
(defun p.codes (list decoder)
  (let ((decoder (om::expand-lst decoder)))
    (labels ((decode (g sub)
               (loop for elt in (if (equal (first sub) :length)
                                    (subseq sub 2 (+ 2 (apply 'mtr (pop g) (second sub))))
                                  sub)
                     while g
                     collect
                     (if (or (equalp (car elt) :length) 
                             (listp (car elt)))
                         (decode (popn g (nucleotides-per-operon elt)) elt)
                       (apply 'mtr (pop g) elt)))))

                     
      (loop for group in (group-list list 
                                     (nucleotides-per-operon decoder) :linear)
            collect (decode group decoder)))))


