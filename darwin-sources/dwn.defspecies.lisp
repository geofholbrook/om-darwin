(in-package dwn)

#|  ;;; USAGE ;;;;

  (defspecies <name-of-species>
              
              [ :operon-initarg <initarg> ]  ;;; this is the keyword used to specify the number of operons for a (sub)species
                                             ;;; defaults to :num-operons.   number of operons defaults to 8.

              :species-slots 
              ;;; these define characteristics of a subspecies, for example the length or range of a melody ... does not mutate

              { (<name of slot> <default-value>) }*

              :specimen-slots

              ;;; not implemented: slots that mutate but are not part of the repeating operon

              { (<name-of-slot> :range <range> [ :cardinality <card-range> ]) }*

              :operon-slots
              ;;; these are the properties of a single operon (the repeating data structure), which are mutated
              ;;; keywords are :range (a decoder understood by mod-to-range) and :cardinality, for a varying number of values
              ;;; the same range

              { (<name-of-slot> :range <range> [ :cardinality <card-range> ]) }*

              

              :phenotyper
              ;;; code for creating a phenotype from the genotype (not the raw genotype)
              ;;; implied is the first line, (defmethod phenotyper ((self <name-of-species)))
              ;;; good practice is to output the "arrange" data type, in some cases this is assumed.

              <phenotyper-body>
  
  )

;;; defspecies  should define an s.<name> function, and a decoder parameter called *<name>-decoder*

|#

(defparameter *default-num-operons* 8)

;;; the species metaclass (initform of these slots have no effect ... why?)
;;; gonna have to change this, or change defspecies anyway ...
(defclass species (om::omstandardclass) 
  ((operon-initarg :initform 'num-operons :initarg :operon-initarg :accessor operon-initarg)
   (species-slots :initform nil :initarg :species-slots :accessor species-slots)
   (operon-slots :initform nil :initarg :operon-slots :accessor operon-slots)))


(om::defclas specimen ()
  ((operons :initform nil)   
   (pheno :initform nil)  
   (raw-genotype :initarg :raw :initform nil)
   (decoder :initform nil))
  (:metaclass species))    

(setf (operon-initarg (find-class 'specimen)) 'num-operons)

(om::defclas operon ()
  ((owner))) ;; specimen to which this operon belongs (parent would be confusing in the genetic algorithm context)


(defmethod update-geno ((self specimen)) nil)
(defmethod phenotype ((self specimen)) self)  ;;; t?      ;; so that code will work in cases where there is no phenotype

(defmethod update ((self specimen))
  (update-geno self)
  (setf (pheno self)
        nil)
  self)


(defmethod mutate ((self specimen) &optional ga-params)
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
  (let ((spec (eval (om::omng-copy model))))
    (setf (raw-genotype spec) raw)
    (update spec)))

(defun randomize-specimen (spec &optional ga-params)
  (raw+model (loop repeat (length (raw-genotype spec)) 
                   collect (rrnd (get-param :gene-range ga-params)))
             spec))

(defmethod population-from-model ((model specimen) (criterion function) &optional ga-params)
  (loop repeat *capacity*
        collect (let ((spec (randomize-specimen model ga-params)))
                  (list (evaluate spec criterion) spec 0))))

;;; pretty slick!
(defmethod run ((model specimen) (criterion function) (max-generations number) &key (finalizer #'finalize))
  (update model) ;;; make sure pheno is nil, etc.
  (setf *most-recent-result*
        (run (raw-genotype model)
             #'(lambda (raw) (funcall criterion (raw+model raw model)))
             max-generations
             :finalizer #'(lambda (raw) (funcall finalizer (raw+model raw model))))))



(defmethod finalizer ((self specimen)) #'identity)

;; convert to viewing / auditioning format, not for fitenss functioning
(defmethod finalize ((self specimen)) 
  (funcall (finalizer self)
           (phenotype self)))
     

(defun combine-slotdefs (direct inherited)
  (append (loop for slot in inherited
                collect (or (find slot direct :test #'(lambda (a b) (equalp (car a) (car b)))) 
                            slot))
          (loop for slot in direct
                unless (find slot inherited :test #'(lambda (a b) (equalp (car a) (car b))))
                collect slot)))



;;; how best to encapsuate the various parts of this macro, which is getting too long ... ?
(defmacro defspecies (species-name inheritance &body args)

  ;0. parse arguments
  ;1. create specimen class
  ;2. create operon class
  ;3. specialize phenotype method
  ;4. specialize update-geno method   ... updates (structured) genotype to reflect raw genotype
  ;5. initialize-instance: set the decoder based on operon structure, create raw genotype
  ;6. 
  
  (print (om::string+ "expanding " (prin1-to-string species-name)))

  (flet ((symbol+ (&rest substrings)
           (intern (string-upcase (apply #'om::string+ substrings))))

         (om-symbol+ (&rest substrings)
           (intern (string-upcase (apply #'om::string+ substrings))
                   'om)))    
    
           ;;; created symbols must be uppercase

    (let* ((namestring (prin1-to-string species-name))
           (superclass (if inheritance (find-class (car inheritance))
                         (find-class 'specimen)))

           (operon-initarg (operon-initarg superclass))
           direct-species-slots
           direct-operon-slots
           species-slots
           operon-slots
           phenotyper-body)

      (loop for arg in args
            with target = :species-slots
            do
            (if (keywordp arg)
                (setf target arg)
              (case target
                (:operon-initarg (setf operon-initarg arg))
                (:phenotyper (setf phenotyper-body arg))
                (:species-slots (push arg direct-species-slots))
                (:operon-slots (push arg direct-operon-slots))))

            finally do
            (setf direct-species-slots (reverse direct-species-slots))
            (setf direct-operon-slots (reverse direct-operon-slots)))

      

      (setf species-slots (combine-slotdefs direct-species-slots (species-slots superclass)))
      (setf operon-slots (combine-slotdefs direct-operon-slots (operon-slots superclass)))

      `(let ((bogus (print (om::string+ "evaluating " ,(prin1-to-string species-name))))
             (species (om::defclas! ,species-name ,(or inheritance '(specimen))

                                    ((,operon-initarg :initform ,*default-num-operons*)   ;;; default for number of operons is 8

                      ;species-slots -- custom parameters that do not get mutated
                                     ,@(mapcar #'(lambda (slotdef)
                                                   (if (atom slotdef)
                                                       (list slotdef)
                                                     (list (first slotdef) :initform (second slotdef))))
                                               direct-species-slots))

                                    (:metaclass species))))

         ;;; create operon class
         (om::defclas ,(symbol+ namestring "-operon") 
                      ,(if (and inheritance (not (equalp (car inheritance) 'specimen)))
                           (list (symbol+ (prin1-to-string (car inheritance)) "-operon"))
                         '(operon))
                      ,(mapcar #'(lambda (slotdef) 
                                   (list (first slotdef))) 
                               direct-operon-slots))

         ;;; set class slots
         (setf (operon-initarg species) ',operon-initarg)
         (setf (species-slots species) ',species-slots)
         (setf (operon-slots species) ',operon-slots)

         ;;; specialize the phenotype method (if given)
         ,@(when phenotyper-body
             `((defmethod phenotype ((self ,species-name))
                      (or (pheno self)
                          (setf (pheno self)
                                ,phenotyper-body)))))

         ;;; specialize the update-geno method    
         (defmethod update-geno ((self ,species-name))
           (setf (operons self)
                 (loop for list-operon in (p.codes (raw-genotype self) (decoder self))
                       collect (mki ',(symbol+ namestring "-operon")
                                    ,@(loop for slot in operon-slots
                                            for k from 0
                                            collect (make-keyword (first slot))
                                            collect `(nth ,k list-operon))
                                    :owner self))))

         ;;; create random-specimen maker
         (defmethod initialize-instance ((self ,species-name) &rest rest)
           (apply #'shared-initialize self t rest)

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

         (defmethod! ,(om-symbol+ "make-" namestring) 
                     ((,operon-initarg number)
                      ,@(mapcar #'(lambda (slotdef) (list (car slotdef) t))
                                species-slots))
           :initvals '(,*default-num-operons*
                       ,@(mapcar #'(lambda (slotdef) (if (listp (cadr slotdef))
                                                         (cadadr slotdef)
                                                       (cadr slotdef)))  ;; get rid of a quote  
                                 species-slots))
           :icon 703
           (make-instance ',species-name 
                          ,(make-keyword operon-initarg) ,operon-initarg
                          ,@(loop for slotdef in species-slots
                                  append (list (make-keyword (prin1-to-string (car slotdef)))
                                               (car slotdef)))))
         ;;;;
                                               
           ))))

;;; for fun more than convenience:

(defmacro maperons (spec &body body)
  `(mapcar #'(lambda (op)
               ,@body)
           (operons ,spec)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;===============================
;===============================

;;; "CODES"     ;;; maybe this should be in a separate file? if it is useful without defspecies ...

; a decoder is a list where each element is of the form '(:set a b c) or '(min max) or '(min max divisor)
; or (min max divisor float?) ...  somewhat like the arguments to dwn::mod-to-range
; i.e. '(:set a b c)  or '(0 11) or '((0 0.9) 10 t)

; it can be a nested list, and can be formatted to be passed to expand-lst

; so: '((60 72) (:set :up :down) (3 8) (8* ((1 3)))) 
; could yield '(69 :down (2 1 3 2 2 3 1 1))

; an operon is a group of nucleotides that is understood by one decoder
; a nucleotide is a single element of the raw genotype

; (s-dot creates random specimen, appropriate to the corresponding p-dot)

(defun nucleotides-per-operon (decoder)
  (if (equalp (first decoder) :length)
      (nucleotides-per-operon (cdr decoder))
    (loop for d in (om::expand-lst decoder)
          sum (if (some #'(lambda (elt) (and elt
                                             (listp elt)))  ;; consider nil an atom for this purpose
                        d)
                  (nucleotides-per-operon d)
                1))))


(defun s.codes (num-operons decoder)
  (loop repeat (* num-operons 
                  (nucleotides-per-operon (or decoder (list (get-param :gene-range)))))
        collect (rrnd (get-param :gene-range))))


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
  (let ((decoder (or (om::expand-lst decoder) (list (get-param :gene-range)))))
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


