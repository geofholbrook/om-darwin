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


(defmacro defspecies (species-name &rest args)
  
  (flet ((symbol+ (&rest substrings)
           (intern (string-upcase (apply #'om::string+ substrings)))))    ;;; created symbols must be uppercase.

    (let* ((namestring (prin1-to-string species-name))
           (decoder-symbol (symbol+ "*" namestring "-decoder*"))
           (phenotyper-symbol (symbol+ namestring "-phenotyper"))
           (species-slots '())
           (operon-slots '())
           (prefix (symbol+ namestring "-"))  ;; default
           phenotyper-body)

      (let ((target nil))
        (dolist (arg args)
          (if (keywordp arg)
              (setf target arg)
            (case target
              (:prefix (setf prefix arg))
              (:phenotyper (setf phenotyper-body arg))
              (:species-slots (push arg species-slots))
              (:operon-slots (push arg operon-slots)))))
        (setf species-slots (reverse species-slots))
        (setf operon-slots (reverse operon-slots)))

      `(progn

         ;;; create specimen structure
         (defstruct (,species-name (:conc-name ,prefix))

           ,@species-slots       ; species-specific slots
           (raw)                 ; default slots
           (operons)
           (phenotyper-function)
           (phenotype))

         ;;; create operon structure
         (defstruct (,(symbol+ (prin1-to-string prefix) "operon") (:conc-name ,prefix))
           (parent)
           ,@(mapcar #'(lambda (slotdef) 
                         (list (first slotdef))) 
                     operon-slots))

         ;;; create decoder
         (defparameter ,decoder-symbol
           ',(loop for slotdef in operon-slots
                   for range = (om::find-keyword :range (cdr slotdef))
                   for card = (om::find-keyword :cardinality (cdr slotdef))
                   collect (if card
                               (if (atom card)
                                   (create-list card range)
                                   `(:length ,card ,@(create-list (second card) range)))
                               range)))

         ;;; define the phenotype function
         (defun ,phenotyper-symbol (self)
           ,phenotyper-body)

         ;;; create random-specimen maker
         (defun ,(symbol+ "random-" namestring)
                (&key ,@species-slots)

           (let ((raw (s.codes num-operons ,decoder-symbol)))
           
             (let ((spec (,(symbol+ "make-" namestring)
                          ,@(mapcan (lambda (slotdef)
                                      (list (om::make-keyword (first slotdef)) (first slotdef)))
                                    species-slots)
                          :raw raw
                          :operons (let ((trans (mat-trans (p.codes raw ,decoder-symbol))))
                                     (mapcar #'(lambda ,(mapcar 'first operon-slots)
                                                 (,(symbol+ "make-" (prin1-to-string prefix) "operon")
                                                  ,@(mapcan (lambda (slotdef)
                                                              (list (om::make-keyword (first slotdef)) (first slotdef)))
                                                            operon-slots)))
                                             ,@(loop for k from 0 to (1- (length operon-slots))
                                                     collect `(nth ,k trans))))
                          :phenotype nil
                          :phenotyper-function ',phenotyper-symbol)))

               (dolist (op (,(symbol+ prefix "operons") spec))
                 (setf (,(symbol+ prefix "parent") op) spec))))))
         )))


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
    `(let ((,rep '()))
       (loop repeat ,n
             do (push (pop ,symbol) ,rep))
       (nreverse ,rep))))



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


