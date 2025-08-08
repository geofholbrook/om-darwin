(in-package dwn)



(om::defclass! om-specimen (sp-pheno sp-list)
  ((approx :initform 2 :initarg :approx :accessor approx))
  (:icon 703))



;;; main method ;;;;;;;;

(om::defmethod! evolute ((model t) (crit function) (generations number) &optional (id 0))
  :initvals (list (om::mki 'om-specimen) (om::mki 'criterion) 1000 0)
  :icon 701
  (let ((result (if (= generations 0)
                    model
                  (d::run (d::population-from-model model crit)
                       generations))))
    ;(save-genotype id (geno result))
    result))

;;;;;;;;;;;


;;; just bypass all the fancy stuff 
(defmethod d::evaluate ((self om-specimen) (crit function) &optional index)
  (let ((result (funcall crit
                         self)))
    (if (numberp result)
        (float result)
      (if result 1 0))))



;;; even less fancy
(defmethod d::evaluate ((self list) (crit function) &optional index)
  (funcall crit self))

(defmethod d::mutate ((self list) &rest args)
  (incf (nth (rrnd 0 (1- (length self))) self)
        (* (rrnd 1 3) (if (= (rrnd 1 2) 1) 1 -1))))
    
  
(defmethod d::geno ((specimen list)) (cdr specimen))

;;; and a quick way of combining functions
(om::defmethod! add (&rest criteria)
  :icon 702

  (lambda (spec)
    (apply '+ (mapcar #'(lambda (crit)
                          (evaluate spec crit))
                      (remove-if #'null criteria)))))







(defun fittest () *best*)


(defmethod om::omNG-copy ((self om-specimen))
  `(let ((new ,(call-next-method)))
     (setf (geno new) ',(geno self))
     (setf (phenotyper new) ',(phenotyper self))
     (setf (pheno-args new) ',(pheno-args self))
     (setf (pheno new) ',(pheno self))
     new))

(defmethod om::omNG-save ((self om-specimen) &optional (values? nil))
  `(let ((new ,(call-next-method)))
     (setf (geno new) ',(geno self))
     (setf (phenotyper new) ',(phenotyper self))
     (setf (pheno-args new) ',(pheno-args self))
     (setf (pheno new) ',(pheno self))
     new))




;;; ���������������������������������������������
;;; ���������� micronum / keynum  ���������������
;;; ���������������������������������������������


;this is the "Finale-style" coding for microtones in integers (where the origin is 60)

(defun keynum->micronum (key approx)
  (+ 60 (* (- key 60)
           approx 
           1/2)))

(defun micronum->keynum (micro approx)
  (float (+ 60 (/ (- micro 60)
                  approx 1/2))))

(defun encode-for-micro (lis approx)
   (funcall-to-tree #'(lambda (elt)
                       (keynum->micronum elt approx))
                   lis))

(defun decode-for-micro (lis approx)
  (funcall-to-tree #'(lambda (elt)
                       (micronum->keynum elt approx))
                   lis))




;;; �����������������������������������������������������������������������
;;; ���������� mutation-correction for "dx" type specimens  ���������������
;;; �����������������������������������������������������������������������


(defun dx-mutation-correct (result mut-spec range) ;destructive
   (if (listp mut-spec)
       result
     (let ((loc (location mut-spec))
           (alt (alteration mut-spec)))
       (when (and (< loc (1- (length result)))
                  (withinp (- (nth (1+ loc) result)
                              alt)
                           range))
         (subs-posn result (1+ loc)(- (nth (1+ loc) result)
                                       alt)))
       result)))

;;; this operation works fine on encoded values (for example, a gene with a 
;;; range of '0-100 representing a pithc from 60 to 72) as long as mod-to-range
;;; is used to decode it, rather than something like om-range


(om::defclas dx-pheno-mixin ()
  ((special-mutation-frequency :initform 50)))

(defmethod mutate :after ((self dx-pheno-mixin) &rest args)
  (if (< (random 100) 
         (special-mutation-frequency self))
      (loop for mut in (last-mutation self)
            do
            (dx-mutation-correct (geno self) mut (range self)))))



;;; ���������������������������������������������
;;; ���������� om specimen types  ���������������
;;; ���������������������������������������������


(defclass sp-chords (om-specimen) 
  ((cardinality :initform 2 :initarg :cardinality :accessor cardinality)))

(defmethod phenotype ((self sp-chords))
  (group-list (decode-for-micro (geno self) (approx self)) 
              (cardinality self) :linear))

(om::defmethod! s-Chords ((len number) (card number) (range list) &key (approx 2))
  :icon 703 :initvals '(10 2 (60 72) 2)
  (om::mki 'sp-chords :len (* len card) :range (encode-for-micro range approx) :cardinality card :approx approx))


;;;;;;;;;;;;;;

(om::defmethod! s-Melody ((len number) (range list) &key (approx 2))
  :icon 703 :initvals '(10 (60 72))
  (s-Chords len 1 range approx))

;;;;;;;;;;;;;;
;;;;;;;;;;;;;;


(om::defclas sp-dx-chords (sp-chords dx-pheno-mixin)
  ((start-range :initform '(60 72))
   (interval-range :initform '(-2 2)))
  (:default-initargs
   :range '(0 100)))

(om::defmethod! s-dx-chords ((len number) (card number) (start-range list) (interval-range list) &key (approx 2))
  :icon 703 :initvals '(10 2 (60 72) (-2 2) 2)
  (om::mki 'sp-dx-chords :len len :cardinality card :start-range start-range :interval-range interval-range :approx approx))

(defmethod phenotype ((self sp-dx-chords))
  (let ((grouped (group-list (geno self) (cardinality self) :linear))
        result)
    (push (mod-to-range (car grouped) (start-range self) (/ (approx self) 2)) result)
    (loop for intervals in (cdr grouped) 
          do 
          (push (om::om+ (car result) 
                         (mod-to-range intervals (interval-range self) (/ (approx self) 2))) 
                result))
    (funcall-to-tree 'float (nreverse result))))

;;;;;;;;;;;;;;

(om::defmethod! s-dx-melody ((len number) (start-range list) (interval-range list) &key (approx 2))
  :icon 703 :initvals '(10 (60 72) (-2 2) 2)
  (s-dx-chords len 1 start-range interval-range :approx approx))

;;;;;;;;;;;;;;
;;;;;;;;;;;;;;    

(om::defclas sp-codes (sp-chords)
  ((decoders)))

;a decoder is either a range or a set. e.g. (60 72) or (set 1/16 1/8 1/4)
;for a range, the third element on are extra arguments to mod-to-range (e.g. for quartertones)

(om::defmethod! s-codes ((len number) (decoders list) &key class)
  :icon 703
    (om::mki (or class 'sp-codes) 
         :len (* len (length decoders))
         :range (list 0 (1- (loop for d in decoders
                                  maximize (if (equalp (car d) 'set)
                                               (length (cdr d))
                                             (* (1+ (- (second d) (first d))) 
                                                (or (third d) 1))))))
         :cardinality (length decoders)
         :decoders decoders))

(defmethod sp-codes-phenotyper ((self sp-codes))
  (loop for group in (group-list (geno self) 
                                  (cardinality self) :linear)
         collect (loop for num in group
                       for d in (decoders self)
                       collect (apply #'mod-to-range
                                      (if (equalp (car d) 'set)
                                          (list num d)
                                        `(,num ,(first-n d 2) ,@(nthcdr 2 d)))))))

(defmethod phenotype ((self sp-codes)) 
   (sp-codes-phenotyper self))
                       



(om::defmethod! s-MapMelody ((len number) (pitches list))
  :icon 703 :initvals '(10 (60 62 64 65 67 69 71 72))
  (om::mki 'om-specimen
       :len len
       :range `(0 ,(1- (length pitches)))
       :phenotyper #'(lambda (s)
                       (loop for g in (geno s) collect (list (nth g pitches))))))

(om::defmethod! s-MapMelody ((len number) (pitches om::score-element))
  :icon 703 
  (s-MapMelody len (om/ (flat (om::lmidic pitches)) 100)))

(om::defmethod! s-MapChords ((len number) (card number) (pitches list))
   :icon 703 :initvals '(10 2 (60 62 64 65 67 69 71 72))
  (om::mki 'om-specimen
       :len (* len card)
       :range `(0 ,(1- (length pitches)))
       :phenotyper #'(lambda (s)
                       (group-list (loop for g in (geno s) collect (nth g pitches))
                                   card
                                   :linear))))

(om::defmethod! s-MapChords ((len number) (card number) (pitches om::score-element))
  :icon 703
  (s-MapMelody len card (om/ (flat (om::lmidic pitches)) 100)))




;;; ���������������������������������������������
;;; ���������� om criteria types  ���������������
;;; ���������������������������������������������



;;;; CUSTOM ;;;;;;;;;;;;;;

(om::defmethod! c-pheno ((fun function) &optional (goal 1))
  :icon 702 :initvals '(nil 1)
  (om::mki 'simple-criterion
       :eval-fun #'(lambda (pheno &optional index)
                     (not-boolean! (funcall fun pheno)))
       :goal goal))

(om::defmethod! c-pheno ((crit criterion) &optional goal)
  :icon 702 :initvals '(nil 1)
  (c-alt-pheno crit 'pheno))

(om::defmethod! c-spec ((fun function) &optional (goal 1))
  :icon 702 :initvals '(nil 1)
  (om::mki 'spec-criterion
       :eval-fun #'(lambda (pheno &optional index)
                     (not-boolean! (funcall fun pheno index)))
       :goal goal))


(om::defmethod! c-print ()
  :icon 702
  (c-spec #'(lambda (x &optional index)
              (print (format nil "index ~D : ~A" index x)))))


(om::defmethod! c-null ()
  :icon 702
  (om::mki 'criterion) ;;; always returns 0.0
)
                  


;;;;;;**** conditional criterion

(om::defmethod! c-if (condition crit)
  :icon 702
  (if condition
      crit
    (om::mki 'criterion)  ;;; <--- always returns 0.0
    ))


(defun not-boolean! (x)
;;; we want true = 1, nil = 0, so that we can average the values.
  (if (or (is-fraction x) (numberp x))
      x
    (if x 1 0)))



(defun take-proper-average (results) 
;;; if the results are fractions, then averaging their fraction-values would give an incorrect result,
;;; since it wouldn't take into account differing denominators of different results.
;;; ie., taking a pitch survey where different chords have different cardinalities
;;; this takes care of that!

  (if (null results)
      (fraction 0 0)
    (join-fractions (loop for result in results
                          collect (if (numberp result)
                                      (fraction result 1)
                                    result)))))

(om::defmethod! c-block ((fun t) (width t) &optional (goal 1) (provide-index t))
  ;;; if width is NIL, then the elements of the pheno are evaluated individually, instead of subsequences
  :icon 702 :initvals '(nil 2 1)
 (om::mki 'simple-criterion
       :eval-fun #'(lambda (pheno &optional index)
                     (fraction-value 
                      (take-proper-average (loop with num = (- (length pheno) (if width (1- width) 0))
                                                 for sub on pheno
                                                 for k from 0
                                                 while (>= (length sub) (or width 1))
                                                 collect (not-boolean! 
                                                          (funcall fun 
                                                                   (if width
                                                                       (subseq sub 0 width)
                                                                     (car sub))
                                                                   (or index 
                                                                       (when provide-index
                                                                         (if (= num 1)
                                                                             0
                                                                           (/ k (1- num))))
                                                                         )))
                                                 )
                                           )))
       :goal goal))



 
(om::defmethod! c-block ((fun criterion) (width t) &optional (goal 1) (provide-index t))
  :icon 702 :initvals '(nil 2 1)
  (c-block #'(lambda (sub-pheno &optional index) 
               (evaluate sub-pheno fun index))
           width goal provide-index)) 
            


(om::defmethod! c-subblock ((fun t) (width number) (height number) (avo symbol) &optional (goal 1))
  :icon 702 
  :initvals '(nil 2 2 :all 1) 
  :menuins '((3 (("all combinations" :all) ("adjacent only" :avo))))
  (c-block #'(lambda (blok &optional index)
               (let ((combos ;'((0 1) (1 2)))
                      (funcall (if (equalp avo :avo)
                                   #'adjacent-subset-list   
                                 #'make-subset-list)
                               (arithm-ser 0 (1- (length (car blok))) 1) height))
                     )
                 (take-proper-average (loop for combo in combos
                                            collect (not-boolean! (funcall fun (voices-of blok combo) index)))
                                      )))
           width
           goal))


(om::defmethod! c-melody ((fun t) (width number) &optional (goal 1))
  :icon 702 :initvals '(nil 2 1)
  (c-block #'(lambda (blok &optional index)
               (take-proper-average (loop for melody in (mat-trans blok)
                                          if (= (length melody) width)
                                          collect (not-boolean! (funcall fun melody index)))
                                    ))
           width
           goal))

(om::defmethod! c-melody ((fun criterion) (width number) &optional (goal 1))
  :icon 702 :initvals '(nil 2 1)
  (c-block #'(lambda (blok &optional index)
               (take-proper-average (loop for melody in (mat-trans blok)
                                          if (= (length melody) width)
                                          collect (evaluate melody fun index))
                                    ))
           width
           goal))

;;; same thing as c-melody, but abstract : take "columns" out of a list of "rows"
(om::defmethod! c-trans ((fun criterion) (width number) &optional (goal 1))
  :icon 702 :initvals '(nil 2 1)
  (c-melody fun width goal))



(om::defmethod! c-chord ((fun t) &optional (goal 0))
  :icon 702 :initvals '(nil 0)
  (c-block #'(lambda (list-of-one-chord &optional index)
               (funcall fun (car list-of-one-chord)
                            index))
           1
           goal))

(om::defmethod! c-chord ((fun criterion) &optional (goal 0))
  :icon 702
  (c-block fun 1 goal))

;;;; new addition april 2011 ;;;;;
(om::defmethod! c-element ((crit criterion) &optional (goal 0) (provide-index t))
  :icon 702
  (c-block crit nil goal provide-index))



(om::defmethod! c-subchord ((fun t) (height number) (avo t) &optional (goal 1))
  :icon 702 :initvals '(nil 2 nil 1)
  :menuins '((2 (("all combinations" :all) ("adjacent only" :avo))))
   (c-subblock #'(lambda (listy-chord &optional index)
                   (funcall fun (car listy-chord) index))
               1 height avo goal))


(om::defmethod! c-note ((fun t) &optional (goal 1))
  :icon 702 :initvals '(nil 1)
  (c-chord #'(lambda (chord &optional index)
               (fraction (loop for note in chord
                               sum (not-boolean!
                                    (funcall fun note index)))
                         (length chord)))
           goal))

(om::defmethod! c-note ((crit criterion) &optional (goal 1))
  :icon 702 :initvals '(nil 1)
  (c-chord #'(lambda (chord &optional index)
               (fraction (loop for note in chord
                               sum (not-boolean!
                                    (evaluate note crit index)))
                         (length chord)))
           goal))




(defmethod objfromobjs ((fun function) (type criterion))
  (om::mki 'simple-criterion
       :eval-fun #'(lambda (pheno &optional index)
                     (declare (ignore index))
                     (funcall fun pheno))
       :goal 0))
                     


(om::defmethod! forbid ((crit function))
  :icon 702
  (om::mki 'simple-criterion
       :eval-fun #'(lambda (pheno &optional index)
                     (not-boolean! (funcall crit pheno)))
       :goal 0))

(om::defmethod! forbid ((crit criterion))
  :icon 702
  (om::mki 'simple-criterion
       :eval-fun #'(lambda (pheno &optional index)
                     (evaluate pheno crit index))
       :goal 0))




(defmacro with-dynamic-value ((maybe-bpf) &body body)

  `(if (is-enved ,maybe-bpf)
       (let ((converted (mapcar #'convert-to-list-pairs (om::plots ,maybe-bpf))))
         (if (= (length converted) 2)
             (progn
               ,@(subst `(get-range-from-bpfs converted index)
                        maybe-bpf
                        body))
           (progn
             ,@(subst `(x-transfer (first converted) index)
                      maybe-bpf
                      body))))
     (if (and (listp ,maybe-bpf) 
              (notany 'atom ,maybe-bpf)
              (listp (car ,maybe-bpf)))  ;; nested list means it's a bpf (already in list pairs)
         (let ((converted (if (listp (caar ,maybe-bpf))    ;;; means it is a list of (hopefully) 2 bpfs
                              ;;; (((0 0) (1 .5)) ((0 .5) (1 1)))
                              ;;; note: it could be (nil <bpf>) ... still would satisfy caar!
                              ;;; in any case, leave it be.
                              ,maybe-bpf
                            (if (listp (cadar ,maybe-bpf))   ;; means it's a range bpf (such as ((0.0 (2 5)) (1.0 (4 8))))
                                
                              ;convert to two normal list-paired bpfs
                                (mat-trans (loop for point in ,maybe-bpf
                                                 collect (list (list (car point) (caadr point))
                                                               (list (car point) (cadadr point)))))
                              
                              
                              (list ,maybe-bpf)  ;;; otherwise ... it is a single bpf ... make it a list of one bpf (for get-range-from-bpfs)
                              ))))
           ,@(subst `(get-range-from-bpfs converted index)
                    maybe-bpf
                    body))
       (progn
         ,@body))))




;(defclas silly-criterion (simple-criterion) ())

(om::defmethod! c-fun ((fun t) (value t))
  :icon 702 :initvals '(nil nil)
  (with-dynamic-value (value) 
    (om::mki 'simple-criterion
         :eval-fun #'(lambda (pheno &optional index)
                       (fraction (not-boolean! (interpret-range-1 (funcall fun pheno) value))
                                 1))
         :goal 0)))


;;; new april 2011
(om::defmethod! c-apply ((fun t) (value t))
  :icon 702 :initvals '(nil nil)
  (with-dynamic-value (value) 
    (om::mki 'simple-criterion
         :eval-fun #'(lambda (pheno &optional index)
                       (fraction (not-boolean! (interpret-range-1 (apply fun pheno) value))
                                 1))
         :goal 0)))


;compat
(om::defmethod! c-func ((fun t) (value t))
   :icon 702
   (c-fun fun value))

(defun process-min-and-max (plots)
  (flet ((min-fun (plot) (equalp (last-n (car plot) 4) "-min"))
         (max-fun (plot) (equalp (last-n (car plot) 4) "-max")))

    (loop for pair in (demix plots ;(mapcar #'(lambda (n) (list (first n))) plots) ;debugging
                              #'(lambda (plot)
                                  (if (or (min-fun plot)
                                          (max-fun plot))
                                      (om::string-butlast (car plot) 4))))
          collect (if (min-fun (first pair))
                      (if (and (second pair) (max-fun (second pair)))
                          pair
                        (list (first pair) nil))
                    (if (max-fun (first pair))
                        (if (and (second pair) (min-fun (second pair)))
                            (reverse pair)
                          (list nil (first pair)))
                      (first pair))))))
                    

(om::defmethod! c-envelopes ((enved om::om-enved))
  :icon 702 :initvals '(nil nil)
  (apply #'c-list 
         (loop for plot in (process-min-and-max (om::plots enved))
               collect (if (= (length plot) 2)
                           (c-func (read-from-string (om::string-butlast (if (first plot)
                                                                         (caar plot)
                                                                       (caadr plot)) 4)) 
                                   (mapcar #'convert-to-list-pairs plot))
                         (c-func (read-from-string (car plot))
                                 (convert-to-list-pairs plot))))))
         
                                   
                                         

        

                                              
  

(om::defmethod! c-structures ((crit criterion) &optional (goal 0))
;to evaluate all structures within a phenotype ... in all tree branches
  :icon 702
  (om::mki 'simple-criterion
       :eval-fun #'(lambda (pheno &optional index)
                     (take-proper-average (let ((lis (remove-if-not #'structurep (flat pheno))))
                                            (loop for struct in lis
                                                  for k from 0
                                                  collect (evaluate struct crit (or index 
                                                                                   (/ k (1- (length lis)))))))))
       :goal goal))
                                            




;;; ���������������������������������������������
;;; ���������� histograms �����������������������
;;; ���������������������������������������������


(om::defclass! histogram () 
  ((spread :initform '(1 1 1 1 1 1 1 1 1 1 1 1) :initarg :spread :accessor spread)
   (measure-fun :initform #'(lambda (x) (mod x 12)) :initarg :fun :accessor measure-fun)))



(om::defmethod! c-note ((self histogram) &optional (goal 1))
  :icon 702 :initvals '(nil 1)
  (om::mki 'simple-criterion
       :eval-fun #'(lambda (pheno &optional index)
                     (hg-compare self (flat pheno)))
       :goal 0))

(defmethod hg-compare ((self histogram) subj)
  (let ((v (om-scale/sum (spread self) (length subj))))
    (loop for item in subj
          do
          (decf (nth (funcall (measure-fun self) item) v)))

    (fraction (reduce '+ (om-abs v)) (length subj)))) 
    
                 


;;; ���������������������������������������������
;;; ���������� joining criteria �����������������
;;; ���������������������������������������������



(om::defmethod! c-list (&rest args)
  :icon 702
  :doc "Join criteria together.

A numeric argument applies a weight to the criteria on the immediate left.
A list of two numbers applies a weight and an exponent.

The exponent is applied first, then the weight. See `calculate-score`."

  (let ((criteria '())
        (name (when (stringp (car args)) (car args))))
    (dolist (arg args)
      (when (and arg (not (stringp arg)))
        (cond
          ((and (numberp arg) criteria)
           (setf (weight (car criteria)) arg))
          ((and (listp arg) criteria)
           (setf (weight (car criteria)) (first arg)
                 (exponent (car criteria)) (second arg)))
          (t (push arg criteria)))))
    (om::mki 'list-criterion
             :criteria (nreverse criteria)
             :name name)))




;;; ���������������������������������������������
;;; ���������� alteration criteria ��������������
;;; ���������������������������������������������

;;; uses a different phenotyper INSTEAD of (phenotyper <specimen>) (so, this phenotyper operates on a specimen instance)
(om::defmethod! c-alt-pheno ((crit criterion) (phenotyper t))
  :icon 702
  (om::mki 'alt-pheno-criterion
       :crit crit
       :pheno phenotyper))

;;; uses another phenotyper IN ADDITION to (phenotyper <specimen>) (so, this phenotyper operates on a phenotype)
(om::defmethod! c-post-pheno ((crit criterion) (phenotyper t))
  :icon 702
  (om::mki 'post-pheno-criterion 
       :crit crit 
       :pheno phenotyper))



(om::defmethod! c-group-list ((crit criterion))
  :icon 702
  (c-alt-pheno crit #'(lambda (spec)
                        (group-list (geno spec) (len spec) :linear))))


(om::defmethod! c-voices ((crit criterion) (voices list))
  :icon 702
  (c-post-pheno crit #'(lambda (pheno)
                         (voices-of pheno voices))))


(om::defmethod! c-voices ((crit criterion) (voices number))
  :icon 702
  (c-voices crit (list voices)))



;beautiful!
(om::defmethod! c-voice-funnel (&rest criteria)
  :icon 702
  (apply #'c-list (loop for crit in criteria
                        for voice from 0
                        when crit
                        collect (c-voices crit voice))))


(om::defmethod! c-portion ((crit criterion) (range list) &optional (mode :proportional))
  ;;; may 1st 2011 rewrite

  ;;; mode is either: :proportional :ordinal :time :proportional-time 
  ;;; proportional means that ranges has values between 0 and 1, translating to element positions based on length of the phenotype.
  ;;; ordinal means that range will give the range of element positions
  ;;; time means that pheno is an arrange and range gives the range of start times (om ratios)
  ;;; proportional-time is not implemented


  :icon 702
  (c-post-pheno crit #'(lambda (pheno)
                         (case mode
                           (:proportional
                            (subseq pheno 
                                       (round (* (first range) (length pheno)))
                                       (round (* (second range) (length pheno)))))
                           (:ordinal 
                            (subseq pheno (first range) (second range)))

                           (:time 
                            (loop for region in pheno
                                 ; while (<= (region-start region)
                                 ;           (second range)) ;;;; so, like many functions, assumes the phenotype is sorted by region-start
                                  if (withinp (region-start region)
                                              (first range)
                                              (second range))
                                  collect region))))))

(om::defmethod! c-sections ((boundaries list) (mode t) &rest criteria)
  ;;; mode is either: :proportional :ordinal :time :proportional-time 
  ;;; see c-portion for modes

  :icon 702
  (apply #'c-list (let ((sub boundaries))
                    (loop for criterion in criteria
                          while (cdr sub)
                          collect (if (or (numberp criterion)
                                          (and (listp criterion) (numberp (car criterion))))
                                      criterion ;;; it's a weight
                                    (prog1
                                        (c-portion criterion (first-n sub 2) mode)
                                      (pop sub)))))))
            
 

;;; updated may1st 2011
(defmethod get-time-blocks ((self list) resolution &optional overlap (mode :list))
  ;; modes are :list and :arrange
  ;; :list cuts by index, :arrange cuts by region-start positions
  (declare (ignore overlap)) ; not implemented yet

  (loop with result = nil
        with block = nil
        with start = (if (equalp mode :list)
                         0
                         (/ (floor (* (region-start (first self)) 4)) 4))
        with block-width = (/ (if (equalp mode :list)
                                  (length self)
                                  (- (/ (ceiling (* (region-start (last-elem self)) 4)) 4)
                                     start))
                              resolution)
        with block-index = 1
        for element in self
        for index from 0
        do
          (if (and (< block-index resolution)
                   (>= (if (equalp mode :list)
                           index
                           (region-start element))
                       (+ (* block-width block-index) start)))
              (progn
                (push (nreverse block) result)
                (incf block-index)
                (setf block (list element)))
              (push element block))
        finally (push (nreverse block) result)
        (return (nreverse result))))


(defmethod get-time-blocks ((self specimen) resolution &optional overlap (mode :list))
  (get-time-blocks (pheno self) resolution overlap mode))
        


(om::defmethod! c-dynamic ((crit criterion) (resolution number) &optional overlap (mode :list))
  ;;;; modes are :list and :arrange ... arrange divides according to region-starts!!!
  :icon 702 
  (om::mki 'spec-criterion 
       :eval-fun #'(lambda (spec &optional index)     
                     (fraction-value 
                      (take-proper-average ;(let ((sorted (sort pheno #'< #'first)))
                       (remove-if #'null
                                  (loop for tb in (get-time-blocks spec
                                                                   resolution
                                                                   overlap
                                                                   mode)
                                        for index from 0
                                        if tb
                                        collect (evaluate tb 
                                                          crit
                                                          (/ index (1- resolution))))))))))




;;;;;;;;;;;;
;;; common criteria

(defparameter *mode-menu* '(("unsigned" :unsigned) ("signed" :signed) ("unsigned class" :unsigned-class) ("signed class" :class)))

(defun apply-mode (num mode)
  (case mode
    (:unsigned (abs num))
    (:signed num)
    (:class (signed-mod num 12))
    (:unsigned-class (mod (abs num) 12))))

(defun interpret-range (num list goal)
  (if (and (listp list) (equalp (first list) 'set))
      (if (member num (cdr list) :test #'=) t)
    (if goal
        (if (atom list)
            (= num list)
          (withinp num list))
      (if (atom list)
          (abs (- num list))
        (within-M num list)))))


(defun interpret-range-1 (value rng)
  (if (and (listp rng) (equalp (first rng) 'set))
      (if (member value (cdr rng) :test #'equalp) t)
    
    (if (or (null rng)
            (equalp rng t))
        (abs (- (not-boolean! value) (not-boolean! rng)))

      (if (atom rng)
          (abs (- value rng))
        (within-M value rng)))))


;---------
(om::defmethod! c-melodic ((range list) (mode symbol) &optional (goal nil))
  :icon 702 :initvals '(1 :unsigned nil)
  :menuins `((1 ,*mode-menu*))
  (if (or goal (equalp (first range) 'set))
      (c-melody #'(lambda (m &optional index)
                    (not-boolean! (interpret-range (apply-mode (- (second m) (first m)) mode) range goal)))
                2 goal)
    (c-melody #'(lambda (m &optional index)
                  (within-M (apply-mode (- (second m) (first m)) mode) range))
              2 0)))

(om::defmethod! c-melodic ((range number) (mode symbol) &optional (goal nil))
  :icon 702 :initvals '(1 :unsigned nil)
  :menuins `((1 ,*mode-menu*))
  (if goal
      (c-melody #'(lambda (m &optional index)
                    (not-boolean! (= (apply-mode (- (second m) (first m)) mode) range goal)))
                2 goal)
    (c-melody #'(lambda (m &optional index)
                  (abs (- (apply-mode (- (second m) (first m)) mode) range)))
            2 0)))


(om::defmethod! c-delta ((range t) (mode symbol) &optional (goal nil))
  :icon 702 :initvals '(1 :unsigned nil)
  :menuins `((1 ,(first-n *mode-menu* 2)))
   (c-melodic range mode goal))


;------------


(om::defmethod! c-motion ((type symbol) &optional (goal 0) (avo :all))
  :icon 702 :initvals '(:parallel 0 :all)
  :menuins `((0 (("parallel" :parallel) ("similar" :similar) ("contrary" :contrary) ("oblique" :oblique)))
             (2 (("all combinations" :all) ("adjacent only" :avo))))
  (c-subblock #'(lambda (blok &optional index)
                  (not-boolean!
                   (equalp type
                           (let ((interval1 (- (first (second blok))
                                               (first (first blok))))
                                 (interval2 (- (second (second blok))
                                               (second (first blok)))))
                             (cond ((= interval1 interval2) :parallel)
                                   ((or (= interval1 0) (= interval2 0)) :oblique)
                                   ((= (sign-of interval1) (sign-of interval2)) :similar)
                                   (t :contrary))))))
              2 2 avo goal))    


;------------

(om::defmethod! c-harmonic ((range list) (mode symbol) &optional (goal nil) (avo :all))
  :icon 702 :initvals '(1 :unsigned nil :all)
  :menuins `((1 ,*mode-menu*)
             (3 (("all combinations" :all) ("adjacent only" :avo))))
  (if (or goal (equalp (first range) 'set)) 
      (c-subchord #'(lambda (m &optional index)
                      (not-boolean! (interpret-range (apply-mode (- (second m) (first m)) mode) range goal)))       
                  2 avo (or goal 1))
    (c-subchord #'(lambda (m &optional index)
                    (within-m (apply-mode (- (second m) (first m)) mode) range))
                2 avo 0)))

(om::defmethod! c-harmonic ((range number) (mode symbol) &optional (goal nil) (avo :all))
  :icon 702 :initvals '(1 :unsigned nil :all)
  :menuins `((1 ,*mode-menu*) 
             (3 (("all combinations" :all) ("adjacent only" :avo))))
  (if goal
      (c-subchord #'(lambda (m &optional index)
                      (not-boolean! (= (apply-mode (- (second m) (first m)) mode) range)))
                2 avo goal)
    (c-subchord #'(lambda (m &optional index)
                    (abs (- (apply-mode (- (second m) (first m)) mode) range)))
            2 avo 0)))


(om::defmethod! c-card ((card t) &optional (goal nil))
  :icon 702 :initvals '(2 nil)
  (c-chord (with-dynamic-value (card)
             (if goal 
                 #'(lambda (chord &optional index) (if (withinp (if (equalp chord :silence) 0 (length chord)) (range! card)) 1 0))
               #'(lambda (chord &optional index) (within-m (if (equalp chord :silence) 0 (length chord)) (range! card))))
           (or goal 0))))

;---------


(om::defmethod! c-pitch ((range t) (mode symbol) &optional goal)
  :icon 702 :initvals '(1 :class nil)
  :menuins '((1 (("class" :class) ("unclassed" :signed))))

     (c-note (with-dynamic-value (range)
               #'(lambda (m &optional index)
                   (not-boolean! (interpret-range (apply-mode m mode) range goal))))
             (or goal
                 (if (and (not (is-enved range)) (listp range) (equalp (first range) 'set))
                     1
                   0))))


(c-pitch 0 :class)

;-------- synonymous with c-pitch, but might be better symantically
(om::defmethod! c-number ((range t) (goal t))
  :icon 702 :initvals '(0 1)
  (c-pitch range :signed goal))





;------ move this to somewhere else in the file

(defun convert-to-list-pairs (plot)
  (loop for coord in (second plot) ;;; coords (list of dotted pairs)
        collect (list (car coord) (cdr coord))))
  

(defun get-range-from-bpfs (bpfs index)
  (let ((yvals (loop for bpf in bpfs
                     collect (float (when bpf
                                      (x-transfer bpf index))))))
    (if (= (length yvals) 1)
        (car yvals)
      yvals)))









(om::defmethod! c-pset ((pset list) (goal t) &optional (modulo 12) (zero 60))
  :icon 702 :initvals '((0 4 7) 1.0 12 60) 
  (om::mki 'simple-criterion
           :eval-fun #'(lambda (pheno &optional index)
                         (let ((notes (flat pheno))
                               (trues 0))
                           (dolist (note notes)
                             (when (member (mod (+ note modulo (- (mod zero modulo))) modulo)
                                           pset)
                               (incf trues)))
                           (/ trues (length notes))))
           :goal goal))



(om::defmethod! c-diatonish ((min number) (max number) &optional (goal 1.0))
  :icon 702 :initvals '(3 5 1.0)
  (om::mki 'simple-criterion
           :eval-fun #'(lambda (pheno &optional index)
                         (let ((denom 0)
                               (numer 0))
                           (loop for sub on pheno
                                 while (>= (length sub) min)
                                 do (incf denom)
                                    (incf numer
                                          (if (> (belongs-to (flat (subseq sub 0 min))
                                                             '(0 2 4 5 7 9 11)) 0)
                                              (if (> (length sub) max)
                                                  (if (> (belongs-to (flat (subseq sub 0 (1+ max)))
                                                                     '(0 2 4 5 7 9 11)) 0)
                                                      0
                                                      1)
                                                1)
                                            0)))
                           (if (plusp denom)
                               (/ numer denom)
                               0.0))))
           :goal goal)


(om::defmethod! c-belonging ((min number) (max number) (alt-set list) &optional (goal 1.0))
  :icon 702 :initvals '(3 5 (0 2 4 5 7 9 11) 1.0)
  (om::mki 'simple-criterion
           :eval-fun #'(lambda (pheno &optional index)
                         (let ((denom 0)
                               (numer 0)
                               (set (or alt-set '(0 2 4 5 7 9 11))))
                           (loop for sub on pheno
                                 while (>= (length sub) min)
                                 do (incf denom)
                                    (let ((first-belong (belongs-to (flat (subseq sub 0 min)) set)))
                                      (when (> first-belong 0)
                                        (if (> (length sub) max)
                                            (let ((second-belong (belongs-to (flat (subseq sub 0 (1+ max))) set)))
                                              (when (= second-belong 0)
                                                (incf numer)))
                                          (incf numer)))))
                           (/ numer denom)))
           :goal goal))





(om::defmethod! c-weighted-avo ((crit criterion) (voices list))
  :icon 702
  (apply 'c-list
         (loop for pair in (adjacent-subset-list voices 2)
               for weight from 2 by -.1
               collect (c-voices crit pair)
               collect weight)))





(defmethod population-from-model ((model t) criterion)
  (d::make-population criterion
                      model))
                       

(defparameter *output-dur* 500)
(defparameter *output-tempo* 100)

(defvar *dwn-result-list* (create-list 50 nil))




(defmethod set-geno ((self specimen) geno)
  (let ((new (true-copy self)))
    (setf (geno new) geno)
    (clear-pheno new)
    new))



(defun geno-filename (ID)
  (make-pathname :directory '(:absolute "Users" "geofholbrook" "Data" "lib" "OM-libraries" "om-darwin" "geno-storage")
                 :name (format nil "geno-~D" ID)
                 :type "lisp"))

(defun save-genotype (ID geno)
  (with-open-file (out (geno-filename ID)
                       :direction :output
                       :if-exists :rename-and-delete)
    (format out "~A" geno)))

(defun load-genotype (ID)
  (with-open-file (in (geno-filename ID)
                       :direction :input)
    (read in))) 
  


(om::defmethod! continuation-gate ((continue? t) (new-specimen specimen) (id number))
  :icon 701
  (if continue? 
      (progn
        (set-geno new-specimen (load-genotype id)))
    (set-geno new-specimen (rnd-genotype new-specimen))))




(defmethod objfromobjs ((self om-specimen) (type chord-seq))
  (om::mki 'chord-seq
       :lmidic (om::om* (pheno self) 100)
       :lonset `(0 ,*output-dur*)
       :ldur `(,*output-dur*)))


(defun stored-genotypes ()
  *dwn-result-list*)

(defun restore-stored-genotypes (list)
  (setf *dwn-result-list* list))


(om::defmethod! freeze-specimen ((self sp-list) (mod t) (exception t))
  :icon 705 
  (setf (freeze self) (if mod
                          (list mod (om::list! exception))
                        t))
   self)

(om::defmethod! unfreeze-specimen ((self sp-list))
  :icon 705
  (setf (freeze self) nil)
  self)



;;;; fix terrible sextuplets in OM

(defmethod make-same-number-type-as (self model)
  (if (floatp model)
      (float self)
    (round self)))

(defmethod split-sextuplets ((self list))  
  (let ((new (copy-tree self)))
    (list (first new)
          (loop for measure in (second new)
                collect (list (first measure)
                              (loop for beat in (second measure)
                                    if (and (listp beat)
                                            (listp (second beat))
                                            (every #'atom (second beat))
                                            (not (every #'(lambda (x) (= x 1)) (second beat)))
                                            (= (apply '+ (mapcar #'abs (second beat))) 6))
                                    collect (list (first beat)
                                                  (let ((first-half nil)
                                                        (second-half nil)
                                                        (sum 0))
                                                    (dolist (div (second beat))
                                                      (cond
                                                        ((= sum 3)
                                                         (push div second-half))
                                                        ((<= (+ sum (abs div)) 3)
                                                         (push div first-half)
                                                         (incf sum (abs div)))
                                                        (t
                                                         (let ((remaining (- 3 sum)))
                                                           (push (make-same-number-type-as (* (sign-of div) remaining) div) first-half)
                                                           (push (* (sign-of div) (float (- (abs div) remaining))) second-half))
                                                         (setf sum 3))))
                                                    (setf first-half (nreverse first-half))
                                                    (setf second-half (nreverse second-half))
                                                    `((1 ,first-half)
                                                      ,(if (every #'(lambda (n) (< n 0)) second-half)
                                                           -1
                                                           `(1 ,second-half)))))
                                    else collect beat))))))
     

     
(defmethod split-sextuplets ((self voice)) 
  (let ((new (om::copy-container self)))
    (setf (om::tree new)
          (split-sextuplets (om::tree new)))
    new))
    
(defmethod split-sextuplets ((self poly))  
  (om::mki 'poly
       :voices (loop for voice in (om::voices self)
                     collect (split-sextuplets voice))))


;;;; FUCKING OMQUANTIFY!!! have to do it myself, with 4 days until my piece is due.

(defun fwd-back-possibilities (num)
  (if (= num 1)
      '((:forward) (:backward))
    (loop for next in (fwd-back-possibilities (- num 1))
                append (list (cons :forward next)
                             (cons :backward next)))))

(defun best-quantify (ratios max-div)
  (let* ((results
           (loop for fwd-back in (fwd-back-possibilities (length ratios))
                 append
                 (loop for q-divisor from 2 to max-div
                       for divisor = (* q-divisor 4)
                       collect
                       (let ((accum-list (cdr (dx->x 0 (mapcar #'abs ratios))))
                             (total-offset 0)
                             (new-accum-list '()))
                         (loop for accum in accum-list
                               for fb in fwd-back
                               for snapped = (multiple-value-list (om::snap-ratio-to-divisor accum divisor :dir fb))
                               do
                                 (push (first snapped) new-accum-list)
                                 (incf total-offset (abs (second snapped)))
                               finally
                                 (setf new-accum-list (nreverse new-accum-list))
                                 (when (= (length (remove-duplicates new-accum-list)) (length ratios))
                                   (return
                                     (list divisor
                                           total-offset
                                           (loop for ratio in ratios
                                                 for dx in (x->dx (cons 0 new-accum-list))
                                                 collect (* (sign-of ratio) dx))))))))))
    (when results
      (let ((filtered (remove-if #'null results)))
        (if (> (least-of (mapcar #'first filtered)) 6)
            (third (first (sort filtered #'< :key #'first)))
          (third (first (sort (remove-if #'(lambda (r)
                                             (> (first r) 6))
                                         filtered)
                              #'< :key #'second)))))))))



;;; write a  new version that
;;; doesn't consider the loss of a rest to be a big deal. (duh)




(defun tie-start-of-beat (beat)
;;; recursiveness takes care of a nested beat, which mktree does occasionally for sextuplet figures
;;; (and maybe in other circumstances)
   (list (first beat)
         (cons (if (listp (first (second beat)))
                   (tie-start-of-beat (first (second beat)))
                 (float (first (second beat))))
               (cdr (second beat)))))

  
((defun get-rid-of-inner-floats (beat)
  (let ((result '()))
    (dolist (x beat)
      (cond
        ((null result)
         (push x result))
        ((floatp x)
         (let ((prev (pop result)))
           (push (if (floatp prev)
                     (float (+ prev x))
                   (round (+ prev x)))
                 result)))
        (t
         (push x result))))
    (nreverse result))))


(defmethod quantify-complex-beats ((self list) &optional (max/ 6) (max/-in 8))   ;;;tree
  (let ((new (copy-tree self)))
     (list (first new)
           (loop for measure in (second new)
                 collect (list (first measure)
                               (loop for beat in (second measure)
                                     if 
                              ;definition of a complex beat
                                     (and (listp beat)
                                          (every 'atom (second beat))
                                          (notevery #'(lambda (x) (= x 1)) (second beat))
                                          (> (apply '+ (mapcar 'abs (second beat))) max/-in))
                                     collect  
                                     (let ((ratios (car (om::tree-to-ratios `(? (((1 4) ((1 ,(get-rid-of-inner-floats (second beat)))))))))))
                                                         ;will have to restore the tie, if there was one (indicated as a float in the tree)
                                                         ;(ratio list has no way of representing ties)

                                       (let ((new-beat (first (second (first (second  ;;; extract first beat of tree
                                                                                      (om::mktree (or (best-quantify ratios max/)
                                                                                                  ratios)
                                                                                              '(1 4))))))))
                                         (setf new-beat (if (floatp (first (second beat)))
                                                            (tie-start-of-beat new-beat)
                                                          new-beat))
                                         new-beat))

                                     else collect beat))))))


(defmethod quantify-complex-beats ((self om::voice) &optional (max/ 6) (max/-in 9)) 
  (let ((new (om::copy-container self)))
    (setf (om::tree new)
          (quantify-complex-beats (om::tree new) max/ max/-in))
    new))
(defmethod quantify-complex-beats ((self om::poly) &optional (max/ 6) (max/-in 9))
  (om::mki 'om::poly
       :voices (loop for voice in (om::voices self)
                     collect (quantify-complex-beats voice max/ max/-in))))



  


(defvar *fixed-sequence* nil)

(om::defmethod! set-fixed-sequence ((cseq chord-seq))
  (setf *fixed-sequence* (om/ (om::lmidic cseq) 100))) 

(om::defmethod! set-fixed-sequence ((self t))
  (setf *fixed-sequence* self))



(defmethod post-apply-fixed ((pheno list))
  (om::om+ pheno *fixed-sequence*))



;;;;;;; from apprentice

(defun flatten (x)
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))

(defun adjacent-intervals (chord &optional (sorted t))
"Returns the adjacent intervals of the chord from
the bottom note to the top.  Sorts chord in
ascending order non-destructively."
  (let ((in-order (sort (copy-list chord) #'<)))
    (let ((sorted (if sorted
                    in-order
                    chord)))
      (cond ((null (rest sorted)) nil)
            (t (cons 
                (- (second sorted)
                   (first sorted))
                (adjacent-intervals (rest sorted))))))))

(defun list-diffs (list)
  "Returns the absolute differences between the
first element of the list and all the other members
in turn. Ideally, the list should have been sorted
in ascending order before being sent to list-diffs."
    (let ((head (first list))
          (tail (cdr list))
          (result nil))
      (dolist (x tail (nreverse result))
        (push (abs (- x head)) result))))


(defun interval-ratios (chord)
  "Given a chord, returns a list of the adjacent intervals
between the chord tones expressed as percentages of the total
outer interval of the chord."
  (let ((sorted (sort (copy-list chord) #'<)))
    (let ((outer-interval (- (first (last sorted)) (first sorted)))
          (ints (adjacent-intervals sorted)))
      (mapcar #'(lambda (x) (if (= outer-interval 0)
                                0
                              (/ x outer-interval))) ints))))

;;NB: the adjacent-interval-variance of a given chord is a good rough estimate of its
;;acoustic dissonance, and it is *much* faster to compute.  Might be very useful to
;;do an initial cut of a large group of chords with 'adjacent-interval-variance' before
;;sending the result to 'acoustic-dissonance'.

(defun adjacent-interval-variance (chord)
  "Trying to judge the degree of 'balance' in the voicing of
a chord.  Returns the total amount of the absolute difference
between all adjacent intervals expressed as percentages of the
total outer interval.  A chord whose adjacent intervals are
all the same will return 0."
  (float (reduce #'+ (flatten
                      (maplist #'(lambda (x) (list-diffs x)) 
                               (interval-ratios chord))))))






#|

;;; �������������� phenotype test �������������������������������

(defclas s-clarinet (om-specimen)
         ())

(defmethod phenotype ((self sp-clarinet))   ; could also use default-initarg for phenotyper
  (mapcar #'list (om+ (geno self) 2)))

(om::defmethod! s-clarinet ((len number) (range list))
  (om::mki 's-clarinet :len len :range range))

;;;

|#


;compat
(om::defmethod! s-Melody ((len number) (range list) &key (approx 2))
  :icon 703 :initvals '(10 (60 72))
  (om::mki 'om-specimen :len len :range range
       :phenotyper #'(lambda (s)
                       (loop for note in (geno s) collect (list note)))))

;;;;;;;;;
;;; smarter way of doing this?
;;; (could make the phenotyper slot obsolete...)

(om::defclass! sp-Melody (om-specimen) ())

(defmethod phenotype ((self sp-Melody))
  (loop for note in (geno self) collect (list note)))



;;;;;;;;; with rhythm ;;;;;;;;;;;;     NOT DONE!!!   
;;; LATER NOTE:  obsolete, but could have some good ideas

(om::defclas om-rhythm-spec (om-specimen)
  ((rhythm-phenotype :initform nil)
   (pitch-phenotype :initform nil)))


;; bit 0 : tie?
;; bits 1-3 (1-8) division
;; bits 4-6 (1-8) how many?

(defun rhythm-bits (code)
  (let ((tie (floor code 64))
        (division (floor (mod code 64) 8))
        (number (mod code 8)))
    (list tie division number)))


(defmethod evaluate ((self om-rhythm-spec) (crit simple-criterion) &optional index)
  (evaluate (pitch-phenotype self) crit))
