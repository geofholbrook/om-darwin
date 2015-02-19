(in-package dwn)

(defmethod is-enved ((self t)) nil)
(defmethod is-enved ((self om::om-enved)) t)

(defun convert-if-bpf (env index)
  (if (and (listp env)
           (not (numberp (first env))))  ; enved-xy comes in as a list of lists (list of plots)
      (om::x-transfer 
       ;convert to list-pairs for OM function (x-transfer)
       (loop for k on (first env) by #'cddr collect (subseq k 0 2))
       index)
    env))

;;; »»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»
;;; »»»»»»»»»»»»»»»»» DECODING »»»»»»»»»»»»»»»»»
;;; »»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»



(defmethod mod-to-range ((n number) range &optional (step 1) float)
;useful for decoding "genes", where the genotype range is larger than the needed phenotype range
  (let ((result (if (equalp (first range) 'set)
                    (nth (mod n (length (cdr range))) (cdr range))
                  (+ (* (mod n (1+ (/ (- (second range) (first range)) step)))
                        step)
                     (first range)))))
    (if float (float result) result)))

(defmethod mod-to-range ((n list) range &optional (divisor 1) float)
  (loop for elt in n
        collect (mod-to-range elt range divisor float)))


(defun range! (n)
  (if (listp n)
      n
    (list n n)))

(defun chordify (lis cardinality &optional (sort t))
;organize a list into sublists of length cardinality. discard remainder
  (loop repeat (floor (length lis) cardinality)
        for sub on lis by #'(lambda (l) (nthcdr cardinality l))
        collect (if sort
                    (om::sort. (subseq sub 0 cardinality))
                  (subseq sub 0 cardinality))))


;;; »»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»
;;; »»»»»»»»»»»»»»»»» FRACTIONS »»»»»»»»»»»»»»»»»
;;; »»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»

;these are meant to retain more information than an common-lisp fraction ... 
;when two fractions are added, the top and bottom are added, such that the one
;with a larger denominator counts more. as it should be, in taking statistics
;such as in darwin



(om::defclas fraction ()
  ((numer)
   (denom)))

(defmethod is-fraction ((self fraction)) t)
(defmethod is-fraction ((self t)) nil)

(defmethod fraction (n d) (mki 'fraction :numer n :denom d))

(defmethod print-object ((object fraction) out-stream)
  (format out-stream "#F(~A/~A)" (numer object) (denom object)))

(defun join-fractions (lis)
  (loop for frac in lis
        if frac sum (numer frac) into n
        if frac sum (denom frac) into d
        finally return (fraction n d)))

(defmethod fraction-value ((self fraction))
  (unless (= (numer self) (denom self) 0)
    (/ (numer self) (denom self))))

(defmethod fraction-value ((self number))
  self)

(defmethod fraction-value ((self t)) ;probably nil, then
  nil)

(defun n/a? (result) (equalp result :n/a))




;;; »»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»
;;; »»»»»»»»»»»»»»»»» RATIOS (rhythm) »»»»»»»»»»
;;; »»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»

;; a freq is like a "fraction" ... a ratio in LISP but the denominator is preserved (unlike 2/4 wich becomes 1/2 on evaluation)


(defun divisor-floor (time divisor)
  (multiple-value-bind (quotient remainder)
      (floor (* time divisor))
    (values (/ quotient divisor)
            (/ remainder divisor))))

(defun divisor-ceiling (time divisor)
  (multiple-value-bind (quotient remainder)
      (ceiling (* time divisor))
    (values (/ quotient divisor)
            (/ remainder divisor))))


;;; »»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»
;;; »»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»

;(new 2013)

(defun proportion-if (fun list)
  (if (null list)
      0
    (/ (count-if fun list)
       (length list))))

(defun proportion (item list)
  (if (null list)
      0
    (/ (count item list)
       (length list))))


;;; »»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»
;;; »»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»»




(defmethod voices-of ((blok list) voices)
  (if (equalp voices :all)
    blok
    (if (listp (car blok))
        (loop for chord in blok
              collect (loop for voice in voices
                            collect (nth voice chord)))
      (loop for voice in blok
            collect (nth voice blok)))))
  



(defmethod is-generic ((self standard-generic-function)) t)
(defmethod is-generic ((self t)) nil)

(defmethod dwn-method-exists-p (fun class)
  (and (is-generic fun)
       (find-method fun nil `(,class
                                ,@(make-list (1- (slot-value fun 'clos::required-arguments))
                                             :initial-element (find-class t))) nil)))





(defun sqr (x) ;why?
  (* x x))

(defun choose-other (lis self)
"returns an element of lis that is not eq to self"
  (let ((spot (random (1- (length lis)))))
    (if (eq (nth spot lis) self)
      (nth (1+ spot) lis)
      (nth spot lis))))



(defun standard-deviation (lis &key (useful t))
"the 'useful' option means that the standard deviation is divided by the square of the average value"

;(sum of Xsquared -((sum of X)*(sum of X)/N))/ (N-1)) 

  (when (> (length lis) 1)
    (let* ((avg (/ (reduce #'+ lis)
                   (length lis)))
           (normalized (if useful
                         (mapcar #'(lambda (n) (/ n avg)) lis)
                         lis)))
      (om::approx-decimals (coerce
                            (/ (- (reduce #'+ (mapcar #'sqr normalized)) 
                                  (/ (sqr (reduce #'+ normalized)) 
                                     (length lis))) 
                               (1- (length lis))) 'float)
                           1
                           3))))

(defun super-nth (location lis)
"this is essentially a nested version of nth, with the capability of making random choices. Location is a list
 of nth values, starting with the outer list and progressively going inwards. so, (super-nth '(4 2 3) lis) is
 equivalent to (nth 3 (nth 2 (nth 4 lis))) ... funny how outward is inward when it comes to the function call,
 huh? ok ... if any element of location is the symbol 'choose , then the function makes a random choice for the
 nth value based on the length of that level of the list. If the element is of the syntax (choose '(start end))
 then the random choice will be constricted to within the indeces given"
 
  (if (atom location) ;if used simply as nth
    (nth location lis)
    (let* ((spot (if (listp (car location))
                              (rrnd (cdar location))
                              (if (equalp (car location) 'choose)
                                (rrnd 0 (- (length lis) 1))
                                (car location))))
           (first-level (nth spot lis)))
      (if (listp first-level)
        (progn
          (print `((nth spot lis) is ,first-level))
          (multiple-value-bind (result loc-args)
                             (super-nth (cdr location) first-level)
          (values result
                  `(,spot ,@loc-args))))
        (values first-level (list spot))))))


(defun multi-memberp (item-list lis)
; returns true if every member of lis is in item-list 
 (dolist (element lis nil)
    (if (member element item-list)
      (return t))))


(defun memberp-tree (item tree)
  (or (if (listp item)
        (multi-memberp item tree)
        (member item tree))
      (dolist (element tree nil)
        (if (and (listp element)
                 (memberp-tree item element))
          (return t)))))




(defun multi-subst (new-items old-items tree)
"just like subst, but replaces any of a list of old-items to the respective item in new-items"
  (let (result)
    (dolist (item tree (nreverse result))
      (push (if (atom item)
              (let ((spot (position-if #'(lambda (n) (equalp item n)) old-items)))
                (if spot
                  (nth spot new-items)
                  item))
              (multi-subst new-items old-items item))
            result))))
           




(defun range-union (&rest ranges-1)
  
  (let* ((ranges (remove-if #'null ranges-1))
         (rng (and ranges
                   (list (apply #'min (mapcar #'first ranges))
                         (apply #'max (mapcar #'second ranges))))))
    (if (and rng (<= (first rng) (second rng))) 
      rng)))

(defun range-intersection (&rest ranges)
"takes two ranges, returns a range that includes indices falling into both ranges. If ranges are discreet, returns nil.
 Actually this returns a list of ranges, to be consistent with (range-difference), even though it will only return a
 single range -- this makes the result appendable with results from range-difference"
  (let ((rng (list (apply #'max (mapcar #'first (remove-if #'null ranges)))
                   (apply #'min (mapcar #'second (remove-if #'null ranges))))))
    (if (<= (first rng) (second rng)) rng)))

(defun range-difference (range1 range2)
"gives list of ranges which represent what is in range1 but not in range2" 
    (append (if (< (first range1) (first range2))
              (list (list (first range1) (1- (first range2)))))
            (if (> (second range1) (second range2)) 
              (list (list (1+ (second range2)) (second range1))))))



(defun within-rangelist (x rangelist)
"expresses whether x falls into one of the ranges on rangelist"
  (do*  ((r rangelist (cdr r))
         (test (withinp x (car r)) (withinp x (car r))))
        ((or test (null (cdr r)))
         test)))

(defun rangelist-range (rangelist)
"returns a range expressing the scope of a list of ranges, disregarding holes"
  (let ((mi (matrix-invert rangelist)))
    (list (least-of (first mi))
          (greatest-of (second mi)))))


;; get rid of this!
(defun gclip (n min max)
  (if (and min (< n min))
    min
    (if (and max (> n max))
      max
      n)))
      


(defun fixed/rnd (x)
  (if (atom x) x (rrnd x)))

(defun fixed/apply (x fun)
  (if (atom x) x (apply (car fun) `(,x ,@(cdr fun)))))


(defun <=M (a b)
  (if (<= a b)
    0
    (- a b)))

(defun >=M (a b)
  (if (>= a b)
    0
    (- b a)))


(defun old-mapcount (fun &rest rest)
 (apply 'mapcar `(,fun ,@rest ,(let ((result))
                                 (dotimes (num (least-of (mapcar #'length rest)) (nreverse result))
                                   (push num result))))))



(defun nth2 (lis n)
"for use when the reverse order of parameters for nth is convenient"
  (nth n lis))

(defun nth-or-last (n lis)
  (if (>= n (length lis))
    (car (last lis))
    (nth n lis)))

(defun nth-or-last-2 (lis n)
  (if (>= n (length lis))
    (car (last lis))
    (nth n lis)))

(defun same-length (x y)
  (labels ((compare (x y)
             (or (and (null x) (null y))
                 (and x y (compare (cdr x) (cdr y))))))
    (if (and (listp x) (listp y))
      (compare x y)
      (= (length x) (length y)))))

(defun greatest-of (lis)
  (when lis
    (loop for elt in (remove-if #'null lis)
          maximize elt)))

(defun least-of (lis)
  (when lis
    (loop for elt in (remove-if #'null lis)
          minimize elt)))

(defun greatest-length (list-of-lists)
  (labels ((rec (lis)
             (let ((len (length lis)))
               (cond ((= len 0)
                      0)
                     ((= (length lis) 1)
                      (length (first lis)))
                     (t 
                      (+ (rec (remove-if #'null (mapcar #'cdr lis))) 1))))))
    (rec (remove-if #'null list-of-lists))))
    
(defun null-list-p (lis)
  (not (position-if-not #'null lis)))

(defun list-of (num-items item &optional canned &aux x)
  (if (and item (not canned) (listp item) (not (numberp (car item))))
    (dotimes (k num-items (nreverse x))
      (push (apply (car item) (cdr item)) x))
    (dotimes (k num-items (nreverse x))
      (push item x))))
    
(defun lia (x) ;list-if-atom
  (if (atom x) (list x) x))

(defun reorder (lis order)
  (let ((result)) 
    (dolist (place order (nreverse result))
      (push (if place
              (nth place lis))
            result))))

(defun extract-keyword (subj keyword)
  (let ((pos (position-if #'(lambda (item) (equalp item keyword)) subj)))
    (if pos
      (nth (+ pos 1) subj))))

(defun change-element (lis fun range &rest args)
  (let* ((min (if (null (first range)) 0 (first range)))
         (max (if (null (second range)) (- (length lis) 1) (second range)))
         (spot (rrnd min max)))
    (values (append (subseq lis 0 spot) 
                    (list (apply fun (nth spot lis) args))
                    (subseq lis (+ spot 1)))
            spot)))
  

(defun signed-mod (num div)
"performs a modulo on the numerical part of a signed integer, preserving the sign itself"
  (* (mod (abs num) div) (if (> num 0) 1 -1)))

(defun ipoint1 (n1 n2 ratio)      ;1-dimensional, that is
"calculates an inner point between n1 and n2, according to the ratio parameter, where
0 will result in n1, 1 will result in n2, 0.5 is in the middle ... all proportions
in between are possible."
  (+ n1 (* (- n2 n1) ratio))) 

(defun ipoint2 (x1 y1 x2 y2 mx)   ;2-dimensional, that is

  (ipoint1 y1 y2 (/ (- mx x1) (- x2 x1))))

(defun fill-bpf (bpf)
  (let ((new nil))
    (dotimes (n (length bpf) (nreverse new))
      (push (if (listp (nth n bpf)) 
              (nth n bpf) 
              (list (/ n (- (length bpf) 1)) (nth n bpf))) 
            new))))
               
;return the point on bpf (0 to 1 scale) that corresponds to the index x on a (0 to (len-1) scale)
(defun interpol (bpf x len)
  (if (atom bpf) bpf
    (do ((pointer (nthcdr 0 (fill-bpf bpf)) (cdr pointer))
         (ratio (om::om-clip (/-return0iferror x (- len 1)) 0 1)))
        ((>= (caadr pointer) ratio) 
         (ipoint2 (caar pointer)
                  (cadar pointer)
                  (caadr pointer)
                  (cadadr pointer)
                  ratio) ;result
         ))))
       
(defun insert-apply (call &rest insert)
  (apply (car call) `(,@insert ,@(cdr call)))) 

    
(defun 0-if-neg (num)
  (if (> num 0) num 0))



(defun make-pair-list (numlist &aux builder)
  (do ((pointer1 (nthcdr 0 numlist) (cdr pointer1)))
      ((endp (cdr pointer1)) (nreverse builder))
    (do ((pointer2 (cdr pointer1) (cdr pointer2)))
        ((endp pointer2))
      (push (list (car pointer1) (car pointer2)) builder))))

(defun make-subset-list (source subset-size &optional extent)
  (if (= subset-size 0)
    '(nil)
    (do ((pointer source (cdr pointer))
         (result nil))
        ((< (length pointer) subset-size) (nreverse result))
      (dolist (next-level (make-subset-list (subseq pointer
                                                    1
                                                    (when extent
                                                      (min (length pointer)
                                                           extent)))
                                            (- subset-size 1)))
        (push (append (list (car pointer)) next-level) result)))))



(defun adjacent-subset-list (source subset-size)
  (do ((sub-source source  (cdr sub-source))
       (result))
      ((< (length sub-source) subset-size) (nreverse result))
    (push (subseq sub-source 0 subset-size) result))) 
    
(defun /-return0iferror (a b)
  (if (= b 0) 
    0
    (/ a b)))
   
(defun memberp (item list)
  "a version of member that returns t or nil"
  (if (member item list :test 'equalp) t)) 

(defun 0-if-null-f (arith &rest args)
  (if (some #'null args) 0 (apply arith args)))

(defun 0-if-null (num)
  (if (null num) 0 num))

(defun make-double-list-if-atom (x)
  (if (atom x)
    (list x x)
    x))

(defun mod-if (subj boolean modifier)
  (if boolean
    (funcall modifier subj)
    subj))

(defun mod-if-s (subj test modifier)
  (if (funcall test subj)
    (funcall modifier subj)
    subj))

(defun sort-and-remove-duplicates (lis)
;must be a list of numbers
  (let ((result nil))
    (dolist (number lis (sort result #'<))
      (unless (member number result) (push number result))))) 

(defun convert-to-zerostart (seq)
  (mapcar #'(lambda (x) (- x (first seq))) seq))

(defun match-pattern (subject model &key (ordered t))
;tests for a match at transposition -- model must start with a 0
  (equal model (convert-to-zerostart (subseq (flatten subject) 0 (length model)))))

(defun delta (index lis &optional (dist 1))
  (- (nth index lis) (nth (- index dist) lis)))

#| ; this is now part of om-geof
(defun withinp (num min &optional max)
  (if (and (not max) (atom min)) 
    (= num min)
    (let ((min (if max min (first min)))
          (max (if max max (second min))))
      (and (or (null min) (>= num min)) (or (null max) (<= num max))))))
|#

(defmethod within-M ((self number) (min t) &optional max tolerance)
  (max
   0
   (- (if (and (not max) (atom min)) 
        (abs (- self min))
        (let ((min (if max min (first min)))
              (max (if max max (second min))))
          (if (and min (< self min))
            (- min self)
            (if (and max (> self max))
              (- self max)
              0))))
      (or tolerance 0))))

(defmethod within-M ((self list) (min t) &optional max tolerance)
  (mapcar #'(lambda (n)
              (within-M n min max tolerance))
          self))

"how much is it off by?"
(defmethod offby ((self number) (value number))
  (abs (- value self)))

(defmethod offby ((self number) (value list))
  (within-M self value) ;;the old function
)

(defmethod offby ((self list) (value t))
  (mapcar #'(lambda (elt)
              (offby elt value))
          self))


(defmethod om* ((arg1 function) (arg2 t))
  (lambda (n) (om* (funcall arg1 n) arg2)))



(defun collect (&rest results)
  (apply '+ (om::flat (remove-if #'null results))))





(defun =-M (num goal)
  (abs (- goal num)))
        
(defun chord-prox (chord1 chord2)
  (sumlist (mapcar #'(lambda (a b) (abs (- a b))) chord1 chord2)))
      

(defun test-modify-subst (subj test modifier)
  (let ((result))
    (dolist (e subj (nreverse result))
      (push (if (funcall test e) 
              (funcall modifier e)
              (if (atom e)
                e
                (test-modify-subst e test modifier)))
            result))))
                  
(defun flatten (lis)
  (let ((result))
    (dolist (element lis result)
      (setf result (append result (if (atom element) (list element) (flatten element)))))))

(defun merge-voices (voices)
  (let ((result))
    (dotimes (n (greatest-length voices) (nreverse result))
      (push (let ((combined))
              (dolist (voice voices (nreverse combined))
                (dolist (note (nth n voice)) (push note combined)))) result))))


(defun belongs-to (subject model &key one-or-zero)  
;e.g. belongs to a diatonic set ... model must be a sorted list of numbers
;returns nil, or the number of ways it is a member (ex. major triad belongs to diatonic 3x)

;not quartertonal, but if all notes are .5 's then it is tested with all integers
  (if (or (every #'(lambda (n) (= (floor n) n)) subject)
          (notany #'(lambda (n) (= (floor n) n)) subject))
      (let ((prepared-subject (sort-and-remove-duplicates 
                               (mapcar #'(lambda (n) (mod (floor n) 12))
                                       (remove-if #'null (flatten subject)))))
            
            (count 0))
    
        (dotimes (rotation 12 (if (and one-or-zero (> count 0))
                                  1
                                count))
          (if (subsetp (mapcar #'(lambda (num) (mod (+ num rotation) 12)) prepared-subject)
                       model)
              (incf count))))
    0))
  
(defun make-totalcurve (bpf num-samples &optional (window-width *default-window-width*))
  (let ((result)
        (total 0)
        (window nil))
    (dotimes (k num-samples (nreverse result))
      (let ((ipoint (interpol bpf k num-samples)))
        (incf total ipoint)
        (setf window (append window (list ipoint)))
        (if (> (length window) window-width) (decf total (pop window)))
        (push total result)))))


(defun decim (number)
  (format nil "~6F" number))

(defun eval-chance-list (chance-list)
"returns n, corresponding to the nth element (percentile chance of occurence) in chance-list.
 last element of chance-list be a list, which would indicate the index-range of allowable results." 

  (if (listp (first chance-list))
    (eval-chance-list (subseq (cdr chance-list) 
                              (or (first (first chance-list)) 0)
                              ((lambda (x) (and x (+ x 1))) (second (first chance-list)))))  
                            
    (do* ((c chance-list (cdr c))
          (x (+ (random (sumlist chance-list)) 1))
          (total (first chance-list) (+ total (first c)))
          (n 0 (+ n 1)))
      
         ((<= x total) n))))

(defun sumlist (lis)
  (loop for i fixnum in lis 
        sum i))

(defun last-position (item sequence)
  (- (length sequence) (position item (reverse sequence)) 1))

(defun inc-wrap (num delta min &optional max)
  (let ((min (if max min (first min)))
        (max (if max max (second min))))
     (let ((result (+ (mod (+ (- num min) delta) (+ (- max min) 1)) min)))
       (values result (- result num)))))

(defun inc-limit (num delta min &optional max)
  (let ((min (if (listp min) (first min) min))
        (max (if (listp min) (second min) max))
        (init (+ num delta)))
    (cond ((and (numberp max) (> init max)) (values max (- max num)))
          ((and (numberp min) (< init min)) (values min (- min num)))
          (t (values init delta)))))

(defun make-different (num min1 &optional max1 canned)
"there is an equal chance of this function returning any number other than num within min and max.
 if canned is specified, this simply gives the closest result for num changed by canned but remaining
 within the boundaries.

 returns, as a second value, the amount of change made"
  (let ((min (if (listp min1) (first min1) min1))
        (max (if (listp min1) (second min1) max1)))
    (let ((new-num (if canned
                     (om::om-clip (+ num canned) min max)
                     (let* ((delta (+ (random (- max min)) 1))
                            (sum (+ num delta)))
                       (if (> sum max) (- (+ min (- sum max)) 1) sum)))))
        (values new-num (- new-num num)))))
     
;;; new version
(defun d::rnd-other (num range)
  (let ((temp (rrnd (car range) (1- (cadr range)))))
    (+ temp (if (>= temp num) 1 0))))



    
(defun extremeties (lis)    ;inefficient
  (list (least-of lis)
        (greatest-of lis)))


(defun weighted-coin (percentile)
"takes either a number or a canned result"
  (if (numberp percentile)
    (< (random 100) percentile)
    percentile))
        
(defun matrix-invert (matrix)
  (let ((result))
    (dotimes (n (greatest-length matrix) (nreverse result))
      (push (mapcar #'(lambda (lis) (nth n lis)) matrix) result))))

(defun nth-column (n matrix)
  (mapcar #'(lambda (lis) (nth n lis)) matrix))

(defun group-into-chords (pheno chord-size &optional (voice 0) (reorder nil) (remove-dup nil))
  (do* (result
        subresult
        (single-voice-pheno (nth-column voice pheno))
        (subpheno  (if reorder 
                     (sort single-voice-pheno #'<)
                     single-voice-pheno)              (cdr subpheno)))

      ((null subpheno) (nreverse (if (null subresult)
                                   result
                                   (cons (nreverse subresult) result))))

    (push (car subpheno) subresult)
    (if (= (length subresult) chord-size)
      (prog ()
        (push (nreverse (if remove-dup 
                            (remove-duplicates subresult)
                            subresult)) result)
        (setf subresult nil)))))

(defun group-into-chords (pheno chord-size &optional (voice 0) (reorder nil) (remove-dup nil))
  (do* (result
        subresult
        (subpheno (nth-column voice pheno) (cdr subpheno)))

      ((null subpheno) (nreverse (if (null subresult)
                                   result
                                   (cons (nreverse subresult) result))))

    (push (car subpheno) subresult)
    (if (= (length subresult) chord-size)
      (prog ()
        (push (mod-if (mod-if (nreverse subresult) reorder #'(lambda (seq) (sort seq #'<)))
                                remove-dup #'remove-duplicates) result)
        (setf subresult nil)))))

(defun <-null=infinity (a b)
  (or (null b) (and a (< a b))))

(defun order-chords (pheno)
  (mapcar #'(lambda (chord) (sort chord #'<-null=infinity)) pheno))

(defun first-chord-ordered (pheno)
  (list (sort (first pheno) #'<-null=infinity)))

(defun counter-list (num)
  (second (assoc num counters)))
              
(defun list-of-orders (lis)
  (if (= (length lis) 2)
    (list lis (reverse lis))
    (let ((result))
      (dotimes (place (length lis) result) 
        (dolist (next-level (list-of-orders (append (subseq lis 0 place)
                                                    (subseq lis (+ place 1)))))
          (push (cons place next-level) result))))))

(defun create-assoc-list (lis)
  (do* ((index 0 (+ index 1))
        (sublis lis (cdr sublis))
        (assoc-list))
       ((null sublis) (nreverse assoc-list))
    (push (list index (car sublis)) assoc-list)))

(defun remove-immediate-reps (seq)
  (let ((result (list (first seq))))
    (maplist #'(lambda (sub) 
                 (if (and (second sub)
                          (not (equalp (first sub) (second sub))))
                   (push (second sub) result)))
             seq)
    (nreverse result)))

(defun change-nils-to-preceding-elements (seq)
  (let ((result)
        (prev))
    (dotimes (n (length seq) (nreverse result))
      (let ((item (nth n seq)))
        (if item
          (prog ()
            (push item result)
            (setf prev item))
          (push prev result))))))

(defun remove-from-list (seq position)
  `(,@(subseq seq 0 position)
    ,@(subseq seq (+ position 1) (length seq))))

(defun insert-at-position (seq newitem position)
  `(,@(subseq seq 0 position)
    ,newitem
    ,@(subseq seq position (length seq))))




#|  what's all this? i think it exists in openmusic  by now 

(defclass! goal-slider (slider)
    ()
   (:icon 298))

(defmethod get-slot-in-out-names ((self goal-slider))
   (values '("direction" "range" "increment" "value" "action") 
           '(:vertical '(0 20) 1 10 nil)
           '("vertical or horizontal" "min and max values" "step" "initial-value" "a patch in mode lambda")
           '(((0 (("horizontal" :horizontal) ("vertical" :vertical)))) nil nil nil nil)))

(defmethod get-super-default-value ((type (eql 'goal-slider)))
  (om-make-dialog-item 'goal-slider (om-make-point 1 4 ) (om-make-point 50 20 ) "untitled" :direction :vertical :range '(0 20) :increment 1 :value 10))
   
(defmethod get-boxsize ((self goal-slider)) (om-make-point 20 80))

(defmethod rep-editor ((self goal-slider) num)
  (cond
   ((= num 0) (om-get-slider-orientation self))
   ((= num 1) (om-get-slider-range self))
   ((= num 2) (om-slider-increment self))
   ((= num 3) (float (/ (om-slider-value self) 20)))
   (t  (om-dialog-item-action self))))
          
|#
                          
       

    

      

  
                       

                   
       
                  
    


