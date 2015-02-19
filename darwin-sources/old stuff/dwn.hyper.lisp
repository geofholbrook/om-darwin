(in-package darwin)


(defclas hyper (ga-object) ())

(defmethod is-hyper ((self t)) nil)
(defmethod is-hyper ((self hyper)) t)

(defclas h-atom (hyper)
  ((value :initform nil)
   (theclass :initform nil)
   (above :initform nil)
   (below :initform nil)
   (next :initform nil)
   (prev :initform nil)
   (parent :initform nil)
   (index :initform nil)))
; number of neighbours is twice the dimension of the superparent!

(defclas h-number (h-atom)
  ((min-value :initform nil)
   (max-value :initform nil)))

(defclas h-List (h-atom)
         ((items :initform nil)
          (variable-length :initform nil)
          (min-length :initform nil)
          (max-length :initform nil)))


(defmethod h-flat ((self h-atom))
  (list self))

(defmethod h-flat ((self h-list))
  (mki 'h-list :items (reduce #'append (mapcar #'h-flat (items self)))))



(defmethod mutate ((self h-number) &rest args)
  (let* ((max-delta (first args))
         (newvalue
          (if max-delta
            (clip (+ (value self) (rrnd (- max-delta) max-delta))
                  (min-value self)
                  (max-value self))
            (let ((temp (rrnd (min-value self) (1- (max-value self)))))   ;set temp to anything other than current value
              (if (>= temp (value self)) 
                (1+ temp)
                temp)))))
    (let ((delta (- newvalue (value self))))
      (setf (value self) newvalue)
      (mki 'mutation :alteration delta))))

(defmethod mutate ((self h-list) &rest args)
  (let* ((spot (rrnd 0 (1- (length (items self)))))
         (sub-mutation (mutate (nth spot (items self)))))
    (mki 'mutation
         :alteration sub-mutation
         :location spot)))

(defun make-h-number (value &optional range)
  (mki 'h-number 
       :value (or value (rrnd range))
       :min-value (and range (first range)) :max-value (and range (second range))))

(defun make-h-list (dimensions atom-function)
;;; creates an inter-linked multi-dimensional structure, each element of which is determined by
;;; atom-function. This can be a function, or a fixed value. A list of two numbers '(x y) is equivalent
;;; (shorthand) to #'(lambda () (rrnd x y))
  (if dimensions
      (let ((new (mki 'h-List :items (loop repeat (if (listp (first dimensions))
                                                      (rrnd (first dimensions))
                                                    (first dimensions))
                                           collect (make-h-list (cdr dimensions) atom-function))
                      :variable-length (listp (first dimensions))
                      :min-length (and (listp (first dimensions)) (first (first dimensions)))
                      :max-length (and (listp (first dimensions)) (second (first dimensions))))))
        (loop for item in (items new)
              for x from 0
              do 
              (setf (parent item) new)
              (setf (index item) x))
        (set-neighbours new)
        new)
    (let ((atom (if (functionp atom-function)
                    (funcall atom-function)
                  (if (and (listp atom-function)
                           (= (length atom-function) 2)
                           (every #'numberp atom-function))
                    (make-h-number nil atom-function)
                    atom-function))))
      (if (is-hyper atom)
          atom
        (mki 'h-atom :value atom)))))


(defmacro prev-chord (&rest rest)
  `(below ,@rest))

(defmacro next-chord (&rest rest)
  `(above ,@rest))

(defmethod set-neighbours ((self h-atom))
  (setf (below self)
        (when (parent self)
          (unless (= (index self) 0)
            (nth (1- (index self)) (items (parent self))))))

  (setf (above self)
        (when (parent self)
          (unless (= (index self) (1- (length (items (parent self)))))
            (nth (1+ (index self)) (items (parent self))))))

  (setf (prev self)
        (when (parent self)
          (when (below (parent self))
            (nth (index self) (items (below (parent self)))))))

  (setf (next self)
        (when (parent self)
          (when (above (parent self))
            (nth (index self) (items (above (parent self)))))))
  self)


(defmethod set-neighbours :after ((self h-list))
  (loop for item in (items self)
        do 
        (setf (parent item) self)
        (set-neighbours item)))

(defmethod clear-relations ((self h-atom))
  (setf (above self) nil (below self) nil (prev self) nil (next self) nil (parent self) nil)
  self)


(defmethod clone-object ((object h-atom))
  (set-neighbours (call-next-method (clear-relations object))))


(defmethod tree ((self h-atom))
  (value self))

(defmethod tree ((self h-list))
  (loop for item in (items self)
        collect (tree item)))
