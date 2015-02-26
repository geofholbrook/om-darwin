(in-package dwn)


;; multi-cell

(defclass multi-cell (specimen)
  ((cells :initform nil :initarg :cells :accessor cells)
   (concatenator :initform nil :initarg :concatenator :accessor concatenator))
  (:metaclass species))

(defmethod species-concatenator ((self t)) #'append)

(defmethod initialize-instance :after ((self multi-cell) &rest rest)
  (setf (cells self)
        (mapcar #'randomize-specimen (cells self)))

  (setf (raw-genotype self)
        (reduce #'append (mapcar #'raw-genotype (cells self)))))


(defmethod update-geno ((self multi-cell))
  (loop for cell in (cells self)
        for raw in (group-list (raw-genotype self)
                               (mapcar 'length (mapcar 'raw-genotype (cells self)))
                               'linear)
        do 
        (setf (raw-genotype cell) raw)
        (update cell)))


(defmethod phenotype ((self multi-cell))
  (reduce (or (concatenator self)
              (species-concatenator (car (cells self))))    ;;; use first cell, for now 
          (mapcar 'phenotype (cells self))))

(om::defmethod! om::make-multi ((cells list) &optional concatenator)
                :icon 703
                (mki 'multi-cell
                     :cells cells
                     :concatenator concatenator))

(defmethod finalize ((self multi-cell))
  (let ((dummy (mki (type-of (first (cells self))))))
    (setf (slot-value dummy 'pheno) (phenotype self))     ;;; quietly change the phenotype
    (finalize dummy)))
    







  