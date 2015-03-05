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

;;; finalize the multi-cell phenotype the same way a single cell would be finalized.
;;; if that's not gonna work you have to specialize multi-cell
(defmethod finalizer ((self multi-cell))
  (finalizer (first (cells self))))
    


;;;;


(defclass stack (multi-cell) ())    ;;; should probably inherit in the other direction ...

(om::defmethod! om::make-stack ((cells list))
                :icon 703
                (mki 'stack
                     :cells cells))

(defmethod phenotype ((self stack))
  (loop for arr in (mapcar 'phenotype (cells self))
        with highest-channel = 0
        append (let* ((minmax (loop for region in arr
                                    minimize (region-chan region) into min
                                    maximize (region-chan region) into max
                                    finally return (list min max)))

                      (offset (1+ (- (first minmax) 
                                     highest-channel))))

                 (incf highest-channel (1+ (- (second minmax)
                                              (first minmax))))

                 (if (= highest-channel 0)
                     arr
                   (loop for region in arr
                         collect `(,(region-start region)
                                   ,(region-len region)
                                   ,(+ (region-chan region) offset)
                                   ,@(nthcdr 3 region)))))))