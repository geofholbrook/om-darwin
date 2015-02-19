(in-package om)


;;; main method ;;;;;;;;

(defmethod! evolute ((model t) (criterion function) (generations number))
  :initvals (list nil nil 100)
  :icon 701
  (d::run model criterion generations))

;;;;;;;;;;;


(defmethod Objfromobjs ((self dwn::specimen) (type voice))
  (dwn::arrange->voice (dwn::phenotype self)))

(defmethod Objfromobjs ((self dwn::specimen) (type poly))
  (dwn::arrange->poly (dwn::phenotype self)))
