(in-package om)


;;; main method ;;;;;;;;

(defmethod! evolute ((model t) (criterion function) (generations number))
  :initvals (list nil nil 100)
  :icon 701
  (d::run model criterion generations))

;;;;;;;;;;;




(defmethod Objfromobjs ((self dwn::specimen) (type tonal-object))
  (dwn::finalize self))