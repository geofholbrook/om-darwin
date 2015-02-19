(in-package dwn)

(om::defmethod* om-gene ((low number) (high number) )
  :initvals '(0 1) :indoc '("min" "max") :icon 200 
  :doc "Returns a random number between <low> and <high>."
  (om-random low high))




