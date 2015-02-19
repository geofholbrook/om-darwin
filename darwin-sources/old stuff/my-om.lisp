
(in-package om)

(defmethod x->dx ((self list))
  :initvals (list 0 1)
  :indoc '("a list of numbers")
  :icon 192
  :doc "computes a list of intervals from a list of points."
  (loop for x in self
        for y in (rest self)
        collect (- y x)) )

(defmethod dx->x ((start number) (list list))
  :initvals (list 0 (list 1 1))
  :indoc '("a number" "a list of numbers")
  :icon 192
  :doc "computes a list of points from a list of intervals and a <start> point"
  (cons start (loop for dx in list
                    sum dx into thesum
                    collect (+ start thesum)) ))