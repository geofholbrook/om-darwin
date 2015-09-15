(in-package om)



(defun fooo ()
  (make-stack (list (define-species #'(lambda ()
                                        (d::make-even-melody (om* (om+ (om-gene 50 61) '(0 4 7 12)) 100) 1/16)))
                    (make-melody 4 '(6000 7200)))))
