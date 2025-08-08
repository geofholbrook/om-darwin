
(defmethod ed-function-name ((w fred-mixin))
  (ed-set-view-font w '(12 :bold)))
 
(defmethod ed-bold ((w fred-mixin))
  (ed-set-view-font w '(:bold)))


(defmethod ed-orange ((w fred-mixin))
  (ed-set-view-font w '((:color-index 176))))

(defmethod ed-red ((w fred-mixin))
  (ed-set-view-font w '((:color-index 176))))

(defmethod ed-black ((w fred-mixin))
  (ed-set-view-font w '((:color-index 0))))

(defmethod ed-blue ((w fred-mixin))
  (ed-set-view-font w '((:color-index 162))))

(defmethod ed-monaco ((w fred-mixin))
  (ed-set-view-font w '("monaco" 9)))

                                     
(comtab-set-key *comtab* '(:shift :control #\Z) 'ed-blue)
(comtab-set-key *comtab* '(:shift :control #\B) 'ed-original)
(comtab-set-key *comtab* '(:shift :control #\R) 'ed-red)
(comtab-set-key *comtab* '(:shift :control #\O) 'ed-orange)
(comtab-set-key *comtab* '(:shift :control #\X) 'ed-bold)


(comtab-set-key *comtab* '(:meta :control #\I) 'ed-italic)
(comtab-set-key *comtab* '(:meta :control :shift #\P) 'ed-plain)
(comtab-set-key *comtab* '(:meta :control :shift #\B) 'ed-bold)
(comtab-set-key *comtab* '(:meta :control :shift #\S) 'ed-function-name)


; I wrote this to figure out the selection process for a population
; this is much more efficient than sorting!


#|
(defmacro select-numbers (newlist oldlist)
  `(loop for new in ,newlist
         do
         (unless (< new (first ,oldlist))
           (loop for old on ,oldlist
                 while (cdr old)
                 until (> (cadr old) new)
                 do (progn
                      (rplacd old (cons new (cdr old)))
                      (pop ,oldlist))))))
|#
