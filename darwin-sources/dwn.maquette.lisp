(in-package om)

;;; call-next-method seems to defer to the method for patchpanel! 
;(defmethod handle-key-event :around ((self MaquettePanel) char) 
;l  (case char
;l    (#\G (maq-toggle-evolution self))
;    (otherwise (call-next-method))))

;;; so i have to copy the whole method

(defmethod handle-key-event ((self MaquettePanel) char) 
   (case char
     (#\SPACE
      (editor-play/stop (editor self)))
     (:om-key-esc (reset-cursor self))
     (#\x (mute-boxes self))
     (#\p (editor-play (editor self)))
     (#\g (setf (grille-p self) (not (grille-p self)))
          (om-invalidate-view self))
     (#\s (editor-stop (editor self)))
     (#\z (lock-boxes self))
     (#\I (mapc 'reinit-contents (get-actives self)) 
          (reinit-connections self))
     (#\C (maquette-color self))
     (#\c (if (get-actives self) (maquette-color-boxes self)
            (call-next-method)))
     (#\a (mapc 'internalize-patch (get-actives self)))
     (#\y (align-boxes self))
     (#\q (memesize-boxes self))
     (#\t (time-align self))
     (#\v  (om-eval-enqueue 
             `(progn
                (setf *cur-eval-panel* ,self)
                (mapc 'eval-box ',(get-actives self))
                (clear-ev-once ,self))
             self
             ))
     (#\V (om-eval-enqueue  
           `(eval-maquette ,self) self))

     (#\G (maq-toggle-evolution self))
     
     (otherwise (call-next-method))))


(defmethod maq-toggle-evolution ((self MaquettePanel))
  (let ((boxes (print (get-actives self 'tempobjframe))))
    (loop for box in boxes do 
          (let ((box-connected-to-tempout
                 (first (connected? 
                         (first (inputs (find-if #'(lambda (b)
                                                     (equalp (type-of b) 'omtempout))
                                                 (boxes (reference (object box))))))))))
            ;(print box-connected-to-tempout)
            (when (d::is-ga-box-p box-connected-to-tempout)
              (let ((engine (value box-connected-to-tempout)))
                (if (print (d::running engine))
                    (d::stop engine)
                  (d::start engine))))))))

;;;;; have to do this:
;;;;; get-mus-ob !!




(defmethod is-editorwindow ((self editorwindow)) t)
(defmethod is-editorwindow ((self t)) nil)

(defun engine-in-front-maquette (engine)
  (let ((front (om-front-window)))
    (when (is-editorwindow front)
      (when (equalp (type-of (obj front)) 'ommaquette)
        (let ((frame (first (frames (find engine
                                          (boxes (object (editorframe (obj front))))
                                          :key #'(lambda (b) (first (value b))))))))
          (redraw-frame frame)
          (omg-select frame))
        ))))
  
