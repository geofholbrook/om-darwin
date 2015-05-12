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

;;; not necessary?
(defmethod d::get-tempobjs ((self relationPanel))
  (remove-if-not #'(lambda (icon)
                     (subtypep (type-of icon) 'tempobjframe))
                 (om-subviews self)))

(defmethod find-connected-ga-box ((self tempobjframe))
  ;;; finds the first box connected to the tempout inside the represented patch
  (let ((connected-box (first (connected? 
                         (first (inputs (find-if #'(lambda (b)
                                                     (equalp (type-of b) 'omtempout))
                                                 (boxes (reference (object self))))))))))
    (when (d::is-ga-box-p connected-box)
      connected-box)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod maq-toggle-evolution ((self MaquettePanel))
  (let ((boxes 
         (get-actives self 'tempobjframe)))
    
    (loop for box in boxes
          for engine = (nth 0 (value (object box)))
          do
          (when (print engine)
            (if (d::running engine)
                  (d::stop engine)
                (d::start engine))))))


(defmethod get-obj-for-maquette-display ((self d::ga-engine)) 
  (d::result self)
)


;;;;;;;;;;;;;;;;;;;;;;;;
;;; not necessary?

(defmethod is-editorwindow ((self editorwindow)) t)
(defmethod is-editorwindow ((self t)) nil)

(defun engine-in-front-maquette (engine)
  (let ((front (om-front-window)))
    (when (is-editorwindow front)
      (when (equalp (type-of (obj front)) 'ommaquette)
        (let ((box (find engine
                         (boxes (object (editorframe (obj front))))
                         :key #'(lambda (b) (first (value b))))))
          (when box
            (let ((frame (first (frames box))))
              (redraw-frame frame)
             ; (when (om-view-container frame)
             ;  (omg-select frame))
            )))))))
  
