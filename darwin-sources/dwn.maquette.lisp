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
  (let ((boxes 
         (get-actives self 'tempobjframe)))
    
    (loop for box in boxes
          for engine = (nth 0 (value (object box)))
          do
          (when engine
            (if (d::running engine)
                  (d::stop engine)
                (d::start engine))))))


(defmethod get-obj-for-maquette-display ((self d::ga-engine)) 
  (d::result self))

;;;; for use with temporal input (first outlet)
;;
(defmethod get-temporal-siblings ((self temporalbox))
  (when (mycontainer self)
    (remove self
            (boxes (mycontainer self)))))

(defmethod get-maq-overlaps ((self temporalbox))
  (when (value self)
    (let ((siblings (get-temporal-siblings self))
          (this (d::phenotype (cadar (d::population (car (value self)))))))
      (let ((start (d::region-start (car this)))
            (end (d::region-end (last-elem this)))
            (beats-per-ms (/ (qtempo (d::result (car (value self))))
                             60000 4)))
        
        (loop for sibling in siblings
              append
              (d::arr-select (d::arr-time-shift 
                              (d::phenotype (cadar (d::population (car (value sibling)))))
                              (* (- (offset sibling)
                                    (offset self)) beats-per-ms))
                             start
                             end)
              )))))

        


