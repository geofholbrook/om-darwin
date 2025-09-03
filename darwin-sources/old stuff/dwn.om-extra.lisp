(in-package om)


;;;;;;;;; patch inspector
;;;
;;; easy way to inspect an OM patch obj ... evaluate this in a patch window
;;;

(defmethod! inspect-this-patch () 
  :icon 750
  ())

(defclass om-inspect-box (omboxcall) ())
(defmethod get-boxcallclass-fun ((self (eql 'inspect-this-patch))) 'om-inspect-box)

(defmethod om::omNG-box-value ((self om-inspect-box) &optional (numout 0))
  (inspect (mycontainer self)))



(defmacro internalize-om-slots (class-1 slot-names)
  (let* ((class (find-class class-1)) 
         (class-symbol (class-name class))
         (names '())
         (defaults '())
         (docs '())
         (menus '()))
    
    (print class-symbol)
    
    (loop for slot in (get-all-initargs-of-class class)
          for name = (slot-value slot 'ccl::name)
          for initform = (slot-value slot 'ccl::initform)
          unless (member (print name) slot-names)
          do (progn
               (push name names)
               (push initform defaults)
               (push initform docs)
               (push nil menus)))
    
    `(defmethod om::get-slot-in-out-names ((self ,class-symbol))
       (values ',(cons 'self (nreverse names))
               ',(cons nil (nreverse defaults))
               ',(cons nil (nreverse docs))
               ',(cons nil (nreverse menus))))))



(defun stop-player! ()
  (stop-player *general-player*))

(defun play-chord (notes) 
  (stop-player!)
  (play (mki 'chord
             :lmidic notes
             :ldur '(1000))))
              


;;;;;;;;;;;;;;;;;;
;;;; this code alters chord-seq editors so that holding 'P' and clicking will play the
;;;; notes currently sounding

(defmethod ms->offset ((self chord-seq) ms)
"just returns an equivalent value, doesn't modify self the way om::offset->ms and
 om::extent->ms seem to do..."
  (* (/ ms 1000) (/ (qtempo self) 60) (qvalue self)))

(defmethod notes-sounding-at-x ((self chord-seq) x1)
  ;(inspect self)
  (let ((x (ms->offset self x1)))
    (remove-if #'null
               (flat (loop for chord in (inside self)
                           while (<= (offset chord) x)
                           collect (let ((onset (offset chord)))
                                     (loop for note in (inside chord)
                                           if (>= (+ onset (ms->offset
                                                            self (dur note)))
                                                  x)
                                           collect (midic note)))))))
  )
                               
                        
(defmethod handle-key-event ((self scorePanel) (char (eql #\p)))
  (let ((ms (pixel-toms (panel self) 
                        (if (>= *om-version* 5.0)
                          (om-mouse-position (panel self))
                          (view-mouse-position (panel self)))))
        (obj (object (view-container self))))
    (play-chord (notes-sounding-at-x obj ms))))

;;;;;;;;;;;;;;



(defun black-circle ()
 '�)

(defun wait (&optional (sixtieths 1))
  (process-wait-with-timeout "" sixtieths #'(lambda () nil)))

;----------------------------
; max-style slider ... not done yet.

(defclass! maxslider ()
  ((n :initform 0 :initarg :n :accessor n)
   (range :initform '(0 127) :initarg :range :accessor range)))

;------------------------------

(defclass! numslider ()
  ((n :initform 0 :initarg :n :accessor n)))

;;; pretty nice interface, much like a Max number box except that you have to double click to edit,
;;; and so far there's no type-in editing

(defmethod class-has-editor-p ((self numslider)) t)
(defmethod get-editor-class ((self numslider)) 'numsliderEditor)

(defun unsigned-expt (base power)
  (if (> base 0)
    (expt base power)
    (- (expt (- base) power))))


(defmethod make-editor-window ((class (eql 'numsliderEditor)) object name ref &key 
                                     winsize winpos (close-p t) (winshow t) 
                                     (wintype :document))
;;; i guess this hijacks the method associated with a double-click
;;; maybe there's a way to simple redefine the double-click handler?
;;; anyway this is easy.
  (do ((original-value (n object))
       (original-mouse-pos (point-v (view-mouse-position (car (frames ref)))))
       (mouse-pos 0)) ((not (mouse-down-p)))
    (wait 5)
    (let ((poll (point-v (view-mouse-position (car (frames ref))))))
      (unless (= mouse-pos poll)
        (setf mouse-pos poll)
        (setf (n object) (round (+ original-value (unsigned-expt (- original-mouse-pos mouse-pos) 1.1))))
        (view-draw-contents (car (frames ref))))))
        
  nil)


;-------------------------------

(defclass! toggle ()
  ((x :initform nil :initarg :x :accessor x))
  (:icon 141))

;;; true / nil toggle, double click to activate.


(defmethod class-has-editor-p ((self toggle)) t)
(defmethod get-editor-class ((self toggle)) 'toggleEditor)

(defclass toggleEditor () ()) ; is this even necessary?

(defmethod make-editor-window ((class (eql 'toggleEditor)) object name ref &key 
                                  winsize winpos (close-p t) (winshow t) 
                                  (wintype :document))

  
  (setf (x object) (not (x object)))
  (update-from-reference ref)
  nil)

;---------------------------------


(defun om-sort (lis &optional (mode 'up))
  (sort lis (case mode
              (up #'<)
              (down #'>))))

(defun om-bpf->points (bpf)
  (mat-trans (list (om-scale (x-points bpf) 0 1)
                   (y-points bpf))))

(defun om-temper (something)
  (if (listp something)
    (mapcar #'(lambda (x) (* (round (/ x 100)) 100)) 
            something)
    (* (round (/ something 100)) 100)))

