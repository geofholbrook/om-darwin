
(in-package dwn)

(defparameter *ga-process* nil)

(om::defclass! ga-engine (oa::om-cleanup-mixin)
  ((current-best :initform nil :accessor current-best)
   (population :initform nil :accessor population)
   (message-flag :initform nil :accessor message-flag)
   (generation :initform 0 :accessor generation)
   (box :initform nil :accessor box)
   (process :initform nil :accessor process)

   ;visible slots
   (model :initform nil :initarg :model :accessor model)
   (fitness-function :initform nil :initarg :fitness-function :accessor fitness-function)
   (max-generations :initform 100 :initarg :max-generations :accessor max-generations))
   
  (:icon 701))


(defclass ga-engine-box (om::omboxeditcall)
  ((process :initform nil :accessor process)))

(defmethod om::get-type-of-ed-box ((self ga-engine)) 'ga-engine-box)

;(defmethod om::omng-remove-element ((self ga-engine-box) elem)
;   (print "does-this-work") self)

;(defmethod oa::om-cleanup ((self ga-engine))
;  (when (process (box self)) 
;    (print "KILLING PROCESS")
;    (oa::om-kill-process (process self))))

(defmethod update-best-candidate ((self ga-engine))
  (setf (current-best self)
        (if (population self)
            (arrange->poly (phenotype (cadar (population self))))
          (make-instance 'om::poly))))

(defmethod initialize-engine ((self ga-engine))
  (when (and (model self)
             (fitness-function self))
    (setf (population self)
          (population-from-model (model self) (fitness-function self))))
  (update-best-candidate self)
  (redraw-editors self))


(defmethod initialize-instance :after ((self ga-engine) &rest args)
  (print 'init-instance)
  (setf (box self) (get-my-box self))
  (initialize-engine self))


;(defmethod om::class-has-editor-p ((self ga-engine)) t)
;(defmethod om::get-editor-class ((self ga-engine)) 'om::polyeditor)

(defmethod om::default-edition-params ((self ga-engine))
  (om::default-edition-params (current-best self)))

(defmethod om::editor-object-from-value ((self ga-engine)) (current-best self))

(defmethod om::draw-mini-view ((self t) (value ga-engine))
  (om::draw-mini-view self (current-best value)))





(defmethod set-fitness-function ((self ga-engine) (fitness-function function))
  (setf (fitness-function self) fitness-function)
  (setf (message-flag self) :new-fitness-function)) 

(defmethod start ((self ga-engine))
  (if (process self) (om-kill-process (process self)))
  (setf (process self)
        (om-run-process "GA PROCESS"
                        #'(lambda ()
                            (run-engine self)))))

(defmethod stop ((self ga-engine))
  (setf (message-flag self) :stop))

(defmethod reinit ((self ga-engine))
  (if (or (null (process self))
          (equal (mp::process-state (process self))
                 :killed))
      (initialize-engine self)
      
    (setf (message-flag self) :reinit)))


; searches all boxes of all patch windows ...
(defun get-my-box (obj)
  (let ((patches (remove-if-not #'(lambda (obj) (equal (type-of obj) 'om::ompatch))
                                (mapcar 'om::obj (om::om-get-all-windows 'om::EditorWindow)))))
 
    (loop for patch in patches
          for box = (find obj (om::boxes patch)
                          :key 'om::value)

          if box 
          return box)))


(defmethod redraw-editors ((self ga-engine))
  (when (box self)
    (om::update-if-editor (box self))                         ;;; for open editor window
    (om::om-draw-contents (first (om::frames (box self))))))  ;;; for miniview


(defmethod run-engine ((self ga-engine))

  (unless (box self)
    (setf (box self) (get-my-box self)))  ;;; just to be sure

  (when (and (model self)
             (fitness-function self))
      
    (setf (message-flag self) nil)
    (setf (generation self) 0)
    
    ; for now, always reinitialize
    
    (loop until (equal (message-flag self) :stop)
          
          do

          (when (equal (message-flag self) :reinit)
            (setf (message-flag self) nil)
            (initialize-engine self))
              
          (when (equal (message-flag self) :new-fitness-function)
            (setf (message-flag self) nil)
              ;re-evaluate fitnesses
            (setf (population self)
                  (loop for entry in (population self)
                        collect (list (evaluate (second entry) (fitness-function self))
                                      (second entry)
                                      0))))

          (iterate (population self) 
                   (fitness-function self))

          (incf (generation self))

          (update-best-candidate self)

          (redraw-editors self)  ;;; causes the 'animation' of score editors

          )))
                



#| 
(defun run ()
  (setf *ga-process*
        (om-run-process "GA PROCESS"
                        #'(lambda () 
                            (loop for i = 4000 then (funcall #'iterator i) do (print i))))))


(om-kill-process *ga-process*)
|#
