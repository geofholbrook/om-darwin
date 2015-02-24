
(in-package dwn)

(defparameter *ga-process* nil)

(om::defclass! ga-engine ()
  ((fitness-function :initform nil :accessor fitness-function)
   (population :initform nil :accessor population)
   (generation :initform 0 :accessor generation)

   (result :initform nil :accessor result)  ;finalized phenotype
   (message-flag :initform nil :accessor message-flag)
   
   (box :initform nil :accessor box)


   ;visible slots
   (model :initform nil :initarg :model :accessor model))
   
  (:icon 701))


(defmethod om::omNG-save ((self specimen) &optional (values? nil))
  `(let ((spec ,(call-next-method)))
     (setf (raw-genotype spec) ,(om::omng-save (raw-genotype self)))
     (update spec)
     spec))
     
(defmethod om::omNG-save ((self ga-engine) &optional (values? nil))
  `(let ((ga ,(call-next-method)))
     (setf (population ga) ,(om::omng-save (population self)))
     (setf (generation ga) ,(om::omng-save (generation self)))
     (setf (result ga) ,(om::omng-save (result self))) 
     ga))

(defmethod process ((self ga-engine)) 
  (when (box self)
    (process (box self))))

(defmethod set-process ((self ga-engine) process)
  (when (box self)
    (setf (process (box self)) process)))



(defclass ga-engine-box (om::omboxeditcall)
  ((process :initform nil :accessor process)))


(defmethod om::get-type-of-ed-box ((self ga-engine)) 'ga-engine-box)

(defmethod om::omng-remove-element :before ((self om::ompatch) (elem ga-engine-box))
  (when (process elem) 
    (print "KILLING PROCESS")
    (om-kill-process (process elem))))

(defmethod om::object-remove-extra :before ((obj ga-engine) box) 
  (print `(object-remove-extra ,obj ,box))
  (when (process box)
    (om-kill-process (process box))))

(defmethod om::omng-box-value :after ((self ga-engine-box) &optional (numout 0))
  (unless (and (equal (om::allow-lock self) "x") (om::value self))
    (when (process self) 
      (om-kill-process (process self)))))

(defmethod cleanup-process ((self t)) nil)
(defmethod cleanup-process ((self ga-engine-box))
  (when (process self) 
      (om-kill-process (process self))))

(defmethod om::load-abstraction-attributes :before ((self om::ompatch) currentpersistent)
  (declare (ignore currentpersistent))
  (mapcar #'cleanup-process (om::boxes self)))



;(defmethod oa::om-cleanup ((self ga-engine))
;  (when (process (box self)) 
;    (print "KILLING PROCESS")
;    (oa::om-kill-process (process self))))

(defmethod update-best-candidate ((self ga-engine))
  (setf (result self)
        (if (population self)
            (finalize (cadar (population self)))
          (make-instance 'om::poly))))


(defmethod randomize-population ((self ga-engine))
  (when (model self)
    (setf (population self)
          (population-from-model (model self) (or (fitness-function self)
                                                  #'(lambda (spec) (declare (ignore spec)) 0))))))

(defmethod initialize-engine ((self ga-engine))
  (randomize-population self)
  (update-best-candidate self)
  (redraw-editors self))

(defmethod initialize-instance :after ((self ga-engine) &rest args)
  (initialize-engine self))


(defmethod om::class-has-editor-p ((self ga-engine)) t)
(defmethod om::get-editor-class ((self ga-engine)) 'om::polyeditor)

(defmethod om::default-edition-params ((self ga-engine))
  (om::default-edition-params (result self)))

(defmethod om::editor-object-from-value ((self ga-engine)) (result self))

(defmethod om::draw-mini-view ((self t) (value ga-engine))
  (om::draw-mini-view self (result value)))


(defmethod running ((self ga-engine))
  (and (process self)
       (not (equal (mp::process-state (process self))
                   :killed))))


(defmethod evaluate-population ((self ga-engine))
  (setf (population self)
        (loop for entry in (population self)
              collect (list (evaluate (second entry) (fitness-function self))
                            (second entry)
                            0))))

(defmethod set-fitness-function ((self ga-engine) (fitness-function function))
  (setf (fitness-function self) fitness-function)
  (if (running self)
      (setf (message-flag self) :new-fitness-function)
    (evaluate-population self)))      


(defvar *process-counter*)
(setf *process-counter* 0)

(defmacro run-process (name func &rest args)
   `(mp:process-run-function ,name '(:priority ,(or oa::*current-priority* 10))
                             (if (functionp ,func) ,func ',func) ,.args))

(defmethod start ((self ga-engine))

  (unless (box self)
    (setf (box self) (get-my-box self)))  ;;; just to be sure

  (if (process self) (om-kill-process (process self)))
  (set-process self
               (run-process (om::string+ "GA PROCESS " (prin1-to-string 
                                                       (incf *process-counter*)))
                            #'(lambda ()
                                (run-engine self)))))


(defmethod stop ((self ga-engine))
  (setf (message-flag self) :stop))


(defmethod reinit ((self ga-engine))
  (if (running self)
      (setf (message-flag self) :reinit)
    (initialize-engine self)))


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
    (let ((frame (when (and (box self)
                                     (om::frames (box self)))
                            (first (om::frames (box self))))))
      (when frame
        (om::om-draw-contents frame))))) ;;; for miniview



(defmethod run-engine ((self ga-engine))


  (when (and (model self)
             (fitness-function self))
      
    (setf (message-flag self) nil)
    (setf (generation self) 0)
    
    ; for now, always reinitialize
    
    (loop until (equal (message-flag self) :stop)
          
          do

          (when (equal (message-flag self) :reinit)
            (setf (message-flag self) nil)
            (randomize-population self))
              
          (when (equal (message-flag self) :new-fitness-function)
            (setf (message-flag self) nil)
            (evaluate-population self))

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
