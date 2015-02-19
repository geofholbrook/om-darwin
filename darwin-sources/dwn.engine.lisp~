

(in-package dwn)

(defparameter *ga-process* nil)

(om::defclass! ga-engine (oa::om-cleanup-mixin)
  ((current-best :initform nil :accessor current-best)
   (process :initform nil :accessor process)
   (population :initform nil :accessor population)
   (message-flag :initform nil :accessor message-flag)
   (generation :initform 0 :accessor generation)

   ;visible slots
   (model :initform nil :initarg :model :accessor model)
   (fitness-function :initform nil :initarg :fitness-function :accessor fitness-function)
   (max-generations :initform 100 :initarg :max-generations :accessor max-generations))
   
  (:icon 701))

(defmethod oa::om-cleanup ((self ga-engine))
  (when (process self) 
    (print "KILLING PROCESS")
    (oa::om-kill-process (process self))))

(defmethod update-best-candidate ((self ga-engine))
  (setf (current-best self)
        (if (population self)
            (arrange->poly (phenotype (cadar (population self))))
          (make-instance 'om::poly))))


(defmethod initialize-instance :after ((self ga-engine) &rest args)
  (when (and (model self)
             (fitness-function self))
    (setf (population self)
          (population-from-model (model self) (fitness-function self))))
  (update-best-candidate self))


;(defclass ga-scoreeditor (om::polyeditor) ())

(defmethod om::class-has-editor-p ((self ga-engine)) t)
(defmethod om::get-editor-class ((self ga-engine)) 'om::polyeditor)

(defmethod om::default-edition-params ((self ga-engine))
  (om::default-edition-params (current-best self)))

(defmethod om::editor-object-from-value ((self ga-engine)) (current-best self))

;;; open editor with current-best
;(defmethod om::make-editor-window ((class (eql 'ga-scoreeditor)) (object ga-engine) name ref &key 
;                                   winsize winpos (close-p t) (winshow t) (resize t) (retain-scroll nil)
;                                   (wintype nil))
;  (call-next-method class (current-best object) name ref 
;                    :winsize winsize :winpos winpos :resize resize 
;                    :close-p close-p :winshow winshow :resize resize
;                    :retain-scroll retain-scroll :wintype wintype))

;(defmethod om::object ((self ga-scoreeditor))
;  (current-best (slot-value self 'om::object)))

;(defmethod om::inside ((self ga-engine))
;  (om::inside (current-best self)))

;(defmethod om::voices ((self ga-engine))
;  (om::voices (current-best self)))




(defmethod set-fitness-function ((self ga-engine) (fitness-function function))
  (setf (fitness-function self) fitness-function)
  (setf (message-flag self) :new-fitness-function)) 

(defmethod stop ((self ga-engine))
  (setf (message-flag self) :stop))

(defmethod start ((self ga-engine))
  (if (process self) (om-kill-process (process self)))
  (setf (process self)
        (om-run-process "GA PROCESS"
                        #'(lambda ()
                            (run-engine self)))))


(defun get-my-box (obj)
  (let* ((editor-windows (om-get-all-windows 'om::EditorWindow))
         (my-win (find obj editor-windows :key #'(lambda (w) (om::ref (om::editor w)))
                      :test #'(lambda (o box) (when box (equal o (om::value box)))))))
    (when my-win (om::ref (om::editor my-win)))))
  
(defmethod run-engine ((self ga-engine))
  (let ((my-box (get-my-box self)))

    (when (and (model self)
               (fitness-function self))
      
      (setf (message-flag self) nil)
      (setf (generation self) 0)
    
    ; for now, always reinitialize
    
      (loop until (equal (message-flag self) :stop)
          
            do 


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
    
            (om::update-if-editor my-box)
            ))))
                



#| 
(defun run ()
  (setf *ga-process*
        (om-run-process "GA PROCESS"
                        #'(lambda () 
                            (loop for i = 4000 then (funcall #'iterator i) do (print i))))))


(om-kill-process *ga-process*)
|#
