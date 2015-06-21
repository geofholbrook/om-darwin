
(in-package dwn)



(om::defclass! ga-engine ()
  (
   (population :initform nil :accessor population)
   (generation :initform 0 :accessor generation)

   (result :initform nil :accessor result)  ;finalized phenotype
   (message-flag :initform nil :accessor message-flag)
   
   (box :initform nil :accessor box)
   (mini-view-mode :initform :result :accessor mini-view-mode)

   ;visible slots
   (model :initform nil :initarg :model :accessor model)
   (fitness-function :initform nil :initarg :fitness-function :accessor fitness-function))
   
  (:icon 701))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GA-ENGINE-BOX    (stores GA process)

(defmethod om::get-type-of-ed-box ((self ga-engine)) 'ga-engine-box)

(defclass ga-engine-container ()
  ((process :initform nil :accessor process)))
(defclass ga-engine-box (om::omboxeditcall ga-engine-container)
  ())


(defmethod is-ga-box-p ((self t)) nil)
(defmethod is-ga-box-p ((self ga-engine-box)) t)

(defmethod process ((self ga-engine)) 
  (when (box self) (process (box self))))
(defmethod set-process ((self ga-engine) process)
  (when (box self) (set-process (box self) process)))

(defmethod set-process ((self ga-engine-box) process)
  (setf (process self) process))


;;;;;;;;;
;;; hack for storing the process in a temporalbox

(defmethod process ((self om::temporalbox))
  (om::free-store self))

(defmethod set-process ((self om::temporalbox) process)
  (setf (om::free-store self) process))


;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;  specializing methods for all ways box might be removed or re-evaluated ... to prevent orphaned processes
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
;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; KEY INTERACTION 

(defmethod om::play-obj? ((self ga-engine)) t)
(defmethod om::get-obj-to-play ((self ga-engine-box)) (result (om::value self)))

(defmethod om::om-geof-keys ((self om::patchPanel) char) 
  (let ((actives (om::get-actives self)))
    (case char
      ;(#\E (om::om-encapsulate self actives))
      (#\L (om::om-funnel self actives))
      (#\Y (om::om-align self actives)
           (om::make-move-after self actives))
      (#\C (om::om-cascade self actives)))))

(defmethod om::handle-key-event :around ((self om::patchPanel) char) 
  (om::modify-patch self)
  (let* ((ga-frames (om::get-actives self 'ga-engine-frame)))
    (case char
      (#\g  (let* ((boxes (mapcar 'om::object ga-frames))
                   (vals (mapcar 'om::value boxes)))
              (if (some #'running vals)
                  (mapc #'stop vals)
                (mapc #'start vals))))

      (otherwise (progn 
                   (when (find-library "om-geof") (om::om-geof-keys self char))
                   (call-next-method self char))))))



;;;;;;;;;;;;;;;;;;;;
;;;; GA-EDITOR
      
(defclass ga-editor (om::EditorView om::object-editor) 
  ())

(defmethod om::editor-compatible-params-p ((ed1 om::scoreeditor) (ed2 ga-editor)) t)

(defmethod om::class-has-editor-p ((self ga-engine)) t)
(defmethod om::get-editor-class ((self ga-engine)) 'ga-editor)

(defmethod om::default-edition-params ((self ga-engine))
  (om::default-edition-params (result self)))


(defmethod om::draw-mini-view ((self t) (value ga-engine))
  (if (equalp (mini-view-mode value) :result)
      (om::draw-mini-view self (result value))
     (call-next-method)))


;;; toggle miniview hack
(defmethod om::get-frame-class ((self ga-engine-box)) 'ga-engine-frame)
(defclass ga-engine-frame (om::boxEditorFrame) ())
(defmethod om::change-edit-mode :after ((self ga-engine-frame))
  (when (om::showpict (om::object self))
    (let ((val (om::value (om::object self))))
      (setf (mini-view-mode val)
            (if (equalp (mini-view-mode val) :result)
                :engine
              :result))))) 



(defmethod initialize-instance :after ((self ga-editor) &rest initargs)
  (let ((win (om::make-editor-window (om::get-editor-class (result (om::object self))) 
                                     (result (om::object self)) "GA RESULT" self
                                     :close-p nil
                                     :wintype '(:toolbox))))
    (push win (om::attached-editors self)))
  (om-add-subviews self 
                   (om-make-dialog-item 'om-button (om-make-point 20 50) (om-make-point 80 20) "Start"
                                        :di-action #'(lambda (button)
                                                       (start (om::object (om-view-container button)))))

                   (om-make-dialog-item 'om-button (om-make-point 20 80) (om-make-point 80 20) "Stop"
                                        :di-action #'(lambda (button)
                                                       (stop (om::object (om-view-container button)))))

                   (om-make-dialog-item 'om-button (om-make-point 20 110) (om-make-point 80 20) "Reinit"
                                        :di-action #'(lambda (button)
                                                       (reinit (om::object (om-view-container button)))))

                   (om-make-dialog-item 'om-check-box (om-make-point 150 50) (om-make-point 200 20)
                                  "retain best candidate")
                                  ;:font *controls-font*

                   (om-make-dialog-item 'om-check-box (om-make-point 150 80) (om-make-point 200 20)
                                  "seed initial population")

                   (om-make-dialog-item 'om-static-text (om-make-point 150 120) (om-make-point 260 60)
                                  "selection method:")

                   (om-make-dialog-item 'om-pop-up-dialog-item
                                        (om-make-point 150 140)
                                        (om-make-point 150 20)
                                        "selection method"
                                        :range  (list "simple ranking" "roulette wheel" "tournament")
                                        :value nil 
                                        )


                   (om-make-dialog-item 'om-static-text (om-make-point 150 170) (om-make-point 260 60)
                                  "crossover method:")

                   (om-make-dialog-item 'om-pop-up-dialog-item
                                        (om-make-point 150 190)
                                        (om-make-point 150 20)
                                        "crossover method"
                                        :range  (list "single-point" "n-point" "uniform")
                                        :value nil 
                                        )


                   (om-make-dialog-item 'om-static-text (om-make-point 150 220) (om-make-point 260 60)
                                        "mutation method:")

                   (om-make-dialog-item 'om-pop-up-dialog-item
                                        (om-make-point 150 240)
                                        (om-make-point 150 20)
                                        "mutation method"
                                        :range  (list "single-point" "permutation" "custom")
                                        :value nil 
                                        )

                   (om-make-dialog-item 'om-check-box (om-make-point 150 270) (om-make-point 200 20)
                                  "simulated annealing")





                   )
  )

#| ;;; no longer necessary?
;;;; BAD HACK/FIX ;;;;;
; redefines an OM method
(in-package om)
(defmethod change-select-system ((self multiseqPanel) (obj simple-container) newsys) (call-next-method))
  (let ((oldpar (get-edit-param (om-view-container self) 'staff)))
    (loop for pos from 0 to (- (length (staff-sys self)) 1) do
          (setf (nth pos (staff-sys self)) (get-staff-system newsys))
          ;(setf (nth pos oldpar) newsys)
          )
    (setf oldpar (create-list (length (staff-sys self)) newsys))
    ;i think this is the same thing.
    
    (set-edit-param (om-view-container self) 'staff oldpar)))

(in-package dwn)
|#


;(defmethod om::editor-object-from-value ((self ga-engine)) (result self))

(defmethod om::update-if-editor :after ((self ga-engine-box))
  (when (om::editorFrame self)
    (om::update-editor-after-eval (om::editor (first (om::attached-editors (om::editorFrame self))))
                                  (result (om::value self)))))

(defmethod redraw-editors ((self ga-engine))
  ;(print 'what?)
  (when (box self)
    (om::update-if-editor (box self))                         ;;; for open editor window
 
    ;;; for miniview
    (let ((frame (when (and (box self)
                            (om::frames (box self)))
                   (first (om::frames (box self))))))
      (when frame
        (om::om-redraw-view frame)
        ))

    ;;; for front maquette
    ;(om::engine-in-front-maquette self)
))

;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod copy-attributes ((from t) (to t)) to)

(defmethod copy-attributes ((from om::bpf) (to om::bpf))
  (setf (om::bpfcolor to) (om::bpfcolor from))
  to)

(defmethod copy-attributes ((from om::bpc-lib) (to om::bpc-lib))
  (setf (om::bpf-list to) 
        (loop for bpcfrom in (om::bpf-list from)
              for bpcto in (om::bpf-list to)
              do 
              (setf (om::point-list bpcfrom) (om::point-list bpcto))
              collect bpcfrom))
  to)

(defmethod update-best-candidate ((self ga-engine))
  (let ((new-best (if (population self)
            (finalize (cadar (population self)))
                    (make-instance 'om::poly))))
    (setf (result self) (copy-attributes (result self) new-best))
    ))


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


;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;


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

(defmethod set-fitness-function ((self ga-engine) (fitness-function t))
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

  ;(om::lock-after-modif (car (om::frames (box self))))

  (when (and (equalp (type-of (box self)) 'om::temporalbox)
             (equalp (process self) 0))
    (set-process self nil)  ;;; free-store
    )

  (if (process self) (om-kill-process (process self)))

  (set-process self
               (run-process (om::string+ "GA PROCESS " (prin1-to-string 
                                                       (incf *process-counter*)))
                            #'(lambda ()
                                (run-engine self))))
)


(defmethod stop ((self ga-engine))
  (setf (message-flag self) :stop))


(defmethod reinit ((self ga-engine))
  (if (running self)
      (setf (message-flag self) :reinit)
    (initialize-engine self)))


; searches all boxes of all patch and maquette windows ...
(defun get-my-box (obj)
  (let ((patches (remove-if-not #'(lambda (obj) (member (type-of obj) '(om::ompatch om::ommaquette)))
                                (mapcar 'om::obj (om::om-get-all-windows 'om::EditorWindow)))))
 
    (loop for patch in patches
          for box = (find obj (om::boxes patch)
                          :key #'(lambda (box)
                                   (if (listp (om::value box))
                                       (nth 0 (om::value box))
                                     (om::value box))))

          if box 
          return box)))



(defmethod run-engine ((self ga-engine))

  (let ((prev))

    (when (and (model self)
               (fitness-function self))
      
      (setf (message-flag self) nil)
    ;(setf (generation self) 0)
    
    ; for now, always reinitialize
    
      (loop until (equal (message-flag self) :stop)
          
            do

            (incf (generation self))

            (when (equal (message-flag self) :reinit)
              (setf (message-flag self) nil)
              (setf (generation self) 0)
              (randomize-population self))
              
            (when (equal (message-flag self) :new-fitness-function)
              (setf (message-flag self) nil)
              (evaluate-population self))

            (setf (population self)
                  (iterate (population self) 
                           (fitness-function self)))     ;;; only sets raw genotypes
   
            (update (cadar (population self)))

            (unless nil (eql (cadar (population self))
                         prev)
              (setf prev (cadar (population self)))
              (update-best-candidate self)
              (redraw-editors self)  ;;; causes the 'animation' of score editors
              )
          
            ))))
                


(defmethod om::omNG-save ((self specimen) &optional (values? nil))
  `(let ((spec ,(call-next-method)))
     (setf (raw-genotype spec) ,(om::omng-save (raw-genotype self)))
     (update spec)
     spec))
     
(defmethod om::omNG-save ((self ga-engine) &optional (values? nil))
  `(let ((ga (mki 'ga-engine)))
     (setf (model ga) ,(om::omng-save (model self)))
     (setf (population ga) ,(om::omng-save (population self)))
     (setf (generation ga) ,(om::omng-save (generation self)))
     (setf (result ga) ,(om::omng-save (result self))) 
     ga))

(defmethod om::omNG-copy ((self specimen))
  `(let ((copy ,(call-next-method)))
     (setf (raw-genotype copy) ',(raw-genotype self))
     (update copy)
     copy))

(defmethod om::omNG-copy ((self ga-engine))
  `(let ((copy (make-instance ',(type-of self))))
     (setf (population copy) ,(om::omng-copy (population self)))
     (setf (generation copy) ,(generation self))
     (setf (result copy) ,(om::omng-copy (result self)))
     copy))

#| 
(defun run ()
  (setf *ga-process*
        (om-run-process "GA PROCESS"
                        #'(lambda () 
                            (loop for i = 4000 then (funcall #'iterator i) do (print i))))))


(om-kill-process *ga-process*)
|#
