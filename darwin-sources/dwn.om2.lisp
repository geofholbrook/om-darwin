(in-package om)


;;; main method ;;;;;;;;

(defmethod! evolute ((model t) (criterion function) (generations number))
  :initvals (list nil nil 100)
  :icon 701
  (d::run model criterion generations))

;;;;;;;;;;;

(defmethod Objfromobjs ((self dwn::specimen) (type tonal-object))
  (dwn::finalize self))

;;;;;;;;


(defvar *gene-counter* nil)
(defvar *raw-buffer* nil)
(defvar *om-gene-mode* :random)    ;;;; {:random :test :buffer}

(defvar *raw-buffer-lock* nil)
(setf *raw-buffer-lock* (mp::make-lock :name "gene-lock"))

(defmacro with-om-gene-mode (mode &body body)
  (let ((tmp (gensym)))
    `(mp::with-lock (*raw-buffer-lock*)
       (let ((,tmp *om-gene-mode*))
         (setf *om-gene-mode* ,mode)
         (prog1
             (handler-bind
                 ((error #'(lambda (err)
                             (setf *om-gene-mode* ,tmp)
                             (capi::display-message "An error of type ~a occurred: ~a" (type-of err) (format nil "~A" err))
                             (abort err))))
               ,@body)
           (setf *om-gene-mode* ,tmp))))))
  

(defparameter *number-of-tests* 10)

(defmethod! om-gene (min max &optional (step 1) floatp)
  :initvals '(0 1 1 nil) :indoc '("minimum value" "maximum value" "step-size" "floating point") 
  :icon 706

  (when (equalp *om-gene-mode* :test)
    (incf *gene-counter*))

  (let ((nucleo (if (member *om-gene-mode* '(:random :test))
                    (rrnd (d::get-param :gene-range))
                  (if *raw-buffer*
                      (pop *raw-buffer*)
                    (error "Error: raw genotype buffer depleted. Raw genotype size should be determinate.")))))
    (d::mod-to-range nucleo (list min max) step floatp)))


(defmethod! nth-gene (lis)
  :icon 706
  (nth (om-gene 0 (1- (length lis))) lis))
               

(defclass d::om-specimen (d::specimen) 
  ((om-function :initform nil :accessor om-function))
  (:metaclass d::species))

(defmethod omNG-copy ((self d::om-specimen))
  `(let ((copy ,(call-next-method)))
     (setf (om-function copy) ,(om-function self))   ;;; seems to work ... obviously won't work for saving though
     copy))

(defmethod d::phenotype ((self d::om-specimen))
  (when (om-function self)
    (with-om-gene-mode :buffer
      (setf *raw-buffer* (d::raw-genotype self))
      (funcall (om-function self)))))

(defun count-gene-calls (fun)
  (with-om-gene-mode :test
    (loop repeat *number-of-tests*
          with result
          do 
          (setf *gene-counter* 0)
          (funcall fun)
          (if result
              (when (not (= result *gene-counter*))
                (error "Error: om-gene called an indeterminate amount of times (redesign the phenotyper)"))
            (setf result *gene-counter*))

          finally return result)))

(defun raw-from-function (fun)
  (d::random-raw-genotype (count-gene-calls fun)))

(defmethod! define-species ((fun function))
  :icon 703
  (let ((spec (mki 'd::om-specimen 
                   :raw (d::random-raw-genotype (count-gene-calls fun))
                   )))
    (setf (om-function spec) fun)
    spec))


  



 
               




  

 
