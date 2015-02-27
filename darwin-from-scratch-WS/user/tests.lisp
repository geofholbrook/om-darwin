(in-package dwn)


;inheritance test (good so far)
(defspecies melody2 (melody))

(defvar *pop* nil)
(defvar *ff* nil)

;multi-cell test

(defun test-stuff ()

(let ((model0 (mki 'melody))

      (model1 (mki 'multi-cell :cells (loop repeat 3 collect (mki 'melody))))
 
      (model2 (mki 'multi-cell :cells (loop repeat 3 collect (om::make-ga-tree 100 '(2 6) '(4 4)))))

      (ff-melody #'(lambda (spec) (loop for region in (phenotype spec)
                                        sum (within-M (region-pitch region) '(60 63)))))

      (ff-tree #'(lambda (spec)
                   (abs (- (length (om::tree2ratio (phenotype spec)))
                           8)))))
  (let* ((model model1)
         (ff ff-melody)
         (pop (population-from-model model ff)))

    (unless (= (caar pop)
               (evaluate (cadar pop) ff))
      (break "population initialization fail: stored fitness does not match actual fitness!"))

    (print "initialization OK")

    (loop repeat 50
          do

          (setf pop (iterate pop ff))
          (update (cadar pop))
        
          (print (caar pop))

          (unless (= (caar pop)
                     (evaluate (cadar pop) ff))
            (setf *pop* pop)
            (setf *ff* ff)
            (break "population iteration fail: stored fitness does not match actual fitness!")))

    (print "basic iteration OK")
  

    (let ((ga (mki 'ga-engine :model model1 :fitness-function ff))
          process)
    
      (print "running GA-ENGINE process")

      (setf process (run-process (om::string+ "GA PROCESS " (prin1-to-string 
                                                             (incf *process-counter*)))
                                 #'(lambda ()
                                     (run-engine ga))))

      (sleep 5)

      (setf (message-flag ga) :stop)

      (unless (= (caar (population ga))
                 (evaluate (cadar (population ga)) ff))
        (setf *pop* (population ga))
        (setf *ff* ff)
        (break "ga-engine fail: stored fitness does not match actual fitness!")))

    (print "GA-ENGINE test OK")
    t))
)
  

