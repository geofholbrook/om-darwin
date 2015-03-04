(in-package dwn)

(defun run-tests (model ff)
  (let ((pop (population-from-model model ff)))

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
  

    (let ((ga (mki 'ga-engine :model model :fitness-function ff))
          process)
      (print "model fitness:")
      (print (evaluate model ff))
      (print "running GA-ENGINE process")

      (setf process (run-process (om::string+ "TEST GA PROCESS" (prin1-to-string 
                                                             (incf *process-counter*)))
                                 #'(lambda ()
                                     (run-engine ga))))

      (sleep 5)

      (setf (message-flag ga) :stop)

      (setf *pop* (population ga))
      (setf *ff* ff)

      (unless (= (caar (population ga))
                 (evaluate (cadar (population ga)) ff))
        (break "ga-engine fail: stored fitness does not match actual fitness!")))

    (print "end fitness")
    (print (caar *pop*))
    (print "GA-ENGINE test OK")
    *pop*))


;useful catch-all
(defun fff (x) 
  #'(lambda (spec) (abs (- (length (flat (phenotype spec)))
                           x))))

