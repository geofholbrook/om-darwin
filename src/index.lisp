(in-package dwn)
(setf lp *load-pathname*)
(defvar *darwin-server* nil)

(defun reset-darwin-definitions ()
  (do-symbols (s (find-package 'dwn)) 
    (if (string-equal (package-name (symbol-package s)) "om-darwin") 
        (unintern s))))

(defun source-directory () (make-pathname :directory (pathname-directory lp)))

(defun server-directory ()
  (make-pathname 
   :directory (append (pathname-directory (source-directory)) (list "server"))))

(defun start-server ()
  (om-run-process "server-process" #'(lambda () (run-script "start"))))

(defun stop-server ()
  (run-script "stop"))

(defmethod test-server ()
  (om::om-term-cmd "curl localhost:32794"))

(defmethod test-js-fn ((js-function-string string))
  (let ((payload 
    (string+ 
      "curl localhost:32794/function -H \"Content-Type: application/json\""
      " -d '{ \"fnString\": \"" 
      js-function-string 
      ", \"arguments\": [] }'"
      )))
    (print payload)
    (om::om-term-cmd payload)
   ))

(defmethod run-script ((script-name string)) 
  (let ((path (namestring (make-pathname 
                           :directory (pathname-directory (server-directory)) 
                           :name script-name :type "sh"))))
    (om-terminal (om::string+ "sh " path))))





    


