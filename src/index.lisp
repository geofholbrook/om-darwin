(in-package dwn)
(setf lp *load-pathname*)
(defvar *darwin-server* nil)

(defun source-directory () (make-pathname :directory (pathname-directory lp)))

(defun server-directory ()
  (make-pathname 
   :directory (append (pathname-directory (source-directory)) (list "server"))))

(defun start-server ()
  (om-run-process "server-process" #'(lambda () (run-script "start"))))

(defun stop-server ()
  (run-script "stop"))

(defmethod run-script ((script-name string)) 
  (let ((path (namestring (make-pathname 
                           :directory (pathname-directory (server-directory)) 
                           :name script-name :type "sh"))))
    (om-terminal (om::string+ "sh " path))))

(defmethod test-function ((js-function-string string))
  (let ((payload (format nil "{ \"fnString\": \"~a\" }" js-function-string)))
    (with-open-stream (http (comm:open-tcp-stream 
                          "localhost" 32794))
      (send-post-request http "localhost" "/test-function" payload)
      (read-body http))))

(defmethod test-server ()
  (with-open-stream (http (comm:open-tcp-stream 
                         "localhost" 32794))
    (send-get-request http "localhost" "/")
    (read-body http)
    ))



    





