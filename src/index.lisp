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

(defmethod terminal-test-server ()
  (om::om-term-cmd "curl localhost:32794"))

(defmethod test-js-fn ((js-function-string string))
  (let ((payload (format nil 
                         "curl -X POST localhost:32794/test-function -H \"Content-Type: application/json\" -d '{ \"fnString\": \"~a\" }'"
                         js-function-string)))
    (print payload)
    (om::om-term-cmd payload)
   ))

(defmethod run-script ((script-name string)) 
  (let ((path (namestring (make-pathname 
                           :directory (pathname-directory (server-directory)) 
                           :name script-name :type "sh"))))
    (om-terminal (om::string+ "sh " path))))


(defconstant +crlf+ (coerce (list #\Return #\Linefeed) 'string))

(defun send-get-request (stream host path)
  (format stream "GET ~A HTTP/1.0~AHost: ~A~A~A"
    path +crlf+ host +crlf+ +crlf+)
  (finish-output stream)
)

(defmethod test-server ()
  (with-open-stream (http (comm:open-tcp-stream 
                         "localhost" 32794))
    (send-get-request http "localhost" "/")
    (read-body http)))

(defun read-body (stream)
  "Read a TCP stream and return the JSON body as a string."
  ;; http standard is that an empty line separates the headers from the body
  (let ((body "")
        (reading-headers t))
    (loop
      for line = (read-line stream nil nil)
      while line
      do
        (unless reading-headers (setf body (concatenate 'string body line)))
        (when (= (length line) 1)   
          (setf reading-headers nil))
    finally (return body))))



    





