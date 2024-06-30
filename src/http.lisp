(in-package dwn)

(defconstant +crlf+ (coerce (list #\Return #\Linefeed) 'string))

(defun send-get-request (stream host path)
  (format stream "GET ~A HTTP/1.0~AHost: ~A~A~A"
    path +crlf+ host +crlf+ +crlf+)
  (finish-output stream))

(defun send-post-request (stream host path payload)
  (format stream "POST ~A HTTP/1.1~AContent-Type: application/json~AHost: ~A~AContent-Length: ~A~A~A~A"
                    path +crlf+ +crlf+ host +crlf+ (prin1-to-string (length payload)) +crlf+ +crlf+ payload)
  (finish-output stream))

(defun read-body (stream)
  """
    Read a TCP stream and return the JSON body as a string.
    Problem: I can't get this to work with a body longer than a single line,
      so it just takes the first line of the body
      otherwise it hangs waiting for another line (EOF doesn't seem to function?)
  """
  ;; http standard is that an empty line separates the headers from the body
  (let ((body "")
        (reading-headers t)
        (counter 0))
    (loop
      while (= counter 0)
      for line = (read-line stream nil nil)
      while line
      do
        (unless reading-headers 
          (setf body (concatenate 'string body line))
          (setf counter (1+ counter)))
        (when (= (length line) 1)
          (setf reading-headers nil))
    finally (return body))))