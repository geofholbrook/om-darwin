(in-package dwn)

(defun run-tests () 
    (stop-server)
    (sleep 1)
    (start-server)
    (sleep 1)

    (let ((response (test-server)))
        (print response)
        (if (not (equalp response "{\"message\":\"om-darwin 2.0 server (root endpoint)\"}"))
          (error (format nil "Test failed: ~a" response))))

    (let ((response (test-function "() => { const result = [1,1]; while (result.length < 10) { result.push(result[result.length - 2] + result[result.length - 1]) }; return result }")))
        (print response)
        (if (not (equalp response "[1,1,2,3,5,8,13,21,34,55]"))
          (error (format nil "Test failed: ~a" response))))

    (stop-server)
)