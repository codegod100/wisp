;; Simple test of format function
(defun format-simple (stream control-string &rest args)
  (let ((result "test"))
    (if (nil? stream)
        result
      (do
        (print result)
        result))))

(format-simple nil "test")

