(defun test-symbol-name (sym)
  (SYMBOL-NAME sym))

(defun test-read-from-string (str)
  (READ-FROM-STRING str))

(defun test-build-name (struct-name)
  (let ((name-str (SYMBOL-NAME struct-name)))
    (READ-FROM-STRING (string-append "make-" name-str))))

(defmacro test-defstruct (name)
  `(do
     (print (list 'test-defstruct ',name))
     ',name))
