;; -*- mode: wisp; fill-column: 64; -*-
;;
;; Struct utilities for WASM tests.

;; Global storage for struct definitions
(defvar *struct-definitions* nil)

(defun get-struct-def (name)
  (let ((entry (assoc name *struct-definitions*)))
    (if entry (tail entry) nil)))

(defun set-struct-def (name slots)
  (let ((alist (set-struct-def-helper name slots *struct-definitions*)))
    (set! *struct-definitions* alist)))

(defun set-struct-def-helper (name slots alist)
  (if (nil? alist)
      (cons (cons name slots) nil)
    (let ((entry (head alist)))
      (if (eq? (head entry) name)
          (cons (cons name slots) (tail alist))
        (cons entry (set-struct-def-helper name slots (tail alist)))))))

(defun build-constructor-name (struct-name)
  (let ((pkg (symbol-package struct-name)))
    (INTERN (STRING-APPEND "MAKE-" (SYMBOL-NAME struct-name))
            (if pkg pkg (find-package "WISP")))))

(defun build-accessor-name (struct-name slot-name)
  (let ((pkg (symbol-package struct-name)))
    (INTERN (STRING-APPEND (SYMBOL-NAME struct-name)
                          "-"
                          (SYMBOL-NAME slot-name))
            (if pkg pkg (find-package "WISP")))))

(defun build-predicate-name (struct-name)
  (let ((pkg (symbol-package struct-name)))
    (INTERN (STRING-APPEND (SYMBOL-NAME struct-name) "?")
            (if pkg pkg (find-package "WISP")))))

(defun symbol-from-name (name)
  (READ-FROM-STRING name))

(defun plist-get-slot (plist slot-name)
  (cond
    ((nil? plist) nil)
    ((nil? (tail plist)) nil)
    ((let ((key (head plist)))
       (let ((normalized (symbol-from-name (SYMBOL-NAME key)))
             (target (symbol-from-name (SYMBOL-NAME slot-name))))
         (eq? normalized target)))
     (head (tail plist)))
    (t (plist-get-slot (tail (tail plist)) slot-name))))

(defun build-struct-slots (slot-names kv-pairs)
  (if (nil? slot-names)
      nil
    (let* ((slot-name (head slot-names))
           (slot-value (plist-get-slot kv-pairs slot-name)))
      (cons (cons slot-name slot-value)
            (build-struct-slots (tail slot-names) kv-pairs)))))

(defun find-slot-entry (slot-key entries)
  (if (nil? entries)
      nil
    (let ((entry (head entries)))
      (if (and (pair? entry)
               (eq? (head entry) slot-key))
          entry
        (find-slot-entry slot-key (tail entries))))))

(defun create-accessor (struct-name slot-name)
  (let ((accessor-name (build-accessor-name struct-name slot-name))
        (slot-key slot-name))
    (set-symbol-function!
     accessor-name
     (fn (instance)
       (let* ((slots (if (pair? instance) (tail instance) nil))
              (entry (find-slot-entry slot-key slots)))
         (if entry (tail entry) nil))))))

(defun create-all-accessors (struct-name slots)
  (if (nil? slots)
      nil
    (do
      (create-accessor struct-name (head slots))
      (create-all-accessors struct-name (tail slots)))))

(defmacro defstruct (name &rest slots)
  `(do
     (set-struct-def ',name ',slots)
     (let ((constructor-name (build-constructor-name (quote ,name)))
           (predicate-name (build-predicate-name (quote ,name))))
       (set-symbol-function!
        constructor-name
        (fn (&rest kv-pairs)
          (let ((entries (build-struct-slots (quote ,slots) kv-pairs)))
            (cons (quote ,name) entries))))
       (set-symbol-function!
        predicate-name
        (fn (x)
          (if (pair? x)
              (eq? (head x) (quote ,name))
            nil)))
       (create-all-accessors (quote ,name) (quote ,slots)))
     ',name))

