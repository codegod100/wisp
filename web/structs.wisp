;; -*- mode: wisp; fill-column: 64; -*-
;;
;; Structs for Wisp
;; A simple implementation of Common Lisp-style structs
;;
;; Usage:
;;   (defstruct foo a b)
;;   (make-foo :a 32 :b 15)  => ((:a . 32) (:b . 15))
;;   (foo-a (make-foo :a 32 :b 15))  => 32
;;

;; Global storage for struct definitions
(defvar *struct-definitions* nil)

;; Cache the base package
(defvar *base-package* nil)

;; Helper: get struct definition
(defun get-struct-def (name)
  (let ((entry (assoc name *struct-definitions*)))
    (if entry
        (tail entry)
      nil)))

;; Helper: set struct definition
(defun set-struct-def (name slots)
  ;; Rebuild the association list with updated entry
  (let ((new-alist (set-struct-def-helper name slots *struct-definitions*)))
    (set! *struct-definitions* new-alist)))

;; Helper: rebuild association list with updated entry
(defun set-struct-def-helper (name slots alist)
  (if (nil? alist)
      (cons (cons name slots) nil)
    (let ((entry (head alist)))
      (if (eq? (head entry) name)
          ;; Replace this entry
          (cons (cons name slots) (tail alist))
        ;; Keep this entry, continue
        (cons entry (set-struct-def-helper name slots (tail alist)))))))

;; Helper: read from string (wrapper for READ-FROM-STRING jet)
;; Note: READ-FROM-STRING takes a string (v08) and returns a value
(defun read-from-string (str)
  (READ-FROM-STRING str))

;; Helper: find value in keyword-value pairs
(defun find-kv-value (key kv-pairs)
  (if (nil? kv-pairs)
      nil
    (let ((pair (head kv-pairs)))
      (if (eq? (head pair) key)
          (tail pair)
        (find-kv-value key (tail kv-pairs))))))

;; Helper: get or cache the base package
(defun get-base-package ()
  (if (nil? *base-package*)
      (let ((pkg (SYMBOL-PACKAGE (read-from-string "test"))))
        (set! *base-package* pkg)
        pkg)
    *base-package*))

;; Helper: intern a symbol (wrapper for INTERN jet)
;; INTERN takes (string package) and creates/finds the symbol
;; Since READ-FROM-STRING works directly, let's try using call with the function
(defun intern-symbol (str &optional pkg)
  ;; Try using call to invoke READ-FROM-STRING
  (call #'READ-FROM-STRING str))

;; Helper: build constructor name at runtime
;; Inline READ-FROM-STRING directly since helper functions don't work
;; Use STRING-APPEND jet directly instead of string-append function
(defun build-constructor-name (struct-name)
  (let ((name-str (SYMBOL-NAME struct-name))
        (make-name-str (STRING-APPEND "make-" name-str)))
    ;; Call READ-FROM-STRING directly - we know this works
    (READ-FROM-STRING make-name-str)))

;; Helper: build accessor name at runtime
(defun build-accessor-name (struct-name slot-name)
  (let ((name-str (STRING-APPEND (SYMBOL-NAME struct-name) "-" (SYMBOL-NAME slot-name))))
    ;; Call READ-FROM-STRING directly
    (READ-FROM-STRING name-str)))

;; Helper: build predicate name at runtime
(defun build-predicate-name (struct-name)
  (let ((name-str (STRING-APPEND (SYMBOL-NAME struct-name) "?")))
    ;; Call READ-FROM-STRING directly
    (READ-FROM-STRING name-str)))

;; Helper: build keyword from slot name at runtime
(defun build-slot-keyword (slot-name)
  (let ((name-str (STRING-APPEND ":" (SYMBOL-NAME slot-name))))
    ;; Keywords are created by READ-FROM-STRING with the : prefix
    ;; This will create them in the KEYWORD package automatically
    (READ-FROM-STRING name-str)))

;; Helper: build struct slots from slot names and values
;; Takes a list of slot names and a list of values, returns association list
(defun build-struct-slots (slot-names values)
  (if (nil? slot-names)
      nil
    (let ((slot-name (head slot-names))
          (slot-value (head values))
          (slot-key (build-slot-keyword slot-name)))
      (cons (cons slot-key slot-value)
            (build-struct-slots (tail slot-names) (tail values))))))

;; Helper: create accessor function for a slot (called at runtime)
(defun create-accessor (struct-name slot-name)
  (let ((accessor-name (build-accessor-name struct-name slot-name))
        (slot-key (build-slot-keyword slot-name)))
    (set-symbol-function!
     accessor-name
     (fn (struct)
       (let ((entry (assoc slot-key struct)))
         (if entry
             (tail entry)
           nil))))))

;; Helper: create all accessors (called at runtime)
(defun create-all-accessors (struct-name slots)
  (if (nil? slots)
      nil
    (do
      (create-accessor struct-name (head slots))
      (create-all-accessors struct-name (tail slots)))))

;; DEFSTRUCT - defines a struct type
;; Uses the struct name's case for constructor and predicate names
(defmacro defstruct (name &rest slots)
  `(do
     ;; Store the struct definition
     (set-struct-def ',name ',slots)
     
     ;; Build constructor and predicate names at runtime using the struct name's case
     (let ((constructor-name (build-constructor-name (quote ,name)))
           (predicate-name (build-predicate-name (quote ,name))))
       
       ;; Create constructor function - uses struct name's case
       ;; Tag struct instances with the struct name as the first element
       (set-symbol-function!
        constructor-name
        (fn (&rest kv-pairs)
          ;; Build association list with struct name tag
          ;; Format: (NAME (:slot1 . val1) (:slot2 . val2) ...)
          (let ((struct-instance
                 (cons (quote ,name)
                       (build-struct-slots (quote ,slots) kv-pairs))))
            struct-instance)))
       
       ;; Create predicate function to check if value is this struct type
       (set-symbol-function!
        predicate-name
        (fn (x)
          ;; Check if it's a list starting with the struct name
          (if (pair? x)
              (eq? (head x) (quote ,name))
            nil)))
       
       ;; Create accessor functions for each slot
       (create-all-accessors (quote ,name) (quote ,slots)))
     
     ;; Return the struct name
     ',name))

;; Example usage:
;; (defstruct foo a b)
;; (make-foo :a 32 :b 15)  => ((:a . 32) (:b . 15))
;; (foo-a (make-foo :a 32 :b 15))  => 32
;; (foo-b (make-foo :a 32 :b 15))  => 15

