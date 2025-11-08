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

;; Helper: find a slot's value within a flat keyword/value list
(defun find-slot-value (slot-name kv-pairs)
  (if (nil? kv-pairs)
      nil
    (let ((key (head kv-pairs))
          (rest (tail kv-pairs)))
      (if (nil? rest)
          nil
        (let ((value (head rest)))
          (if (STRING-EQUAL? (SYMBOL-NAME key)
                             (SYMBOL-NAME slot-name))
              value
            (find-slot-value slot-name (tail rest))))))))

;; Helper: build constructor name at runtime
(defun build-constructor-name (struct-name)
  (READ-FROM-STRING (STRING-APPEND "make-" (SYMBOL-NAME struct-name))))

;; Helper: build accessor name at runtime
(defun build-accessor-name (struct-name slot-name)
  (READ-FROM-STRING (STRING-APPEND (SYMBOL-NAME struct-name) "-" (SYMBOL-NAME slot-name))))

;; Helper: build predicate name at runtime
(defun build-predicate-name (struct-name)
  (READ-FROM-STRING (STRING-APPEND (SYMBOL-NAME struct-name) "?")))

;; Helper: build keyword from slot name at runtime
(defun build-slot-keyword (slot-name)
  (READ-FROM-STRING (STRING-APPEND ":" (SYMBOL-NAME slot-name))))

;; Helper: build struct slots from slot names and values
;; Takes a list of slot names and a list of values, returns association list
(defun build-struct-slots (slot-names kv-pairs)
  (if (nil? slot-names)
      nil
    (let* ((slot-name (head slot-names))
           (slot-key (build-slot-keyword slot-name))
           (slot-value (find-slot-value slot-name kv-pairs)))
      (cons (cons slot-key slot-value)
            (build-struct-slots (tail slot-names) kv-pairs)))))

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
          (let ((slot-entries (build-struct-slots (quote ,slots) kv-pairs)))
            ;; Build association list with struct name tag
            ;; Format: (NAME (:slot1 . val1) (:slot2 . val2) ...)
            (let ((struct-instance (cons (quote ,name) slot-entries)))
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

