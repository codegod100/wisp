;; -*- mode: wisp; fill-column: 64; -*-
;;
;; Generic Functions and Methods for Wisp
;; A first-pass implementation of CLOS-style generic functions
;;
;; Usage:
;;   (defgeneric area (shape))
;;   (defmethod area ((shape string)) "string has no area")
;;   (defmethod area ((shape integer)) (* shape shape))
;;   (area 5)  => 25
;;

;; Global storage for generic function methods
(defvar *generic-methods* nil)

;; Helper: get methods for a generic function
(defun get-methods (name)
  (let ((entry (assoc name *generic-methods*)))
    (if entry
        (tail entry)
      nil)))

;; Helper: set methods for a generic function
(defun set-methods (name methods)
  ;; Rebuild the association list with updated entry
  (let ((new-alist (set-methods-helper name methods *generic-methods*)))
    (set! *generic-methods* new-alist)))

;; Helper: rebuild association list with updated entry
(defun set-methods-helper (name methods alist)
  (if (nil? alist)
      (cons (cons name methods) nil)
    (let ((entry (head alist)))
      (if (eq? (head entry) name)
          ;; Replace this entry
          (cons (cons name methods) (tail alist))
        ;; Keep this entry, continue
        (cons entry (set-methods-helper name methods (tail alist)))))))

;; Helper: normalize type name (handle both uppercase and lowercase)
;; type-of returns uppercase keywords, so normalize everything to uppercase for comparison
(defun normalize-type (type-spec)
  (cond
    ((eq? type-spec 't) 't)
    ((or (eq? type-spec 'string) (eq? type-spec 'STRING)) 'STRING)
    ((or (eq? type-spec 'integer) (eq? type-spec 'INTEGER)) 'INTEGER)
    ((or (eq? type-spec 'cons) (eq? type-spec 'CONS)) 'CONS)
    ((or (eq? type-spec 'symbol) (eq? type-spec 'SYMBOL)) 'SYMBOL)
    ((or (eq? type-spec 'vector) (eq? type-spec 'VECTOR)) 'VECTOR)
    ((or (eq? type-spec 'function) (eq? type-spec 'FUNCTION)) 'FUNCTION)
    (t type-spec)))

;; Helper: get normalized type of a value
(defun normalized-type-of (value)
  (normalize-type (type-of value)))

;; Helper: check if a type matches
(defun type-matches? (value type-spec)
  (cond
    ;; T matches everything
    ((eq? type-spec 't) t)
    ;; Check if type-spec is a symbol
    ((symbol? type-spec)
     ;; First check if it's a struct name (struct names take precedence)
     (let ((struct-def (get-struct-def type-spec)))
       (if struct-def
           ;; It's a struct name - check if value is an instance of this struct
           ;; Struct instances are pairs with the struct name as the head
           (if (pair? value)
               (eq? (head value) type-spec)
             nil)
         ;; Not a struct, check if it's a predicate function
         (let ((pred-fn (symbol-function type-spec)))
           (if pred-fn
               ;; Call the predicate function (e.g., point? for structs)
               (call pred-fn value)
             ;; No function, treat as type name and normalize
             (let ((value-type (normalized-type-of value))
                   (spec-type (normalize-type type-spec)))
               (eq? value-type spec-type)))))))
    ;; Normalize both and check
    (t
     (let ((value-type (normalized-type-of value))
           (spec-type (normalize-type type-spec)))
       ;; Debug: print type matching attempt
       (let ((matches (eq? value-type spec-type)))
         (if (not matches)
             (print (list 'type-mismatch 'value-type value-type 'spec-type spec-type 'original-spec type-spec)))
         matches)))))

;; Helper: check if method signature matches arguments
(defun method-matches? (method-spec args)
  (if (nil? method-spec)
      (nil? args)
    (if (nil? args)
        nil
      (let ((spec-type (head method-spec))
            (arg-value (head args)))
        (let ((matches (type-matches? arg-value spec-type)))
          (if matches
              (method-matches? (tail method-spec) (tail args))
            nil))))))

;; Helper: find the first matching method
(defun find-matching-method (methods args)
  (if (nil? methods)
      nil
    (let ((method (head methods)))
      (let ((spec (head method))
            (body (tail method)))
        ;; Debug: try to log what we're checking
        (let ((matches (method-matches? spec args)))
          (if matches
              body
            (find-matching-method (tail methods) args)))))))

;; Helper: get method signature from parameter list
(defun extract-signature (params)
  (if (nil? params)
      nil
    (let ((param (head params)))
      (if (pair? param)
          ;; (name type) format
          (cons (head (tail param))
                (extract-signature (tail params)))
        ;; Just name, use T (matches anything)
        (cons 't (extract-signature (tail params)))))))

;; Helper: extract parameter names from parameter list
(defun extract-param-names (params)
  (if (nil? params)
      nil
    (let ((param (head params)))
      (if (pair? param)
          ;; (name type) format
          (cons (head param)
                (extract-param-names (tail params)))
        ;; Just name
        (cons param (extract-param-names (tail params)))))))

;; Helper: assoc - find entry in association list
(defun assoc (key alist)
  (if (nil? alist)
      nil
    (let ((entry (head alist)))
      (if (eq? (head entry) key)
          entry
        (assoc key (tail alist))))))

;; Helper: ensure generic function exists (called by defmethod)
(defun ensure-generic-function (name)
  (let ((existing (symbol-function name)))
    (if existing
        ;; Function already exists, check if it's a generic
        existing
      ;; Create the generic function
      (do
        (set-methods name nil)
        (set-symbol-function!
         name
         (fn (&rest args)
           (let ((methods (get-methods name)))
             (if (nil? methods)
                 (do
                   (print (list 'no-methods-defined name))
                   nil)
               (let ((method (find-matching-method methods args)))
                 (if (nil? method)
                     (do
                       ;; Debug: print what we're looking for
                       (print (list 'no-applicable-method name 'args args 'methods methods))
                       ;; Also try to show what types the args are
                       (print (list 'arg-types (map type-of args)))
                       nil)
                   (do
                     ;; Debug: found a method!
                     (print (list 'found-method name 'args args))
                     (apply method args))))))))
        (symbol-function name)))))

;; DEFGENERIC - creates a generic function (optional, defmethod will create it automatically)
(defmacro defgeneric (name params &rest options)
  `(ensure-generic-function ',name))

;; Helper: extract signature from params (called at runtime, not macro expansion)
(defun defmethod-build-signature (params)
  (if (nil? params)
      nil
    (let ((param (head params)))
      (if (pair? param)
          ;; (name type) format - extract the type
          (cons (head (tail param))
                (defmethod-build-signature (tail params)))
        ;; Just name, use T (matches anything)
        (cons 't (defmethod-build-signature (tail params)))))))

;; Helper: extract param names from params (called at runtime)
(defun defmethod-build-param-names (params)
  (if (nil? params)
      nil
    (let ((param (head params)))
      (if (pair? param)
          ;; (name type) format - extract the name
          (cons (head param)
                (defmethod-build-param-names (tail params)))
        ;; Just name
        (cons param (defmethod-build-param-names (tail params)))))))

;; DEFMETHOD - adds a method to a generic function (creates generic if needed)
(defmacro defmethod (name params &rest body)
  ;; Extract param names manually - handle common case of (name type) format
  (let ((param-names
         (if (nil? params)
             nil
           (let ((p (head params)))
             (if (pair? p)
                 ;; (name type) format - extract just the name
                 (cons (head p)
                       (if (nil? (tail params))
                           nil
                         (let ((p2 (head (tail params))))
                           (if (pair? p2)
                               (cons (head p2) nil)
                             (cons p2 nil)))))
               ;; Just name
               (cons p nil))))))
    `(do
       ;; Ensure generic function exists (creates it if needed)
       (ensure-generic-function ',name)
       ;; Build signature at runtime from the quoted params
       (let ((signature (defmethod-build-signature ',params))
             (param-names-list ',param-names)
             (method-fn (fn ,param-names ,@body)))
         ;; Debug: print what we're storing
         (print (list 'storing-method ',name 'signature signature 'param-names param-names-list))
         ;; Get current methods or start with empty list
         (let ((current-methods (get-methods ',name)))
           ;; Add new method to the front (later methods take precedence)
           (set-methods ',name
                      (cons (cons signature method-fn)
                            current-methods))))
       ;; Return the method function to indicate success
       (get-methods ',name))))

;; Example usage:
;; (defmethod area ((shape string)) "string has no area")
;; (defmethod area ((shape integer)) (* shape shape))
;; (area 5)  => 25
;; 
;; Note: defgeneric is optional - defmethod will create the generic function automatically
