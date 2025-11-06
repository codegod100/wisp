;; -*- mode: wisp; fill-column: 64; -*-
;;
;; Common Lisp-style FORMAT function for Wisp
;; Implements basic format directives: ~A, ~S, ~D, ~%, ~~

;; Helper functions first
(defun list-length (list)
  (if (nil? list)
      0
    (+ 1 (list-length (tail list)))))

(defun nth (list n)
  (if (eq? n 0)
      (head list)
    (nth (tail list) (- n 1))))

(defun format-value-aesthetic (val)
  (cond
    ((nil? val) "nil")
    ((eq? val t) "t")
    ((string? val) val)
    ((eq? 'integer (type-of val)) (print-to-string val))
    ((symbol? val) 
     (try (symbol-name val)
       (catch (e k) (print-to-string val))))
    ((pair? val) 
     (try (format-list-aesthetic val)
       (catch (e k) (print-to-string val))))
    (t (print-to-string val))))

(defun format-value-standard (val)
  (cond
    ((nil? val) "nil")
    ((eq? val t) "t")
    ((string? val) (string-append "\"" val "\""))
    ((eq? 'integer (type-of val)) (print-to-string val))
    ((symbol? val)
     (try (symbol-name val)
       (catch (e k) (print-to-string val))))
    ((pair? val)
     (try (format-list-standard val)
       (catch (e k) (print-to-string val))))
    (t (print-to-string val))))

(defun format-list-aesthetic (list)
  (format-list-helper list #'format-value-aesthetic "(" " " ")"))

(defun format-list-standard (list)
  (format-list-helper list #'format-value-standard "(" " " ")"))

(defun format-list-helper (list formatter open sep close)
  (if (nil? list)
      (string-append open close)
    (let ((result (string-append open (call formatter (head list)))))
      (format-list-helper-tail (tail list) formatter result sep close))))

(defun format-list-helper-tail (list formatter result sep close)
  (if (nil? list)
      (string-append result close)
    (if (pair? list)
        (format-list-helper-tail (tail list)
                                 formatter
                                 (string-append result sep (call formatter (head list)))
                                 sep
                                 close)
      (string-append result " . " (call formatter list) close))))

(defun format-directive (directive args arg-index)
  (try
   (cond
     ((string-equal? directive "A")
      ;; ~A - Aesthetic (print value)
      (if (and (not (nil? args)) (< arg-index (list-length args)))
          (let ((val (nth args arg-index)))
            (format-value-aesthetic val))
        ""))
     ((string-equal? directive "S")
      ;; ~S - Standard (print with escaping)
      (if (and (not (nil? args)) (< arg-index (list-length args)))
          (let ((val (nth args arg-index)))
            (format-value-standard val))
        ""))
     ((string-equal? directive "D")
      ;; ~D - Decimal number
      (if (and (not (nil? args)) (< arg-index (list-length args)))
          (let ((val (nth args arg-index)))
            (print-to-string val))
        ""))
     ((string-equal? directive "%")
      ;; ~% - Newline
      "\n")
     ((string-equal? directive "~")
      ;; ~~ - Literal tilde
      "~")
     (t
      ;; Unknown directive, just output it
      (string-append "~" directive)))
   (catch (e k)
     (print-to-string e))))

(defun format-process (control-string args arg-index result)
  (if (eq? (string-length control-string) 0)
      result
    (let ((idx (string-search control-string "~")))
      (if idx
          (let* ((before (string-slice control-string 0 idx))
                 (directive-start (+ idx 1))
                 (directive (if (< directive-start (string-length control-string))
                               (string-nth control-string directive-start)
                             ""))
                 (after-start (+ directive-start 1))
                 (after (if (< after-start (string-length control-string))
                           (string-slice control-string after-start (string-length control-string))
                         ""))
                 (directive-result (if (nil? args)
                                      ""
                                    (format-directive directive args arg-index)))
                 (new-arg-index (if (and (not (nil? args))
                                        (or (string-equal? directive "A")
                                            (string-equal? directive "S")
                                            (string-equal? directive "D")))
                                   (+ arg-index 1)
                                 arg-index)))
            (format-process after
                           args
                           new-arg-index
                           (string-append result
                                         before
                                         directive-result)))
        (string-append result control-string)))))

(defun format-to-string (control-string args)
  (format-process control-string args 0 ""))

;; Main format function - defined last so all helpers are available
(defun format (stream control-string &rest args)
  (let ((result (format-to-string control-string args)))
    (cond
      ((nil? stream)
       ;; If stream is nil, return the formatted string
       result)
      ((eq? stream t)
       ;; If stream is t, print and return the string
       (do
         (print result)
         result))
      (t
       ;; Otherwise, print and return nil (for now)
       (do
         (print result)
         nil)))))
