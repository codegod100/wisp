;; Test helper functions for WASM tests
;; This file provides common utilities needed for testing
;; Load this file before running tests to ensure all helpers are available

;; Note: SYMBOL-NAME jet should be available via Jets.load()
;; This is just a wrapper to make it easier to use in tests
(defun symbol-name (sym)
  (SYMBOL-NAME sym))

