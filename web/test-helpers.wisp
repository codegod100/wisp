;; Test helper functions for WASM tests
;; This file provides common utilities needed for testing
;; Load this file before running tests to ensure all helpers are available

;; Helper: get symbol name as string
;; Wrapper for SYMBOL-NAME jet - makes it easy to use in tests
;; This function is defined in base.wisp, but we ensure it's available here
;; for tests that run before base.wisp is fully loaded
(defun symbol-name (sym)
  (SYMBOL-NAME sym))

