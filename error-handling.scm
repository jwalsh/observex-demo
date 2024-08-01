;; File: error-handling.scm

(define-module (error-handling)
  #:export (with-error-handling))

(use-modules (ice-9 format))

(define (with-error-handling thunk)
  (catch #t
         thunk
         (lambda (key . args)
           (format (current-error-port) "Error: ~a~%" (apply format #f args))
           (exit 1))))
