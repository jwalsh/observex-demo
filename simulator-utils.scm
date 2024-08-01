;; File: simulator-utils.scm

(define-module (simulator-utils)
  #:export (color-text random-uniform random-normal))

(use-modules (ice-9 format)
             (srfi srfi-27))

;; Initialize random number generator
(random-source-randomize! default-random-source)

;; ANSI color codes for output
(define (color-text text color)
  (let ((color-code (case color
                      ((red) "31")
                      ((green) "32")
                      ((yellow) "33")
                      ((blue) "34")
                      ((magenta) "35")
                      ((cyan) "36")
                      (else "0"))))
    (format #f "\x1b[~am~a\x1b[0m" color-code text)))

;; Random number generators
(define (random-uniform)
  (random:uniform))

(define (random-normal mean std-dev)
  (+ mean (* std-dev (random:normal))))
