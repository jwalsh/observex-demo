;; File: time-traveling-debugger.scm

(define-module (time-traveling-debugger)
  #:export (debug
            step
            continue
            rewind
            reset))

(use-modules (ice-9 format))

;; Debugger state
(define *debugger-state* (make-hash-table))

;; Current execution point
(define *current-execution-point* 0)

;; Execution history
(define *execution-history* '())

;; Define a function to debug
(define (debug func)
  (set! *execution-history* '())
  (set! *current-execution-point* 0)
  (hash-table-set! *debugger-state* 'func func)
  (hash-table-set! *debugger-state* 'args '())
  (hash-table-set! *debugger-state* 'result #f)
  (func))

;; Step to the next execution point
(define (step)
  (if (< *current-execution-point* (length *execution-history*))
      (begin
        (set! *current-execution-point* (+ *current-execution-point* 1))
        (display "Stepped to execution point ")
        (display *current-execution-point*)
        (newline)
        (display "Current state: ")
        (display (hash-table-get *debugger-state* 'func))
        (newline)
        (display "Arguments: ")
        (display (hash-table-get *debugger-state* 'args))
        (newline)
        (display "Result: ")
        (display (hash-table-get *debugger-state* 'result))
        (newline))
      (display "Already at the end of the execution history")))

;; Continue execution until the next breakpoint
(define (continue)
  (if (< *current-execution-point* (length *execution-history*))
      (begin
        (set! *current-execution-point* (length *execution-history*))
        (display "Continued to the end of the execution history")
        (newline))
      (display "Already at the end of the execution history")))

;; Rewind to the previous execution point
(define (rewind)
  (if (> *current-execution-point* 0)
      (begin
        (set! *current-execution-point* (- *current-execution-point* 1))
        (display "Rewound to execution point ")
        (display *current-execution-point*)
        (newline)
        (display "Current state: ")
        (display (hash-table-get *debugger-state* 'func))
        (newline)
        (display "Arguments: ")
        (display (hash-table-get *debugger-state* 'args))
        (newline)
        (display "Result: ")
        (display (hash-table-get *debugger-state* 'result))
        (newline))
      (display "Already at the beginning of the execution history")))

;; Reset the debugger state
(define (reset)
  (set! *execution-history* '())
  (set! *current-execution-point* 0)
  (hash-table-set! *debugger-state* 'func #f)
  (hash-table-set! *debugger-state* 'args '())
  (hash-table-set! *debugger-state* 'result #f)
  (display "Debugger state reset")
  (newline))

;; Example usage:
(define (example-func x y)
  (hash-table-set! *debugger-state* 'func 'example-func)
  (hash-table-set! *debugger-state* 'args (list x y))
  (hash-table-set! *debugger-state* 'result (+ x y))
  (set! *execution-history* (append *execution-history* (list (hash-table-get *debugger-state* 'func))))
  (+ x y))

(debug example-func 2 3)
(step)
(display (hash-table-get *debugger-state* 'result))
(newline)
(rewind)
(display (hash-table-get *debugger-state* 'args))
(newline)
