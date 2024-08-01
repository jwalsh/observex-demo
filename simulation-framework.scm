;; File: simulation-framework.scm

(define-module (simulation-framework)
  #:export (run-simulation))

(use-modules (srfi srfi-19))

(define (run-simulation duration interval simulation-step)
  (let* ((start-time (current-time))
         (end-time (add-duration start-time (make-time time-duration 0 duration))))
    (let loop ((current-time start-time))
      (if (time<? current-time end-time)
          (begin
            (simulation-step current-time)
            (sleep interval)
            (loop (add-duration current-time (make-time time-duration 0 interval))))
          (display "Simulation complete.\n")))))
