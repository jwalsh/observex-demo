;; tla+-traffic-light-simulator.scm
;; Metadata
;; =========
;; Author: Jason Walsh <j@wal.sh>
;; Date: 2024-08-03
;; Description: A basic traffic light simulator in Guile Scheme, based on the TLA+ specification at https://wal.sh/research/tla+.html
;; Notes:
;; This simulator is a direct implementation of the TLA+ specification for a traffic light system.
;; It uses the Observex simulation framework to model the system's behavior.
;; The simulator starts in the 'red' state and transitions to 'green', then 'yellow', then back to 'red'.

(use-modules (observex-demo simulation-framework)
             (srfi srfi-1)) ; for `every`

(define (tla+-traffic-light-simulator)
  (define initial-state '((light . red)))
  (define transitions
    '((red  . green)
      (green . yellow)
      (yellow . red)))
  (define (next-state state)
    (assoc-ref transitions (assoc-ref state 'light)))
  (define (run-simulation)
    (simulate initial-state
              next-state
              #:step-hook (lambda (state)
                            (format #t "Light: ~a~%" (assoc-ref state 'light)))))
  (run-simulation))

(tla+-traffic-light-simulator)
