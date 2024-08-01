;; File: anomaly-detection-simulator.scm

(use-modules (ice-9 format)
             (srfi srfi-1)
             (srfi srfi-19)
             (srfi srfi-27))

;; Initialize random number generator
(random-source-randomize! default-random-source)

;; Utility functions
(define (mean lst)
  (/ (apply + lst) (length lst)))

(define (standard-deviation lst)
  (let ((m (mean lst)))
    (sqrt (/ (apply + (map (lambda (x) (expt (- x m) 2)) lst))
             (- (length lst) 1)))))

(define (z-score value lst)
  (let ((m (mean lst))
        (sd (standard-deviation lst)))
    (if (= sd 0)
        0
        (/ (- value m) sd))))

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

;; Metric Generator
(define (make-metric-generator mean std-dev anomaly-frequency anomaly-scale)
  (lambda ()
    (if (< (random:uniform) anomaly-frequency)
        (+ mean (* anomaly-scale (random:normal 0 std-dev)))
        (+ mean (random:normal 0 std-dev)))))

;; Anomaly Detector
(define (make-anomaly-detector window-size threshold)
  (let ((values '()))
    (lambda (value)
      (set! values (take (cons value values) window-size))
      (if (< (length values) window-size)
          #f
          (> (abs (z-score value values)) threshold)))))

;; Alert System
(define (make-alert-system)
  (let ((alerts '()))
    (lambda (metric-name value timestamp)
      (let ((alert (format #f "ALERT: Anomaly detected in ~a. Value: ~,2f at ~a"
                           metric-name value timestamp)))
        (set! alerts (cons alert alerts))
        (display (color-text alert 'red))
        (newline)))))

;; Simulation
(define (run-simulation duration interval)
  (let* ((cpu-metric (make-metric-generator 50 10 0.05 3))
         (memory-metric (make-metric-generator 70 15 0.05 3))
         (cpu-detector (make-anomaly-detector 20 3))
         (memory-detector (make-anomaly-detector 20 3))
         (alert-system (make-alert-system))
         (start-time (current-time))
         (end-time (add-duration start-time (make-time time-duration 0 duration))))

    (let loop ((current-time start-time))
      (if (time<? current-time end-time)
          (begin
            (let ((cpu-value (cpu-metric))
                  (memory-value (memory-metric))
                  (timestamp (date->string (time-utc->date current-time) "~Y-~m-~d ~H:~M:~S")))

              (when (cpu-detector cpu-value)
                (alert-system "CPU Usage" cpu-value timestamp))

              (when (memory-detector memory-value)
                (alert-system "Memory Usage" memory-value timestamp))

              (format #t "~a: CPU: ~,2f, Memory: ~,2f~%"
                      (color-text timestamp 'green)
                      (color-text (number->string cpu-value) 'yellow)
                      (color-text (number->string memory-value) 'cyan)))

            (sleep interval)
            (loop (add-duration current-time (make-time time-duration 0 interval))))))))

;; Main execution
(define (main args)
  (display "Starting Anomaly Detection Simulation...\n")
  (run-simulation 300 1)  ; Run for 300 seconds, checking every 1 second
  (display "Simulation complete.\n"))

;; Run the simulation if the script is executed directly
(if (equal? (current-module) (resolve-module '(guile-user)))
    (main '())
    (display "Anomaly Detection Simulator loaded. Use (main '()) to run the simulation.\n"))
