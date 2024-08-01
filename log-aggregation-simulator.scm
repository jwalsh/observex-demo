;; File: log-aggregation-simulator.scm

(use-modules (ice-9 format)
             (ice-9 match)
             (srfi srfi-1)
             (srfi srfi-19)
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

;; Log entry structure
(define (make-log-entry timestamp service-name level message)
  (vector timestamp service-name level message))

(define (log-entry-timestamp entry) (vector-ref entry 0))
(define (log-entry-service entry) (vector-ref entry 1))
(define (log-entry-level entry) (vector-ref entry 2))
(define (log-entry-message entry) (vector-ref entry 3))

;; Service log generator
(define (make-service-logger service-name)
  (let ((log-levels '(INFO WARNING ERROR)))
    (lambda ()
      (let ((timestamp (date->string (current-date) "~Y-~m-~d ~H:~M:~S"))
            (level (list-ref log-levels (random (length log-levels))))
            (message (case (random 5)
                       ((0) "User login successful")
                       ((1) "Database query completed")
                       ((2) "API request received")
                       ((3) "Cache miss occurred")
                       ((4) "Background job started"))))
        (make-log-entry timestamp service-name level message)))))

;; Central log aggregator
(define (make-log-aggregator)
  (let ((logs '()))
    (lambda (action . args)
      (case action
        ((add-log) (set! logs (cons (car args) logs)))
        ((get-logs) logs)
        ((analyze)
         (let ((analysis-func (car args)))
           (analysis-func logs)))
        (else (error "Unknown action"))))))

;; Log analysis functions
(define (count-logs-by-service logs)
  (let ((count-map '()))
    (for-each
     (lambda (log)
       (let* ((service (log-entry-service log))
              (count (or (assoc-ref count-map service) 0)))
         (set! count-map (assoc-set! count-map service (1+ count)))))
     logs)
    count-map))

(define (count-logs-by-level logs)
  (let ((count-map '()))
    (for-each
     (lambda (log)
       (let* ((level (log-entry-level log))
              (count (or (assoc-ref count-map level) 0)))
         (set! count-map (assoc-set! count-map level (1+ count)))))
     logs)
    count-map))

(define (search-logs-by-keyword logs keyword)
  (filter
   (lambda (log)
     (string-contains (string-downcase (log-entry-message log))
                      (string-downcase keyword)))
   logs))

;; Simulation functions
(define (generate-logs services duration interval)
  (let ((start-time (current-time))
        (end-time (add-duration (current-time) (make-time time-duration 0 duration)))
        (aggregator (make-log-aggregator)))
    (let loop ((current-time start-time))
      (if (time<? current-time end-time)
          (begin
            (for-each
             (lambda (service)
               (let ((log-entry (service)))
                 (aggregator 'add-log log-entry)
                 (format #t "~a [~a] ~a: ~a~%"
                         (color-text (log-entry-timestamp log-entry) 'green)
                         (color-text (log-entry-service log-entry) 'yellow)
                         (color-text (symbol->string (log-entry-level log-entry)) 'magenta)
                         (log-entry-message log-entry))))
             services)
            (sleep interval)
            (loop (add-duration current-time (make-time time-duration 0 interval))))
          aggregator))))

(define (display-analysis-results results)
  (for-each
   (lambda (item)
     (format #t "~a: ~a~%" (color-text (car item) 'cyan) (cdr item)))
   results)
  (newline))

;; Main execution
(define (main args)
  (let* ((services (list (make-service-logger "Frontend")
                         (make-service-logger "Backend")
                         (make-service-logger "Database")
                         (make-service-logger "Cache")))
         (simulation-duration 60)  ; Run for 60 seconds
         (log-interval 1))  ; Generate logs every 1 second

    (format #t "Starting Log Aggregation Simulation...~%~%")
    (let ((aggregator (generate-logs services simulation-duration log-interval)))

      (format #t "~%Log Analysis Results:~%")
      (format #t "Logs by Service:~%")
      (display-analysis-results (aggregator 'analyze count-logs-by-service))

      (format #t "Logs by Level:~%")
      (display-analysis-results (aggregator 'analyze count-logs-by-level))

      (format #t "Logs containing 'error':~%")
      (for-each
       (lambda (log)
         (format #t "~a [~a] ~a: ~a~%"
                 (color-text (log-entry-timestamp log) 'green)
                 (color-text (log-entry-service log) 'yellow)
                 (color-text (symbol->string (log-entry-level log)) 'magenta)
                 (log-entry-message log)))
       (aggregator 'analyze (lambda (logs) (search-logs-by-keyword logs "error"))))

      (format #t "~%Simulation complete.~%"))))

;; Run the simulation if the script is executed directly
(if (equal? (current-module) (resolve-module '(guile-user)))
    (main '())
    (display "Log Aggregation Simulator loaded. Use (main '()) to run the simulation.\n"))
