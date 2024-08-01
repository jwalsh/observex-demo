;; File: chaos-engineering-simulator.scm

(use-modules (ice-9 format)
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

;; Service model
(define (make-service name dependencies)
  (let ((state 'healthy))
    (lambda (action . args)
      (case action
        ((get-name) name)
        ((get-state) state)
        ((set-state!) (set! state (car args)))
        ((get-dependencies) dependencies)
        (else (error "Unknown action"))))))

;; System model
(define (make-system services)
  (lambda (action . args)
    (case action
      ((get-services) services)
      ((get-service)
       (find (lambda (s) (equal? (s 'get-name) (car args))) services))
      (else (error "Unknown action")))))

;; Failure injection
(define (inject-failure! system service-name failure-type duration)
  (let ((service (system 'get-service service-name)))
    (when service
      (service 'set-state! failure-type)
      (format #t "~a: Injected ~a failure into ~a for ~a seconds~%"
              (color-text (date->string (current-date) "~Y-~m-~d ~H:~M:~S") 'green)
              (color-text (symbol->string failure-type) 'red)
              (color-text service-name 'yellow)
              duration)
      (sleep duration)
      (service 'set-state! 'healthy)
      (format #t "~a: Restored ~a to healthy state~%"
              (color-text (date->string (current-date) "~Y-~m-~d ~H:~M:~S") 'green)
              (color-text service-name 'yellow)))))

;; Request simulation
(define (simulate-request system entry-point)
  (let loop ((current-service (system 'get-service entry-point))
             (path (list entry-point)))
    (if (not current-service)
        (format #t "~a: Request failed at ~a~%"
                (color-text (date->string (current-date) "~Y-~m-~d ~H:~M:~S") 'green)
                (color-text (car path) 'red))
        (let ((state (current-service 'get-state)))
          (if (eq? state 'healthy)
              (let ((dependencies (current-service 'get-dependencies)))
                (if (null? dependencies)
                    (format #t "~a: Request completed successfully: ~a~%"
                            (color-text (date->string (current-date) "~Y-~m-~d ~H:~M:~S") 'green)
                            (color-text (string-join (map symbol->string (reverse path)) " -> ") 'cyan))
                    (loop (system 'get-service (car dependencies))
                          (cons (car dependencies) path))))
              (format #t "~a: Request failed at ~a due to ~a~%"
                      (color-text (date->string (current-date) "~Y-~m-~d ~H:~M:~S") 'green)
                      (color-text (current-service 'get-name) 'red)
                      (color-text (symbol->string state) 'magenta)))))))

;; Chaos experiment
(define (run-chaos-experiment system duration interval)
  (let ((start-time (current-time))
        (end-time (add-duration (current-time) (make-time time-duration 0 duration))))
    (let loop ((current-time start-time))
      (if (time<? current-time end-time)
          (begin
            (simulate-request system 'frontend)
            (when (= (random 5) 0)  ; 20% chance of injecting failure
              (let* ((services (system 'get-services))
                     (target-service ((list-ref services (random (length services))) 'get-name))
                     (failure-type (list-ref '(network-latency resource-exhaustion crash) (random 3)))
                     (failure-duration (+ 5 (random 10))))
                (inject-failure! system target-service failure-type failure-duration)))
            (sleep interval)
            (loop (add-duration current-time (make-time time-duration 0 interval))))))))

;; Main execution
(define (main args)
  (let* ((frontend (make-service 'frontend '(api-gateway)))
         (api-gateway (make-service 'api-gateway '(auth-service product-service)))
         (auth-service (make-service 'auth-service '(database)))
         (product-service (make-service 'product-service '(database)))
         (database (make-service 'database '()))
         (system (make-system (list frontend api-gateway auth-service product-service database))))

    (format #t "Starting Chaos Engineering Simulation...~%")
    (run-chaos-experiment system 300 2)  ; Run for 300 seconds, checking every 2 seconds
    (format #t "Simulation complete.~%")))

;; Run the simulation if the script is executed directly
(if (equal? (current-module) (resolve-module '(guile-user)))
    (main '())
    (display "Chaos Engineering Simulator loaded. Use (main '()) to run the simulation.\n"))
