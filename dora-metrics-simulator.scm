;; File: dora-metrics-simulator.scm

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

;; Deployment record structure
(define-record-type <deployment>
  (make-deployment timestamp lead-time success)
  deployment?
  (timestamp deployment-timestamp)
  (lead-time deployment-lead-time)
  (success deployment-success))

;; Incident record structure
(define-record-type <incident>
  (make-incident start-time end-time)
  incident?
  (start-time incident-start-time)
  (end-time incident-end-time))

;; Team performance tracker
(define (make-team-tracker)
  (let ((deployments '())
        (incidents '()))
    (lambda (action . args)
      (case action
        ((add-deployment) (set! deployments (cons (car args) deployments)))
        ((add-incident) (set! incidents (cons (car args) incidents)))
        ((get-deployments) deployments)
        ((get-incidents) incidents)
        (else (error "Unknown action"))))))

;; DORA metric calculations
(define (calculate-deployment-frequency deployments period)
  (let* ((period-start (- (current-time) period))
         (period-deployments (filter
                              (lambda (d)
                                (> (deployment-timestamp d) period-start))
                              deployments)))
    (/ (length period-deployments) (time-second period))))

(define (calculate-lead-time-for-changes deployments)
  (if (null? deployments)
      0
      (let ((lead-times (map deployment-lead-time deployments)))
        (/ (apply + lead-times) (length lead-times)))))

(define (calculate-time-to-restore-service incidents)
  (if (null? incidents)
      0
      (let ((restore-times (map
                            (lambda (i)
                              (- (incident-end-time i) (incident-start-time i)))
                            incidents)))
        (/ (apply + restore-times) (length restore-times)))))

(define (calculate-change-failure-rate deployments)
  (if (null? deployments)
      0
      (let ((failed-deployments (filter
                                 (lambda (d) (not (deployment-success d)))
                                 deployments)))
        (/ (length failed-deployments) (length deployments)))))

;; Simulation functions
(define (simulate-development-activity team-tracker duration interval)
  (let ((start-time (current-time))
        (end-time (add-duration (current-time) (make-time time-duration 0 duration))))
    (let loop ((current-time start-time))
      (if (time<? current-time end-time)
          (begin
            ;; Simulate deployment
            (when (= (random 5) 0) ; 20% chance of deployment
              (let ((lead-time (+ 3600 (random 86400))) ; Lead time between 1 hour and 1 day
                    (success (> (random 100) 10))) ; 90% success rate
                (team-tracker 'add-deployment
                              (make-deployment
                               (time-second current-time)
                               lead-time
                               success))
                (format #t "~a: Deployment ~a (Lead Time: ~a hours)~%"
                        (color-text (date->string (time-utc->date current-time) "~Y-~m-~d ~H:~M:~S") 'green)
                        (if success
                            (color-text "succeeded" 'blue)
                            (color-text "failed" 'red))
                        (/ lead-time 3600))))

            ;; Simulate incident
            (when (= (random 20) 0) ; 5% chance of incident
              (let* ((incident-duration (* 3600 (+ 1 (random 4)))) ; 1-4 hours
                     (incident-end (add-duration current-time (make-time time-duration 0 incident-duration))))
                (team-tracker 'add-incident
                              (make-incident
                               (time-second current-time)
                               (time-second incident-end)))
                (format #t "~a: Incident occurred (Duration: ~a hours)~%"
                        (color-text (date->string (time-utc->date current-time) "~Y-~m-~d ~H:~M:~S") 'green)
                        (/ incident-duration 3600))))

            (sleep interval)
            (loop (add-duration current-time (make-time time-duration 0 interval))))))))

(define (display-dora-metrics team-tracker)
  (let* ((deployments (team-tracker 'get-deployments))
         (incidents (team-tracker 'get-incidents))
         (deployment-frequency (calculate-deployment-frequency deployments (make-time time-duration 0 (* 7 24 3600))))
         (lead-time (calculate-lead-time-for-changes deployments))
         (time-to-restore (calculate-time-to-restore-service incidents))
         (change-failure-rate (calculate-change-failure-rate deployments)))
    (format #t "~%DORA Metrics:~%")
    (format #t "Deployment Frequency: ~,2f per week~%"
            (color-text (number->string (* deployment-frequency 604800)) 'yellow))
    (format #t "Lead Time for Changes: ~,2f hours~%"
            (color-text (number->string (/ lead-time 3600)) 'yellow))
    (format #t "Time to Restore Service: ~,2f hours~%"
            (color-text (number->string (/ time-to-restore 3600)) 'yellow))
    (format #t "Change Failure Rate: ~,2f%~%"
            (color-text (number->string (* change-failure-rate 100)) 'yellow))))

;; Main execution
(define (main args)
  (let ((team-tracker (make-team-tracker))
        (simulation-duration 2592000) ; 30 days
        (simulation-interval 3600))   ; 1 hour

    (format #t "Starting DORA Metrics Simulation...~%~%")
    (simulate-development-activity team-tracker simulation-duration simulation-interval)

    (display-dora-metrics team-tracker)

    (format #t "~%Simulation complete.~%")))

;; Run the simulation if the script is executed directly
(if (equal? (current-module) (resolve-module '(guile-user)))
    (main '())
    (display "DORA Metrics Simulator loaded. Use (main '()) to run the simulation.\n"))
