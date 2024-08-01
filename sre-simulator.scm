;; File: sre-simulator.scm

(use-modules (ice-9 format)
             (srfi srfi-1)
             (srfi srfi-19)
             (srfi srfi-27))

;; Initialize random number generator
(random-source-randomize! default-random-source)

;; Service structure
(define-record-type <service>
  (make-service name slo current-reliability incidents)
  service?
  (name service-name)
  (slo service-slo)
  (current-reliability service-current-reliability set-service-current-reliability!)
  (incidents service-incidents set-service-incidents!))

;; Incident structure
(define-record-type <incident>
  (make-incident severity duration resolution-time)
  incident?
  (severity incident-severity)
  (duration incident-duration)
  (resolution-time incident-resolution-time))

;; On-call engineer structure
(define-record-type <engineer>
  (make-engineer name expertise fatigue)
  engineer?
  (name engineer-name)
  (expertise engineer-expertise)
  (fatigue engineer-fatigue set-engineer-fatigue!))

;; SRE team structure
(define-record-type <sre-team>
  (make-sre-team services engineers error-budget maintenance-strategy)
  sre-team?
  (services sre-team-services set-sre-team-services!)
  (engineers sre-team-engineers)
  (error-budget sre-team-error-budget set-sre-team-error-budget!)
  (maintenance-strategy sre-team-maintenance-strategy set-sre-team-maintenance-strategy!))

;; Helper functions
(define (generate-random-service name)
  (make-service name 0.999 1.0 '()))

(define (generate-random-engineer name)
  (make-engineer name (random 1.0) 0))

(define (simulate-service-performance service)
  (let* ((reliability (service-current-reliability service))
         (variation (* 0.001 (- (random 2.0) 1))))
    (set-service-current-reliability! service (max 0.9 (min 1.0 (+ reliability variation))))))

(define (simulate-incident service)
  (let* ((reliability (service-current-reliability service))
         (incident-chance (- 1 reliability))
         (roll (random 1.0)))
    (when (< roll incident-chance)
      (let* ((severity (cond ((< roll (* incident-chance 0.7)) 'low)
                             ((< roll (* incident-chance 0.9)) 'medium)
                             (else 'high)))
             (duration (case severity
                         ((low) (+ 5 (random 25)))    ; 5-30 minutes
                         ((medium) (+ 30 (random 90))) ; 30-120 minutes
                         ((high) (+ 120 (random 240))))) ; 2-6 hours
             (incident (make-incident severity duration 0)))
        (set-service-incidents! service (cons incident (service-incidents service)))
        incident))))

(define (resolve-incident incident engineer)
  (let* ((base-resolution-time (case (incident-severity incident)
                                 ((low) 15)
                                 ((medium) 45)
                                 ((high) 120)))
         (expertise-factor (- 2 (engineer-expertise engineer)))
         (fatigue-factor (+ 1 (engineer-fatigue engineer)))
         (resolution-time (* base-resolution-time expertise-factor fatigue-factor)))
    (set-engineer-fatigue! engineer (min 1.0 (+ (engineer-fatigue engineer) 0.1)))
    (make-incident (incident-severity incident)
                   (incident-duration incident)
                   resolution-time)))

(define (calculate-slo-adherence service)
  (let* ((total-minutes (* 24 60))  ; Assume daily calculation
         (incidents (service-incidents service))
         (downtime (apply + (map incident-duration incidents)))
         (availability (/ (- total-minutes downtime) total-minutes)))
    (>= availability (service-slo service))))

(define (update-error-budget sre-team)
  (let* ((services (sre-team-services sre-team))
         (adherence-count (count calculate-slo-adherence services))
         (adherence-rate (/ adherence-count (length services)))
         (current-budget (sre-team-error-budget sre-team))
         (budget-change (* (- adherence-rate (service-slo (car services))) 0.1)))  ; Assume all services have same SLO
    (set-sre-team-error-budget! sre-team (max 0.0 (min 1.0 (+ current-budget budget-change))))))

(define (perform-maintenance sre-team)
  (let ((strategy (sre-team-maintenance-strategy sre-team)))
    (for-each
     (lambda (service)
       (case strategy
         ((reactive)
          (when (< (service-current-reliability service) (service-slo service))
            (set-service-current-reliability! service (min 1.0 (+ (service-current-reliability service) 0.001)))))
         ((proactive)
          (set-service-current-reliability! service (min 1.0 (+ (service-current-reliability service) 0.0005))))))
     (sre-team-services sre-team))))

;; Main simulation
(define (run-sre-simulation days)
  (let* ((services (list (generate-random-service "ServiceA")
                         (generate-random-service "ServiceB")
                         (generate-random-service "ServiceC")))
         (engineers (list (generate-random-engineer "Alice")
                          (generate-random-engineer "Bob")
                          (generate-random-engineer "Charlie")))
         (sre-team (make-sre-team services engineers 1.0 'reactive))
         (total-incidents 0)
         (total-resolution-time 0))
    (do ((day 1 (1+ day)))
        ((> day days))
      (for-each simulate-service-performance services)
      (let ((daily-incidents (filter identity (map simulate-incident services))))
        (set! total-incidents (+ total-incidents (length daily-incidents)))
        (for-each
         (lambda (incident)
           (let* ((engineer (list-ref engineers (random (length engineers))))
                  (resolved-incident (resolve-incident incident engineer)))
             (set! total-resolution-time (+ total-resolution-time (incident-resolution-time resolved-incident)))))
         daily-incidents))
      (update-error-budget sre-team)
      (perform-maintenance sre-team)
      (when (zero? (modulo day 7))  ; Weekly summary
        (format #t "Week ~a Summary:~%" (ceiling (/ day 7)))
        (format #t "  SLO Adherence: ~,2f%~%"
                (* 100 (/ (count calculate-slo-adherence services) (length services))))
        (format #t "  Error Budget Remaining: ~,2f%~%" (* 100 (sre-team-error-budget sre-team)))
        (format #t "  Total Incidents: ~a~%" total-incidents)
        (format #t "  Average Resolution Time: ~,2f minutes~%"
                (if (zero? total-incidents) 0 (/ total-resolution-time total-incidents)))
        (format #t "  Current Maintenance Strategy: ~a~%" (sre-team-maintenance-strategy sre-team))
        (newline))
      ;; Switch strategy if error budget is depleted
      (when (zero? (sre-team-error-budget sre-team))
        (set-sre-team-maintenance-strategy! sre-team 'proactive))
      ;; Reset engineer fatigue weekly
      (when (zero? (modulo day 7))
        (for-each (lambda (eng) (set-engineer-fatigue! eng 0)) engineers))))

;; Main execution
(define (main args)
  (format #t "Starting System Reliability Engineering (SRE) Simulation...~%")
  (run-sre-simulation 28)  ; Simulate 4 weeks
  (format #t "~%Simulation complete.~%"))

;; Run the simulation if the script is executed directly
(if (equal? (current-module) (resolve-module '(guile-user)))
    (main '())
    (display "System Reliability Engineering (SRE) Simulator loaded. Use (main '()) to run the simulation.\n"))
