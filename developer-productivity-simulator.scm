;; File: developer-productivity-simulator.scm

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

;; Developer record structure
(define-record-type <developer>
  (make-developer name satisfaction performance activity collaboration efficiency agency motivation learning-culture support)
  developer?
  (name developer-name)
  (satisfaction developer-satisfaction set-developer-satisfaction!)
  (performance developer-performance set-developer-performance!)
  (activity developer-activity set-developer-activity!)
  (collaboration developer-collaboration set-developer-collaboration!)
  (efficiency developer-efficiency set-developer-efficiency!)
  (agency developer-agency set-developer-agency!)
  (motivation developer-motivation set-developer-motivation!)
  (learning-culture developer-learning-culture set-developer-learning-culture!)
  (support developer-support set-developer-support!))

;; Team record structure
(define-record-type <team>
  (make-team name developers deployments incidents)
  team?
  (name team-name)
  (developers team-developers)
  (deployments team-deployments set-team-deployments!)
  (incidents team-incidents set-team-incidents!))

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

;; Helper functions
(define (average lst)
  (if (null? lst)
      0
      (/ (apply + lst) (length lst))))

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
      (average (map deployment-lead-time deployments))))

(define (calculate-time-to-restore-service incidents)
  (if (null? incidents)
      0
      (average (map
                (lambda (i)
                  (- (incident-end-time i) (incident-start-time i)))
                incidents))))

(define (calculate-change-failure-rate deployments)
  (if (null? deployments)
      0
      (/ (length (filter (lambda (d) (not (deployment-success d))) deployments))
         (length deployments))))

;; Simulation functions
(define (simulate-developer-activity developer)
  (set-developer-satisfaction! developer (max 0 (min 100 (+ (developer-satisfaction developer) (- (random 11) 5)))))
  (set-developer-performance! developer (max 0 (min 100 (+ (developer-performance developer) (- (random 11) 5)))))
  (set-developer-activity! developer (max 0 (min 100 (+ (developer-activity developer) (- (random 11) 5)))))
  (set-developer-collaboration! developer (max 0 (min 100 (+ (developer-collaboration developer) (- (random 11) 5)))))
  (set-developer-efficiency! developer (max 0 (min 100 (+ (developer-efficiency developer) (- (random 11) 5)))))
  (set-developer-agency! developer (max 0 (min 100 (+ (developer-agency developer) (- (random 11) 5)))))
  (set-developer-motivation! developer (max 0 (min 100 (+ (developer-motivation developer) (- (random 11) 5)))))
  (set-developer-learning-culture! developer (max 0 (min 100 (+ (developer-learning-culture developer) (- (random 11) 5)))))
  (set-developer-support! developer (max 0 (min 100 (+ (developer-support developer) (- (random 11) 5))))))

(define (simulate-team-activity team)
  (for-each simulate-developer-activity (team-developers team))

  ;; Simulate deployment
  (when (= (random 5) 0) ; 20% chance of deployment
    (let ((lead-time (+ 3600 (random 86400))) ; Lead time between 1 hour and 1 day
          (success (> (random 100) 10))) ; 90% success rate
      (set-team-deployments!
       team
       (cons (make-deployment (time-second (current-time)) lead-time success)
             (team-deployments team)))
      (format #t "~a: Team ~a - Deployment ~a (Lead Time: ~a hours)~%"
              (color-text (date->string (current-date) "~Y-~m-~d ~H:~M:~S") 'green)
              (team-name team)
              (if success
                  (color-text "succeeded" 'blue)
                  (color-text "failed" 'red))
              (/ lead-time 3600))))

  ;; Simulate incident
  (when (= (random 20) 0) ; 5% chance of incident
    (let* ((incident-duration (* 3600 (+ 1 (random 4)))) ; 1-4 hours
           (incident-end (add-duration (current-time) (make-time time-duration 0 incident-duration))))
      (set-team-incidents!
       team
       (cons (make-incident (time-second (current-time)) (time-second incident-end))
             (team-incidents team)))
      (format #t "~a: Team ~a - Incident occurred (Duration: ~a hours)~%"
              (color-text (date->string (current-date) "~Y-~m-~d ~H:~M:~S") 'green)
              (team-name team)
              (/ incident-duration 3600)))))

(define (simulate-development-environment teams duration interval)
  (let ((start-time (current-time))
        (end-time (add-duration (current-time) (make-time time-duration 0 duration))))
    (let loop ((current-time start-time))
      (if (time<? current-time end-time)
          (begin
            (for-each simulate-team-activity teams)
            (sleep interval)
            (loop (add-duration current-time (make-time time-duration 0 interval))))))))

;; Analysis and display functions
(define (calculate-team-averages team)
  (let ((devs (team-developers team)))
    (list
     (cons 'satisfaction (average (map developer-satisfaction devs)))
     (cons 'performance (average (map developer-performance devs)))
     (cons 'activity (average (map developer-activity devs)))
     (cons 'collaboration (average (map developer-collaboration devs)))
     (cons 'efficiency (average (map developer-efficiency devs)))
     (cons 'agency (average (map developer-agency devs)))
     (cons 'motivation (average (map developer-motivation devs)))
     (cons 'learning-culture (average (map developer-learning-culture devs)))
     (cons 'support (average (map developer-support devs))))))

(define (display-team-metrics team)
  (let ((averages (calculate-team-averages team))
        (deployments (team-deployments team))
        (incidents (team-incidents team)))
    (format #t "~%Team: ~a~%" (color-text (team-name team) 'yellow))
    (format #t "DORA Metrics:~%")
    (format #t "  Deployment Frequency: ~,2f per week~%"
            (* (calculate-deployment-frequency deployments (make-time time-duration 0 (* 7 24 3600))) 604800))
    (format #t "  Lead Time for Changes: ~,2f hours~%"
            (/ (calculate-lead-time-for-changes deployments) 3600))
    (format #t "  Time to Restore Service: ~,2f hours~%"
            (/ (calculate-time-to-restore-service incidents) 3600))
    (format #t "  Change Failure Rate: ~,2f%~%"
            (* (calculate-change-failure-rate deployments) 100))
    (format #t "SPACE Framework Averages:~%")
    (for-each
     (lambda (metric)
       (format #t "  ~a: ~,2f~%"
               (color-text (symbol->string (car metric)) 'cyan)
               (cdr metric)))
     averages)))

;; Main execution
(define (main args)
  (let* ((team1-devs (list (make-developer "Alice" 70 80 75 85 70 75 80 70 75)
                           (make-developer "Bob" 75 70 80 75 80 70 75 80 70)
                           (make-developer "Charlie" 80 75 70 80 75 80 70 75 80)))
         (team2-devs (list (make-developer "David" 65 75 70 80 75 70 75 70 75)
                           (make-developer "Eve" 70 80 75 70 80 75 80 75 70)
                           (make-developer "Frank" 75 70 80 75 70 80 75 80 75)))
         (team1 (make-team "Team Alpha" team1-devs '() '()))
         (team2 (make-team "Team Beta" team2-devs '() '()))
         (teams (list team1 team2))
         (simulation-duration 2592000) ; 30 days
         (simulation-interval 3600))   ; 1 hour

    (format #t "Starting Developer Productivity Simulation...~%~%")
    (simulate-development-environment teams simulation-duration simulation-interval)

    (for-each display-team-metrics teams)

    (format #t "~%Simulation complete.~%")))

;; Run the simulation if the script is executed directly
(if (equal? (current-module) (resolve-module '(guile-user)))
    (main '())
    (display "Developer Productivity Simulator loaded. Use (main '()) to run the simulation.\n"))
