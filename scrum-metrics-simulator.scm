;; File: scrum-metrics-simulator.scm

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

;; Story structure
(define-record-type <story>
  (make-story id points type)
  story?
  (id story-id)
  (points story-points)
  (type story-type))

;; Sprint structure
(define-record-type <sprint>
  (make-sprint number goal stories capacity velocity burndown satisfaction)
  sprint?
  (number sprint-number)
  (goal sprint-goal)
  (stories sprint-stories set-sprint-stories!)
  (capacity sprint-capacity)
  (velocity sprint-velocity set-sprint-velocity!)
  (burndown sprint-burndown set-sprint-burndown!)
  (satisfaction sprint-satisfaction set-sprint-satisfaction!))

;; Team structure
(define-record-type <team>
  (make-team name members velocity)
  team?
  (name team-name)
  (members team-members)
  (velocity team-velocity set-team-velocity!))

;; Helper functions
(define (generate-random-story id)
  (make-story id
              (+ 1 (random 8))  ; Story points between 1 and 8
              (list-ref '(feature bug tech-debt) (random 3))))

(define (generate-sprint-goal)
  (list-ref '("Implement user authentication"
              "Optimize database queries"
              "Refactor legacy code"
              "Add new payment gateway"
              "Improve UI/UX for mobile devices")
            (random 5)))

(define (calculate-sprint-capacity team)
  (* (length (team-members team))
     (+ 6 (random 5))))  ; Each member can do 6-10 points per sprint

(define (simulate-daily-progress sprint day total-days)
  (let* ((total-points (apply + (map story-points (sprint-stories sprint))))
         (ideal-burn (- total-points (* (/ total-points total-days) day)))
         (actual-burn (max 0 (- ideal-burn (- 5 (random 11))))))  ; Random variation
    (set-sprint-burndown! sprint
                          (cons (cons day (cons ideal-burn actual-burn))
                                (sprint-burndown sprint)))))

(define (simulate-sprint team sprint)
  (let ((capacity (sprint-capacity sprint))
        (days 10))  ; Assuming 2-week sprint
    (for-each (lambda (day)
                (simulate-daily-progress sprint day days))
              (iota days))
    (let ((completed-points (apply + (map story-points
                                          (take (sprint-stories sprint)
                                                (random (length (sprint-stories sprint))))))))
      (set-sprint-velocity! sprint completed-points)
      (set-team-velocity! team (cons completed-points (team-velocity team)))
      (set-sprint-satisfaction! sprint (+ 5 (random 6)))  ; Satisfaction 5-10
      sprint)))

(define (run-sprint-planning team backlog sprint-number)
  (let* ((capacity (calculate-sprint-capacity team))
         (sprint-goal (generate-sprint-goal))
         (sprint-stories '())
         (remaining-capacity capacity))
    (let loop ((stories backlog))
      (if (or (null? stories) (<= remaining-capacity 0))
          (make-sprint sprint-number
                       sprint-goal
                       (reverse sprint-stories)
                       capacity
                       0  ; Initial velocity
                       '()  ; Initial burndown
                       0)  ; Initial satisfaction
          (let ((story (car stories)))
            (if (<= (story-points story) remaining-capacity)
                (begin
                  (set! sprint-stories (cons story sprint-stories))
                  (set! remaining-capacity (- remaining-capacity (story-points story)))
                  (loop (cdr stories)))
                (loop (cdr stories))))))))

(define (display-sprint-metrics sprint)
  (format #t "~%Sprint ~a Metrics:~%" (color-text (number->string (sprint-number sprint)) 'yellow))
  (format #t "Goal: ~a~%" (color-text (sprint-goal sprint) 'cyan))
  (format #t "Capacity: ~a points~%" (sprint-capacity sprint))
  (format #t "Velocity: ~a points~%" (sprint-velocity sprint))
  (format #t "Stories Committed: ~a~%" (length (sprint-stories sprint)))
  (format #t "Burndown: ~%")
  (for-each (lambda (day)
              (format #t "  Day ~a: Ideal - ~a, Actual - ~a~%"
                      (car day) (cadr day) (cddr day)))
            (reverse (sprint-burndown sprint)))
  (format #t "Sprint Satisfaction: ~a/10~%" (sprint-satisfaction sprint)))

(define (display-team-metrics team sprints)
  (format #t "~%Team ~a Metrics:~%" (color-text (team-name team) 'magenta))
  (format #t "Team Members: ~a~%" (length (team-members team)))
  (format #t "Average Velocity: ~,2f points per sprint~%"
          (/ (apply + (team-velocity team)) (length (team-velocity team))))
  (format #t "Velocity Trend: ~a~%"
          (string-join (map number->string (reverse (team-velocity team))) " -> ")))

;; Main simulation
(define (run-scrum-simulation num-sprints)
  (let* ((team (make-team "Agile Avengers" '("Alice" "Bob" "Charlie" "David" "Eve") '()))
         (backlog (map generate-random-story (iota 100))))
    (let loop ((sprint-num 1)
               (remaining-backlog backlog)
               (completed-sprints '()))
      (if (> sprint-num num-sprints)
          (begin
            (for-each display-sprint-metrics (reverse completed-sprints))
            (display-team-metrics team completed-sprints))
          (let* ((sprint (run-sprint-planning team remaining-backlog sprint-num))
                 (simulated-sprint (simulate-sprint team sprint)))
            (format #t "~%~a~%" (make-string 50 #\=))
            (format #t "Completed Sprint ~a~%" sprint-num)
            (format #t "~a~%" (make-string 50 #\=))
            (display-sprint-metrics simulated-sprint)
            (loop (+ sprint-num 1)
                  (drop remaining-backlog (length (sprint-stories simulated-sprint)))
                  (cons simulated-sprint completed-sprints)))))))

;; Main execution
(define (main args)
  (format #t "Starting Scrum Metrics Simulation...~%")
  (run-scrum-simulation 5)  ; Simulate 5 sprints
  (format #t "~%Simulation complete.~%"))

;; Run the simulation if the script is executed directly
(if (equal? (current-module) (resolve-module '(guile-user)))
    (main '())
    (display "Scrum Metrics Simulator loaded. Use (main '()) to run the simulation.\n"))
