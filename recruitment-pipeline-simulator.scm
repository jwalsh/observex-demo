;; File: recruitment-pipeline-simulator.scm

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

;; Candidate structure
(define-record-type <candidate>
  (make-candidate id source skills experience salary-expectation status)
  candidate?
  (id candidate-id)
  (source candidate-source)
  (skills candidate-skills)
  (experience candidate-experience)
  (salary-expectation candidate-salary-expectation)
  (status candidate-status set-candidate-status!))

;; Job opening structure
(define-record-type <job-opening>
  (make-job-opening id title required-skills experience-level salary-range)
  job-opening?
  (id job-opening-id)
  (title job-opening-title)
  (required-skills job-opening-required-skills)
  (experience-level job-opening-experience-level)
  (salary-range job-opening-salary-range))

;; Recruitment pipeline structure
(define-record-type <recruitment-pipeline>
  (make-recruitment-pipeline job-openings candidates activities costs)
  recruitment-pipeline?
  (job-openings pipeline-job-openings set-pipeline-job-openings!)
  (candidates pipeline-candidates set-pipeline-candidates!)
  (activities pipeline-activities set-pipeline-activities!)
  (costs pipeline-costs set-pipeline-costs!))

;; Helper functions
(define (generate-random-candidate id)
  (make-candidate id
                  (list-ref '(job-board employee-referral linkedin direct-application) (random 4))
                  (take (shuffle '(python java javascript c++ sql react node.js aws devops)) (+ 2 (random 4)))
                  (+ 1 (random 10))
                  (* 1000 (+ 40 (random 61)))
                  'applied))

(define (generate-random-job-opening id)
  (make-job-opening id
                    (list-ref '("Software Engineer" "Data Scientist" "Product Manager" "UX Designer") (random 4))
                    (take (shuffle '(python java javascript c++ sql react node.js aws devops)) (+ 3 (random 3)))
                    (list-ref '(entry mid senior) (random 3))
                    (cons (* 1000 (+ 40 (random 31))) (* 1000 (+ 70 (random 31))))))

;; Simulation functions
(define (source-candidates pipeline num-candidates)
  (let* ((current-candidates (pipeline-candidates pipeline))
         (new-candidates (map generate-random-candidate
                              (iota num-candidates (+ 1 (apply max 0 (map candidate-id current-candidates)))))))
    (set-pipeline-candidates! pipeline (append current-candidates new-candidates))
    (set-pipeline-activities! pipeline
                              (cons (cons 'sourcing (length new-candidates))
                                    (pipeline-activities pipeline)))
    (set-pipeline-costs! pipeline
                         (cons (cons 'sourcing (* 100 (length new-candidates)))
                               (pipeline-costs pipeline)))))

(define (screen-candidates pipeline)
  (let* ((candidates (pipeline-candidates pipeline))
         (screened-candidates (map (lambda (c)
                                     (if (and (>= (candidate-experience c) 2)
                                              (>= (length (candidate-skills c)) 3))
                                         (begin (set-candidate-status! c 'screened) c)
                                         c))
                                   candidates))
         (passed-screening (filter (lambda (c) (eq? (candidate-status c) 'screened)) screened-candidates)))
    (set-pipeline-candidates! pipeline screened-candidates)
    (set-pipeline-activities! pipeline
                              (cons (cons 'screening (length passed-screening))
                                    (pipeline-activities pipeline)))
    (set-pipeline-costs! pipeline
                         (cons (cons 'screening (* 50 (length candidates)))
                               (pipeline-costs pipeline)))))

(define (interview-candidates pipeline)
  (let* ((candidates (pipeline-candidates pipeline))
         (to-interview (filter (lambda (c) (eq? (candidate-status c) 'screened)) candidates))
         (interviewed-candidates (map (lambda (c)
                                        (if (zero? (random 2))  ; 50% chance to pass interview
                                            (begin (set-candidate-status! c 'interviewed) c)
                                            c))
                                      to-interview))
         (passed-interview (filter (lambda (c) (eq? (candidate-status c) 'interviewed)) interviewed-candidates)))
    (set-pipeline-candidates! pipeline
                              (append passed-interview
                                      (filter (lambda (c) (not (member c to-interview))) candidates)))
    (set-pipeline-activities! pipeline
                              (cons (cons 'interviews (length passed-interview))
                                    (pipeline-activities pipeline)))
    (set-pipeline-costs! pipeline
                         (cons (cons 'interviews (* 200 (length to-interview)))
                               (pipeline-costs pipeline)))))

(define (make-offers pipeline)
  (let* ((candidates (pipeline-candidates pipeline))
         (to-offer (filter (lambda (c) (eq? (candidate-status c) 'interviewed)) candidates))
         (offered-candidates (map (lambda (c)
                                    (if (zero? (random 2))  ; 50% chance to make an offer
                                        (begin (set-candidate-status! c 'offered) c)
                                        c))
                                  to-offer))
         (received-offers (filter (lambda (c) (eq? (candidate-status c) 'offered)) offered-candidates)))
    (set-pipeline-candidates! pipeline
                              (append received-offers
                                      (filter (lambda (c) (not (member c to-offer))) candidates)))
    (set-pipeline-activities! pipeline
                              (cons (cons 'offers (length received-offers))
                                    (pipeline-activities pipeline)))
    (set-pipeline-costs! pipeline
                         (cons (cons 'offers (* 100 (length to-offer)))
                               (pipeline-costs pipeline)))))

(define (process-hires pipeline)
  (let* ((candidates (pipeline-candidates pipeline))
         (to-hire (filter (lambda (c) (eq? (candidate-status c) 'offered)) candidates))
         (hired-candidates (map (lambda (c)
                                  (if (zero? (random 2))  ; 50% chance to accept offer
                                      (begin (set-candidate-status! c 'hired) c)
                                      (begin (set-candidate-status! c 'declined) c)))
                                to-hire))
         (new-hires (filter (lambda (c) (eq? (candidate-status c) 'hired)) hired-candidates)))
    (set-pipeline-candidates! pipeline
                              (append new-hires
                                      (filter (lambda (c) (not (member c to-hire))) candidates)))
    (set-pipeline-activities! pipeline
                              (cons (cons 'hires (length new-hires))
                                    (pipeline-activities pipeline)))
    (set-pipeline-costs! pipeline
                         (cons (cons 'onboarding (* 1000 (length new-hires)))
                               (pipeline-costs pipeline)))))

;; Metric calculation functions
(define (calculate-source-effectiveness pipeline)
  (let* ((candidates (pipeline-candidates pipeline))
         (hired-candidates (filter (lambda (c) (eq? (candidate-status c) 'hired)) candidates))
         (sources (map candidate-source hired-candidates)))
    (map (lambda (source)
           (cons source (count (lambda (s) (eq? s source)) sources)))
         '(job-board employee-referral linkedin direct-application))))

(define (calculate-time-to-hire pipeline)
  (let* ((activities (pipeline-activities pipeline))
         (sourcing-count (apply + (map cdr (filter (lambda (a) (eq? (car a) 'sourcing)) activities))))
         (hire-count (apply + (map cdr (filter (lambda (a) (eq? (car a) 'hires)) activities)))))
    (if (zero? hire-count)
        0
        (/ sourcing-count hire-count))))

(define (calculate-cost-per-hire pipeline)
  (let* ((costs (pipeline-costs pipeline))
         (total-cost (apply + (map cdr costs)))
         (hire-count (apply + (map cdr (filter (lambda (a) (eq? (car a) 'hires)) (pipeline-activities pipeline))))))
    (if (zero? hire-count)
        0
        (/ total-cost hire-count))))

(define (calculate-offer-acceptance-rate pipeline)
  (let* ((activities (pipeline-activities pipeline))
         (offer-count (apply + (map cdr (filter (lambda (a) (eq? (car a) 'offers)) activities))))
         (hire-count (apply + (map cdr (filter (lambda (a) (eq? (car a) 'hires)) activities)))))
    (if (zero? offer-count)
        0
        (/ hire-count offer-count))))

;; Display functions
(define (display-recruitment-metrics pipeline)
  (format #t "~%Recruitment Pipeline Metrics:~%")
  (format #t "Source Effectiveness:~%")
  (for-each (lambda (source)
              (format #t "  ~a: ~a hires~%" (car source) (cdr source)))
            (calculate-source-effectiveness pipeline))
  (format #t "Time to Hire: ~,2f candidates sourced per hire~%" (calculate-time-to-hire pipeline))
  (format #t "Cost per Hire: $~,2f~%" (calculate-cost-per-hire pipeline))
  (format #t "Offer Acceptance Rate: ~,2f%~%" (* 100 (calculate-offer-acceptance-rate pipeline))))

;; Main simulation
(define (run-recruitment-simulation months)
  (let* ((initial-job-openings (map generate-random-job-opening (iota 5)))
         (pipeline (make-recruitment-pipeline initial-job-openings '() '() '())))
    (do ((month 1 (1+ month)))
        ((> month months))
      (format #t "~%~a~%" (make-string 50 #\=))
      (format #t "Month ~a~%" month)
      (format #t "~a~%" (make-string 50 #\=))

      ;; Simulate recruitment activities
      (source-candidates pipeline (+ 10 (random 21)))  ; Source 10-30 candidates
      (screen-candidates pipeline)
      (interview-candidates pipeline)
      (make-offers pipeline)
      (process-hires pipeline)

      ;; Display metrics
      (display-recruitment-metrics pipeline))))

;; Main execution
(define (main args)
  (format #t "Starting Talent Acquisition and Recruitment Pipeline Simulation...~%")
  (run-recruitment-simulation 12)  ; Simulate 12 months
  (format #t "~%Simulation complete.~%"))

;; Run the simulation if the script is executed directly
(if (equal? (current-module) (resolve-module '(guile-user)))
    (main '())
    (display "Talent Acquisition and Recruitment Pipeline Simulator loaded. Use (main '()) to run the simulation.\n"))
