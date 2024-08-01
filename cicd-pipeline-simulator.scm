;; File: cicd-pipeline-simulator.scm

(use-modules (ice-9 format)
             (srfi srfi-1)
             (srfi srfi-19)
             (srfi srfi-27))

;; Initialize random number generator
(random-source-randomize! default-random-source)

;; Pipeline stage structure
(define-record-type <pipeline-stage>
  (make-pipeline-stage name duration success-rate)
  pipeline-stage?
  (name stage-name)
  (duration stage-duration)
  (success-rate stage-success-rate))

;; Build structure
(define-record-type <build>
  (make-build id commit-hash start-time stages)
  build?
  (id build-id)
  (commit-hash build-commit-hash)
  (start-time build-start-time)
  (stages build-stages))

;; Pipeline structure
(define-record-type <pipeline>
  (make-pipeline stages builds)
  pipeline?
  (stages pipeline-stages)
  (builds pipeline-builds set-pipeline-builds!))

;; Helper functions
(define (generate-commit-hash)
  (format #f "~8,'0x" (random (expt 16 8))))

(define (run-pipeline-stage stage)
  (let ((duration (+ (stage-duration stage) (random 60)))  ; Add some variability
        (success (< (random 100) (* 100 (stage-success-rate stage)))))
    (cons duration success)))

(define (run-build pipeline)
  (let* ((stages (pipeline-stages pipeline))
         (build-id (+ 1 (length (pipeline-builds pipeline))))
         (commit-hash (generate-commit-hash))
         (start-time (current-time))
         (stage-results (map run-pipeline-stage stages))
         (build (make-build build-id commit-hash start-time stage-results)))
    (set-pipeline-builds! pipeline (cons build (pipeline-builds pipeline)))
    build))

;; Metric calculation functions
(define (calculate-build-success-rate pipeline)
  (let* ((builds (pipeline-builds pipeline))
         (successful-builds (count (lambda (build)
                                     (every (lambda (stage-result) (cdr stage-result))
                                            (build-stages build)))
                                   builds)))
    (/ successful-builds (length builds))))

(define (calculate-average-build-time pipeline)
  (let* ((builds (pipeline-builds pipeline))
         (build-times (map (lambda (build)
                             (apply + (map car (build-stages build))))
                           builds)))
    (/ (apply + build-times) (length build-times))))

;; Main simulation
(define (run-cicd-simulation days)
  (let* ((pipeline-stages (list (make-pipeline-stage "Build" 120 0.95)
                                (make-pipeline-stage "Unit Test" 300 0.90)
                                (make-pipeline-stage "Integration Test" 600 0.85)
                                (make-pipeline-stage "Deploy" 300 0.98)))
         (pipeline (make-pipeline pipeline-stages '())))
    (do ((day 1 (1+ day)))
        ((> day days))
      (format #t "Day ~a:~%" day)
      (let* ((num-builds (+ 1 (random 5)))  ; 1-5 builds per day
             (daily-builds (map (lambda (_) (run-build pipeline)) (iota num-builds))))
        (format #t "  Builds: ~a~%" (length daily-builds))
        (format #t "  Success Rate: ~,2f%~%" (* 100 (calculate-build-success-rate pipeline)))
        (format #t "  Average Build Time: ~,2f minutes~%" (/ (calculate-average-build-time pipeline) 60))))))

;; Main execution
(define (main args)
  (format #t "Starting CI/CD Pipeline Simulation...~%")
  (run-cicd-simulation 30)  ; Simulate 30 days
  (format #t "~%Simulation complete.~%"))

;; Run the simulation if the script is executed directly
(if (equal? (current-module) (resolve-module '(guile-user)))
    (main '())
    (display "CI/CD Pipeline Simulator loaded. Use (main '()) to run the simulation.\n"))
