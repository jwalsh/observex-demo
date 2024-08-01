;; File: cloud-cost-optimizer-simulator.scm

(use-modules (ice-9 format)
             (srfi srfi-1)
             (srfi srfi-19)
             (srfi srfi-27))

;; Initialize random number generator
(random-source-randomize! default-random-source)

;; Cloud instance structure
(define-record-type <cloud-instance>
  (make-cloud-instance id type provider cost utilization)
  cloud-instance?
  (id instance-id)
  (type instance-type)
  (provider instance-provider)
  (cost instance-cost)
  (utilization instance-utilization set-instance-utilization!))

;; Workload structure
(define-record-type <workload>
  (make-workload id resource-demand variation)
  workload?
  (id workload-id)
  (resource-demand workload-resource-demand)
  (variation workload-variation))

;; Cloud infrastructure structure
(define-record-type <cloud-infrastructure>
  (make-cloud-infrastructure instances workloads auto-scale-threshold)
  cloud-infrastructure?
  (instances infrastructure-instances set-infrastructure-instances!)
  (workloads infrastructure-workloads set-infrastructure-workloads!)
  (auto-scale-threshold infrastructure-auto-scale-threshold))

;; Helper functions
(define (generate-random-instance id)
  (make-cloud-instance
   id
   (list-ref '(small medium large) (random 3))
   (list-ref '(aws gcp azure) (random 3))
   (+ 0.1 (random 1.0))  ; Cost per hour
   0))  ; Initial utilization

(define (generate-random-workload id)
  (make-workload
   id
   (+ 1 (random 10))  ; Base resource demand
   (/ (random 100) 100.0)))  ; Variation as a percentage

(define (calculate-instance-capacity instance)
  (case (instance-type instance)
    ((small) 4)
    ((medium) 8)
    ((large) 16)))

(define (calculate-total-capacity infrastructure)
  (apply + (map calculate-instance-capacity (infrastructure-instances infrastructure))))

(define (calculate-total-demand infrastructure)
  (apply + (map workload-resource-demand (infrastructure-workloads infrastructure))))

(define (adjust-workload-demand workload)
  (let* ((base-demand (workload-resource-demand workload))
         (variation (workload-variation workload))
         (adjustment (* base-demand variation (- (random 2.0) 1))))
    (max 0 (+ base-demand adjustment))))

;; Simulation functions
(define (simulate-auto-scaling infrastructure)
  (let* ((total-capacity (calculate-total-capacity infrastructure))
         (total-demand (calculate-total-demand infrastructure))
         (utilization (/ total-demand total-capacity))
         (threshold (infrastructure-auto-scale-threshold infrastructure)))
    (cond
     ((> utilization threshold)
      (let ((new-instance (generate-random-instance
                           (+ 1 (apply max (map instance-id (infrastructure-instances infrastructure)))))))
        (set-infrastructure-instances!
         infrastructure
         (cons new-instance (infrastructure-instances infrastructure)))
        'scaled-up))
     ((< utilization (/ threshold 2))
      (let ((instances (infrastructure-instances infrastructure)))
        (when (> (length instances) 1)
          (set-infrastructure-instances! infrastructure (cdr instances))
          'scaled-down)))
     (else 'no-change))))

(define (update-instance-utilization infrastructure)
  (let* ((instances (infrastructure-instances infrastructure))
         (total-capacity (apply + (map calculate-instance-capacity instances)))
         (total-demand (apply + (map adjust-workload-demand (infrastructure-workloads infrastructure)))))
    (for-each (lambda (instance)
                (set-instance-utilization!
                 instance
                 (min 1.0 (/ total-demand total-capacity))))
              instances)))

(define (calculate-hourly-cost infrastructure)
  (apply + (map instance-cost (infrastructure-instances infrastructure))))

(define (simulate-reserved-instances infrastructure reserved-percentage)
  (let* ((instances (infrastructure-instances infrastructure))
         (num-reserved (floor (* (length instances) reserved-percentage)))
         (reserved-instances (take instances num-reserved))
         (on-demand-instances (drop instances num-reserved))
         (reserved-cost (* 0.6 (apply + (map instance-cost reserved-instances))))  ; 40% discount
         (on-demand-cost (apply + (map instance-cost on-demand-instances))))
    (+ reserved-cost on-demand-cost)))

(define (simulate-spot-instances infrastructure spot-percentage)
  (let* ((instances (infrastructure-instances infrastructure))
         (num-spot (floor (* (length instances) spot-percentage)))
         (spot-instances (take instances num-spot))
         (on-demand-instances (drop instances num-spot))
         (spot-cost (* 0.3 (apply + (map instance-cost spot-instances))))  ; 70% discount
         (on-demand-cost (apply + (map instance-cost on-demand-instances))))
    (+ spot-cost on-demand-cost)))

(define (simulate-multi-cloud infrastructure)
  (let* ((instances (infrastructure-instances infrastructure))
         (providers '(aws gcp azure))
         (provider-instances (map (lambda (provider)
                                    (filter (lambda (instance)
                                              (eq? (instance-provider instance) provider))
                                            instances))
                                  providers))
         (provider-costs (map (lambda (instances)
                                (apply + (map instance-cost instances)))
                              provider-instances)))
    (apply + provider-costs)))

;; Main simulation
(define (run-cloud-optimization-simulation days)
  (let* ((initial-instances (map generate-random-instance (iota 5)))
         (initial-workloads (map generate-random-workload (iota 3)))
         (infrastructure (make-cloud-infrastructure initial-instances initial-workloads 0.8))
         (total-cost 0)
         (utilization-sum 0)
         (scaling-events 0))
    (do ((hour 1 (1+ hour)))
        ((> hour (* days 24)))
      (update-instance-utilization infrastructure)
      (let ((scaling-result (simulate-auto-scaling infrastructure)))
        (when (eq? scaling-result 'scaled-up)
          (set! scaling-events (1+ scaling-events))))
      (set! total-cost (+ total-cost (calculate-hourly-cost infrastructure)))
      (set! utilization-sum (+ utilization-sum
                               (/ (apply + (map instance-utilization (infrastructure-instances infrastructure)))
                                  (length (infrastructure-instances infrastructure)))))
      (when (zero? (modulo hour 24))
        (let ((day (/ hour 24)))
          (format #t "Day ~a Summary:~%" day)
          (format #t "  Average Hourly Cost: $~,2f~%" (/ total-cost hour))
          (format #t "  Average Utilization: ~,2f%~%" (* 100 (/ utilization-sum hour)))
          (format #t "  Current Instances: ~a~%" (length (infrastructure-instances infrastructure)))
          (format #t "  Scaling Events: ~a~%" scaling-events)
          (format #t "  Reserved Instances Cost: $~,2f~%"
                  (simulate-reserved-instances infrastructure 0.3))
          (format #t "  Spot Instances Cost: $~,2f~%"
                  (simulate-spot-instances infrastructure 0.2))
          (format #t "  Multi-Cloud Cost: $~,2f~%"
                  (simulate-multi-cloud infrastructure))
          (newline))))))

;; Main execution
(define (main args)
  (format #t "Starting Cloud Infrastructure Cost Optimization Simulation...~%")
  (run-cloud-optimization-simulation 30)  ; Simulate 30 days
  (format #t "~%Simulation complete.~%"))

;; Run the simulation if the script is executed directly
(if (equal? (current-module) (resolve-module '(guile-user)))
    (main '())
    (display "Cloud Infrastructure Cost Optimization Simulator loaded. Use (main '()) to run the simulation.\n"))
