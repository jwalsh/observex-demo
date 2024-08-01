;; File: financial-metrics-simulator.scm

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

;; Company structure
(define-record-type <company>
  (make-company name revenue expenses assets liabilities employees customers)
  company?
  (name company-name)
  (revenue company-revenue set-company-revenue!)
  (expenses company-expenses set-company-expenses!)
  (assets company-assets set-company-assets!)
  (liabilities company-liabilities set-company-liabilities!)
  (employees company-employees set-company-employees!)
  (customers company-customers set-company-customers!))

;; Helper functions
(define (generate-random-growth base-value min-growth max-growth)
  (* base-value (+ 1 (/ (+ min-growth (random (- max-growth min-growth))) 100))))

;; Metric calculation functions
(define (calculate-current-ratio company)
  (/ (company-assets company) (company-liabilities company)))

(define (calculate-gross-margin company)
  (/ (- (company-revenue company) (company-expenses company))
     (company-revenue company)))

(define (calculate-net-burn company)
  (- (company-revenue company) (company-expenses company)))

(define (calculate-net-profit company)
  (- (company-revenue company) (company-expenses company)))

(define (calculate-ebitda company)
  ;; Simplified EBITDA calculation
  (calculate-net-profit company))

(define (calculate-arr company)
  ;; Assuming all revenue is recurring for simplicity
  (company-revenue company))

(define (calculate-cac-payback-period company)
  ;; Simplified CAC payback period calculation
  (/ (company-expenses company) (length (company-customers company))))

(define (calculate-customer-lifetime-value company)
  ;; Simplified CLV calculation
  (/ (company-revenue company) (length (company-customers company))))

(define (calculate-revenue-per-employee company)
  (/ (company-revenue company) (length (company-employees company))))

(define (calculate-mrr-growth-rate prev-revenue current-revenue)
  (if (zero? prev-revenue)
      0
      (* 100 (/ (- current-revenue prev-revenue) prev-revenue))))

;; Simulation functions
(define (simulate-month company prev-revenue)
  (let ((new-revenue (generate-random-growth (company-revenue company) -5 10))
        (new-expenses (generate-random-growth (company-expenses company) -2 8))
        (new-assets (generate-random-growth (company-assets company) -3 7))
        (new-liabilities (generate-random-growth (company-liabilities company) -1 5))
        (new-employees (max 1 (+ (length (company-employees company)) (- (random 3) 1))))
        (new-customers (max 1 (+ (length (company-customers company)) (- (random 5) 2)))))
    (set-company-revenue! company new-revenue)
    (set-company-expenses! company new-expenses)
    (set-company-assets! company new-assets)
    (set-company-liabilities! company new-liabilities)
    (set-company-employees! company (make-list new-employees 'employee))
    (set-company-customers! company (make-list new-customers 'customer))
    (calculate-mrr-growth-rate prev-revenue new-revenue)))

;; Display functions
(define (display-company-metrics company mrr-growth)
  (format #t "~%Company: ~a~%" (color-text (company-name company) 'yellow))
  (format #t "Current Ratio: ~,2f~%" (calculate-current-ratio company))
  (format #t "Gross Margin: ~,2f%~%" (* 100 (calculate-gross-margin company)))
  (format #t "Net Burn: $~,2f~%" (calculate-net-burn company))
  (format #t "Net Profit: $~,2f~%" (calculate-net-profit company))
  (format #t "Revenue: $~,2f~%" (company-revenue company))
  (format #t "EBITDA: $~,2f~%" (calculate-ebitda company))
  (format #t "Annual Recurring Revenue: $~,2f~%" (calculate-arr company))
  (format #t "CAC Payback Period: ~,2f months~%" (calculate-cac-payback-period company))
  (format #t "Customer Lifetime Value: $~,2f~%" (calculate-customer-lifetime-value company))
  (format #t "Revenue Per Employee: $~,2f~%" (calculate-revenue-per-employee company))
  (format #t "MRR Growth Rate: ~,2f%~%" mrr-growth))

;; Main simulation
(define (run-financial-simulation months)
  (let* ((initial-company (make-company "TechCorp"
                                        1000000  ; Initial revenue
                                        800000   ; Initial expenses
                                        1500000  ; Initial assets
                                        1000000  ; Initial liabilities
                                        (make-list 50 'employee)
                                        (make-list 100 'customer)))
         (company initial-company))
    (do ((month 1 (1+ month))
         (prev-revenue (company-revenue company) (company-revenue company)))
        ((> month months))
      (let ((mrr-growth (simulate-month company prev-revenue)))
        (format #t "~%~a~%" (make-string 50 #\=))
        (format #t "Month ~a~%" month)
        (format #t "~a~%" (make-string 50 #\=))
        (display-company-metrics company mrr-growth)))))

;; Main execution
(define (main args)
  (format #t "Starting Financial Metrics Simulation...~%")
  (run-financial-simulation 12)  ; Simulate 12 months
  (format #t "~%Simulation complete.~%"))

;; Run the simulation if the script is executed directly
(if (equal? (current-module) (resolve-module '(guile-user)))
    (main '())
    (display "Financial Metrics Simulator loaded. Use (main '()) to run the simulation.\n"))
