;; File: diversity-inclusion-metrics-simulator.scm

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

;; Employee structure
(define-record-type <employee>
  (make-employee id gender ethnicity age salary level join-date)
  employee?
  (id employee-id)
  (gender employee-gender)
  (ethnicity employee-ethnicity)
  (age employee-age)
  (salary employee-salary set-employee-salary!)
  (level employee-level set-employee-level!)
  (join-date employee-join-date))

;; Organization structure
(define-record-type <organization>
  (make-organization name employees applicants promotions layoffs)
  organization?
  (name organization-name)
  (employees organization-employees set-organization-employees!)
  (applicants organization-applicants set-organization-applicants!)
  (promotions organization-promotions set-organization-promotions!)
  (layoffs organization-layoffs set-organization-layoffs!))

;; Helper functions
(define (generate-random-employee id)
  (make-employee id
                 (list-ref '(male female non-binary) (random 3))
                 (list-ref '(white black hispanic asian other) (random 5))
                 (+ 22 (random 43))
                 (+ 30000 (random 70000))
                 (list-ref '(entry mid senior executive) (random 4))
                 (current-date)))

(define (generate-random-applicant)
  (make-employee #f
                 (list-ref '(male female non-binary) (random 3))
                 (list-ref '(white black hispanic asian other) (random 5))
                 (+ 22 (random 43))
                 0
                 'applicant
                 #f))

;; Metric calculation functions
(define (calculate-workforce-composition employees)
  (let ((total (length employees)))
    (list
     (cons 'gender (map (lambda (g) (cons g (/ (count (lambda (e) (eq? (employee-gender e) g)) employees) total)))
                        '(male female non-binary)))
     (cons 'ethnicity (map (lambda (e) (cons e (/ (count (lambda (emp) (eq? (employee-ethnicity emp) e)) employees) total)))
                           '(white black hispanic asian other)))
     (cons 'age-groups (map (lambda (ag) (cons ag (/ (count (lambda (e) (and (>= (employee-age e) (car ag))
                                                                             (< (employee-age e) (cdr ag))))
                                                     employees)
                                                total)))
                            '((22 . 30) (30 . 40) (40 . 50) (50 . 65))))
     (cons 'levels (map (lambda (l) (cons l (/ (count (lambda (e) (eq? (employee-level e) l)) employees) total)))
                        '(entry mid senior executive))))))

(define (calculate-recruitment-funnel-diversity applicants hires)
  (let ((total-applicants (length applicants))
        (total-hires (length hires)))
    (list
     (cons 'applicants (calculate-workforce-composition applicants))
     (cons 'hires (calculate-workforce-composition hires)))))

(define (calculate-retention-rates employees start-date end-date)
  (let* ((start-employees (filter (lambda (e) (<= (employee-join-date e) start-date)) employees))
         (end-employees (filter (lambda (e) (<= (employee-join-date e) end-date)) employees))
         (retained (filter (lambda (e) (member e end-employees)) start-employees))
         (retention-rate (/ (length retained) (length start-employees))))
    (cons retention-rate
          (map (lambda (group)
                 (let* ((group-start (filter (lambda (e) (eq? (employee-gender e) group)) start-employees))
                        (group-retained (filter (lambda (e) (eq? (employee-gender e) group)) retained)))
                   (cons group (/ (length group-retained) (length group-start)))))
               '(male female non-binary)))))

(define (calculate-promotion-rates promotions employees)
  (let ((total-employees (length employees)))
    (cons (/ (length promotions) total-employees)
          (map (lambda (group)
                 (let* ((group-employees (filter (lambda (e) (eq? (employee-gender e) group)) employees))
                        (group-promotions (filter (lambda (e) (eq? (employee-gender e) group)) promotions)))
                   (cons group (/ (length group-promotions) (length group-employees)))))
               '(male female non-binary)))))

(define (calculate-pay-equity employees)
  (let* ((levels '(entry mid senior executive))
         (level-data (map (lambda (level)
                            (let ((level-employees (filter (lambda (e) (eq? (employee-level e) level)) employees)))
                              (cons level
                                    (map (lambda (group)
                                           (let ((group-salaries (map employee-salary
                                                                      (filter (lambda (e) (eq? (employee-gender e) group))
                                                                              level-employees))))
                                             (cons group (if (null? group-salaries)
                                                             0
                                                             (/ (apply + group-salaries) (length group-salaries))))))
                                         '(male female non-binary)))))
                          levels)))
    level-data))

;; Simulation functions
(define (simulate-hiring organization num-hires)
  (let* ((new-applicants (map (lambda (_) (generate-random-applicant)) (iota (+ num-hires (random 20)))))
         (hires (take (shuffle new-applicants) num-hires))
         (employees (organization-employees organization))
         (new-employees (map (lambda (hire id)
                               (make-employee id
                                              (employee-gender hire)
                                              (employee-ethnicity hire)
                                              (employee-age hire)
                                              (+ 30000 (random 20000))
                                              'entry
                                              (current-date)))
                             hires
                             (iota num-hires (apply max (map employee-id employees))))))
    (set-organization-employees! organization (append employees new-employees))
    (set-organization-applicants! organization (append (organization-applicants organization) new-applicants))
    hires))

(define (simulate-promotions organization)
  (let* ((employees (organization-employees organization))
         (promotion-candidates (filter (lambda (e) (not (eq? (employee-level e) 'executive))) employees))
         (num-promotions (max 1 (floor (/ (length promotion-candidates) 10))))
         (promoted-employees (take (shuffle promotion-candidates) num-promotions)))
    (for-each (lambda (e)
                (set-employee-level! e (case (employee-level e)
                                         ((entry) 'mid)
                                         ((mid) 'senior)
                                         ((senior) 'executive)))
                (set-employee-salary! e (+ (employee-salary e) 10000)))
              promoted-employees)
    (set-organization-promotions! organization (append (organization-promotions organization) promoted-employees))
    promoted-employees))

(define (simulate-layoffs organization layoff-percentage)
  (let* ((employees (organization-employees organization))
         (num-layoffs (floor (* (length employees) layoff-percentage)))
         (laid-off-employees (take (shuffle employees) num-layoffs)))
    (set-organization-employees! organization (lset-difference equal? employees laid-off-employees))
    (set-organization-layoffs! organization (append (organization-layoffs organization) laid-off-employees))
    laid-off-employees))

;; Display functions
(define (display-workforce-composition composition)
  (for-each (lambda (category)
              (format #t "~a:~%" (color-text (symbol->string (car category)) 'yellow))
              (for-each (lambda (group)
                          (format #t "  ~a: ~,2f%~%" (car group) (* 100 (cdr group))))
                        (cdr category)))
            composition))

(define (display-recruitment-funnel-diversity funnel)
  (format #t "~%Recruitment Funnel Diversity:~%")
  (for-each (lambda (stage)
              (format #t "~a:~%" (color-text (symbol->string (car stage)) 'yellow))
              (display-workforce-composition (cdr stage)))
            funnel))

(define (display-retention-rates retention-data)
  (format #t "~%Retention Rates:~%")
  (format #t "Overall: ~,2f%~%" (* 100 (car retention-data)))
  (for-each (lambda (group)
              (format #t "~a: ~,2f%~%" (car group) (* 100 (cdr group))))
            (cdr retention-data)))

(define (display-promotion-rates promotion-data)
  (format #t "~%Promotion Rates:~%")
  (format #t "Overall: ~,2f%~%" (* 100 (car promotion-data)))
  (for-each (lambda (group)
              (format #t "~a: ~,2f%~%" (car group) (* 100 (cdr group))))
            (cdr promotion-data)))

(define (display-pay-equity pay-data)
  (format #t "~%Pay Equity:~%")
  (for-each (lambda (level-data)
              (format #t "~a:~%" (color-text (symbol->string (car level-data)) 'yellow))
              (for-each (lambda (group-data)
                          (format #t "  ~a: $~,2f~%" (car group-data) (cdr group-data)))
                        (cdr level-data)))
            pay-data))

;; Main simulation
(define (run-diversity-inclusion-simulation months)
  (let* ((initial-employees (map generate-random-employee (iota 100)))
         (organization (make-organization "TechCorp" initial-employees '() '() '())))
    (do ((month 1 (1+ month)))
        ((> month months))
      (format #t "~%~a~%" (make-string 50 #\=))
      (format #t "Month ~a~%" month)
      (format #t "~a~%" (make-string 50 #\=))

      ;; Simulate hiring
      (let ((hires (simulate-hiring organization (+ 2 (random 5)))))
        (format #t "New hires: ~a~%" (length hires))
        (display-recruitment-funnel-diversity (calculate-recruitment-funnel-diversity
                                               (organization-applicants organization)
                                               hires)))

      ;; Simulate promotions
      (let ((promotions (simulate-promotions organization)))
        (format #t "Promotions: ~a~%" (length promotions))
        (display-promotion-rates (calculate-promotion-rates promotions (organization-employees organization))))

      ;; Simulate layoffs (every 6 months)
      (when (zero? (modulo month 6))
        (let ((layoffs (simulate-layoffs organization 0.05)))
          (format #t "Layoffs: ~a~%" (length layoffs))))

      ;; Display current workforce composition
      (format #t "~%Current Workforce Composition:~%")
      (display-workforce-composition (calculate-workforce-composition (organization-employees organization)))

      ;; Display retention rates (calculated every 3 months)
      (when (zero? (modulo month 3))
        (display-retention-rates (calculate-retention-rates
                                  (organization-employees organization)
                                  (add-duration (current-date) (make-time time-duration 0 (* -3 30 86400)))
                                  (current-date))))

      ;; Display pay equity
      (display-pay-equity (calculate-pay-equity (organization-employees organization))))))

;; Main execution
(define (main args)
  (format #t "Starting Diversity and Inclusion Metrics Simulation...~%")
  (run-diversity-inclusion-simulation 12)  ; Simulate 12 months
  (format #t "~%Simulation complete.~%"))

;; Run the simulation if the script is executed directly
(if (equal? (current-module) (resolve-module '(guile-user)))
    (main '())
    (display "Diversity and Inclusion Metrics Simulator loaded. Use (main '()) to run the simulation.\n"))
