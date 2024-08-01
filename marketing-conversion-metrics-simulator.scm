;; File: marketing-conversion-metrics-simulator.scm

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

;; User interaction structure
(define-record-type <user-interaction>
  (make-user-interaction timestamp type)
  user-interaction?
  (timestamp user-interaction-timestamp)
  (type user-interaction-type))

;; Campaign structure
(define-record-type <campaign>
  (make-campaign name budget start-date end-date interactions)
  campaign?
  (name campaign-name)
  (budget campaign-budget)
  (start-date campaign-start-date)
  (end-date campaign-end-date)
  (interactions campaign-interactions set-campaign-interactions!))

;; Helper functions
(define (generate-random-interaction campaign-start campaign-end)
  (let* ((timestamp (+ campaign-start (random (- campaign-end campaign-start))))
         (type (list-ref '(impression click purchase signup download contact-form) (random 6))))
    (make-user-interaction timestamp type)))

(define (simulate-campaign name budget duration)
  (let* ((start-date (current-time))
         (end-date (add-duration start-date (make-time time-duration 0 (* duration 86400))))
         (num-interactions (+ 1000 (random 9000)))
         (interactions (map (lambda (_)
                              (generate-random-interaction
                               (time-second start-date)
                               (time-second end-date)))
                            (iota num-interactions))))
    (make-campaign name budget start-date end-date interactions)))

;; Metric calculation functions
(define (calculate-total-conversions interactions)
  (length (filter (lambda (i) (memq (user-interaction-type i)
                                    '(purchase signup download contact-form)))
                  interactions)))

(define (calculate-conversion-rate interactions)
  (let ((total-interactions (length interactions))
        (conversions (calculate-total-conversions interactions)))
    (if (zero? total-interactions)
        0
        (/ conversions total-interactions))))

(define (calculate-post-click-conversion-rate interactions)
  (let* ((clicks (filter (lambda (i) (eq? (user-interaction-type i) 'click)) interactions))
         (post-click-conversions (filter
                                  (lambda (i)
                                    (and (> (user-interaction-timestamp i)
                                            (user-interaction-timestamp (car clicks)))
                                         (memq (user-interaction-type i)
                                               '(purchase signup download contact-form))))
                                  interactions)))
    (if (null? clicks)
        0
        (/ (length post-click-conversions) (length clicks)))))

(define (calculate-post-click-impulse-conversions interactions time-window)
  (let* ((clicks (filter (lambda (i) (eq? (user-interaction-type i) 'click)) interactions))
         (impulse-conversions (filter
                               (lambda (i)
                                 (and (memq (user-interaction-type i)
                                            '(purchase signup download contact-form))
                                      (any (lambda (click)
                                             (< (- (user-interaction-timestamp i)
                                                   (user-interaction-timestamp click))
                                                time-window))
                                           clicks)))
                               interactions)))
    (if (null? clicks)
        0
        (/ (length impulse-conversions) (length clicks)))))

(define (calculate-post-click-latent-conversions interactions time-window)
  (let* ((clicks (filter (lambda (i) (eq? (user-interaction-type i) 'click)) interactions))
         (latent-conversions (filter
                              (lambda (i)
                                (and (memq (user-interaction-type i)
                                           '(purchase signup download contact-form))
                                     (any (lambda (click)
                                            (and (> (- (user-interaction-timestamp i)
                                                       (user-interaction-timestamp click))
                                                    time-window)
                                                 (< (- (user-interaction-timestamp i)
                                                       (user-interaction-timestamp click))
                                                    (* time-window 30)))) ; 30-day window
                                          clicks)))
                              interactions)))
    (if (null? clicks)
        0
        (/ (length latent-conversions) (length clicks)))))

(define (calculate-post-impression-conversion-rate interactions time-window)
  (let* ((impressions (filter (lambda (i) (eq? (user-interaction-type i) 'impression)) interactions))
         (post-impression-conversions (filter
                                       (lambda (i)
                                         (and (memq (user-interaction-type i)
                                                    '(purchase signup download contact-form))
                                              (any (lambda (impression)
                                                     (< (- (user-interaction-timestamp i)
                                                           (user-interaction-timestamp impression))
                                                        time-window))
                                                   impressions)))
                                       interactions)))
    (if (null? impressions)
        0
        (/ (length post-impression-conversions) (length impressions)))))

;; Display functions
(define (display-campaign-metrics campaign)
  (let* ((interactions (campaign-interactions campaign))
         (total-conversions (calculate-total-conversions interactions))
         (conversion-rate (calculate-conversion-rate interactions))
         (post-click-rate (calculate-post-click-conversion-rate interactions))
         (impulse-rate (calculate-post-click-impulse-conversions interactions 600)) ; 10 minutes
         (latent-rate (calculate-post-click-latent-conversions interactions 86400)) ; 1 day
         (post-impression-rate (calculate-post-impression-conversion-rate interactions 2592000))) ; 30 days
    (format #t "~%Campaign: ~a~%" (color-text (campaign-name campaign) 'yellow))
    (format #t "Total Conversions: ~a~%" (color-text (number->string total-conversions) 'green))
    (format #t "Conversion Rate: ~,2f%~%" (* conversion-rate 100))
    (format #t "Post-Click Conversion Rate: ~,2f%~%" (* post-click-rate 100))
    (format #t "Post-Click Impulse Conversion Rate: ~,2f%~%" (* impulse-rate 100))
    (format #t "Post-Click Latent Conversion Rate: ~,2f%~%" (* latent-rate 100))
    (format #t "Post-Impression Conversion Rate: ~,2f%~%" (* post-impression-rate 100))))

;; Main simulation
(define (run-marketing-simulation num-campaigns)
  (let* ((campaigns (map (lambda (i)
                           (simulate-campaign
                            (format #f "Campaign ~a" (+ i 1))
                            (+ 5000 (random 15000))  ; Budget between $5000 and $20000
                            (+ 30 (random 61))))     ; Duration between 30 and 90 days
                         (iota num-campaigns))))
    (for-each (lambda (campaign)
                (format #t "~%~a~%" (make-string 50 #\=))
                (display-campaign-metrics campaign)
                (format #t "~a~%" (make-string 50 #\=)))
              campaigns)))

;; Main execution
(define (main args)
  (format #t "Starting Marketing Conversion Metrics Simulation...~%")
  (run-marketing-simulation 5)  ; Simulate 5 campaigns
  (format #t "~%Simulation complete.~%"))

;; Run the simulation if the script is executed directly
(if (equal? (current-module) (resolve-module '(guile-user)))
    (main '())
    (display "Marketing Conversion Metrics Simulator loaded. Use (main '()) to run the simulation.\n"))
