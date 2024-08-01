;; File: multi-region-failover-simulator.scm

(add-to-load-path ".")
(use-modules (jaeger)
             (ice-9 format)
             (srfi srfi-1)
             (srfi srfi-19)
             (srfi srfi-27))

;; ANSI color codes
(define (color-text text color)
  (let ((color-code (case color
                      ((red) "31")
                      ((green) "32")
                      ((yellow) "33")
                      ((blue) "34")
                      ((magenta) "35")
                      ((cyan) "36")
                      (else "0"))))  ; default to no color
    (format #f "\x1b[~am~a\x1b[0m" color-code text)))

;; Data Structures
(define (make-feature-flag name enabled)
  (cons name enabled))
(define (feature-flag-name flag) (car flag))
(define (feature-flag-enabled flag) (cdr flag))

(define (make-service name region)
  (cons name region))
(define (service-name service) (car service))
(define (service-region service) (cdr service))

(define (make-region name)
  name)
(define (region-name region) region)

;; User session tracking
(define (make-user-session id)
  (vector id #f))
(define (user-session-id session) (vector-ref session 0))
(define (user-session-current-page session) (vector-ref session 1))
(define (set-user-session-current-page! session page)
  (vector-set! session 1 page))

;; Utility Functions
(define (generate-uuid)
  (format #f "~4,'0x~4,'0x-~4,'0x-~4,'0x-~4,'0x-~4,'0x~4,'0x~4,'0x"
          (random (expt 16 4)) (random (expt 16 4))
          (random (expt 16 4)) (random (expt 16 4))
          (random (expt 16 4)) (random (expt 16 4))
          (random (expt 16 4)) (random (expt 16 4))))

(define (log-request component requester action ref-id region)
  (let ((requester-id (if (vector? requester)
                          (user-session-id requester)
                          requester)))
    (format #t "~a [~a]: ~a by ~a ~a. RefID: ~a~%"
            (color-text component 'cyan)
            (color-text region 'magenta)
            action
            (if (vector? requester) "User" "Service")
            (color-text requester-id 'yellow)
            (color-text ref-id 'green)))
  (send-span-to-jaeger component action 50 ref-id)) ; Send span to Jaeger

;; Global state
(define current-active-region 'us-east-1)

;; Load Balancer
(define (load-balancer user-session ref-id action)
  (log-request "Load Balancer" user-session
               (format #f "Routing ~a to ~a" action current-active-region)
               ref-id
               current-active-region)
  (simulate-service-mesh user-session ref-id action current-active-region))

;; Failover mechanism
(define (admin-trigger-failover)
  (set! current-active-region
        (if (eq? current-active-region 'us-east-1) 'us-east-2 'us-east-1))
  (format #t "~%~a~%" (color-text (format #f "ADMIN ACTION: Failover triggered. Active region is now ~a" current-active-region) 'red)))

;; Service Simulation Functions
(define (simulate-service-mesh user-session ref-id action region)
  (log-request "Service Mesh" user-session (format #f "Directing ~a in ~a" action region) ref-id region)
  (case (string->symbol action)
    ((home-page) (simulate-home-page-visit user-session ref-id region))
    ((product-search) (simulate-product-search user-session ref-id "summer shoes" region))
    ((product-list) (simulate-product-list-view user-session ref-id "electronics" region))
    ((product-detail) (simulate-product-detail-view user-session ref-id "PROD-123" region))
    ((order-view) (simulate-order-view user-session ref-id "ORD-456" region))))

(define (simulate-home-page-visit user-session ref-id region)
  (log-request "HomeService" user-session "Fetching home page content" ref-id region))

(define (simulate-product-search user-session ref-id query region)
  (log-request "SearchService" user-session (format #f "Searching for '~a'" query) ref-id region))

(define (simulate-product-list-view user-session ref-id category region)
  (log-request "ProductService" user-session (format #f "Fetching product list for category '~a'" category) ref-id region))

(define (simulate-product-detail-view user-session ref-id product-id region)
  (log-request "ProductService" user-session (format #f "Fetching product details for product ID ~a" product-id) ref-id region))

(define (simulate-order-view user-session ref-id order-id region)
  (log-request "OrderService" user-session (format #f "Fetching order details for order ID ~a" order-id) ref-id region))

;; User Journey Simulation
(define (simulate-user-journey user-session)
  (let ((ref-id (generate-uuid)))
    (log-request "CDN/Edge" user-session "Request received" ref-id "Global")
    (log-request "Frontend Proxy" user-session "Routing request" ref-id "Global")

    ;; Simulate random user action
    (let ((action (list-ref '("home-page" "product-search" "product-list" "product-detail" "order-view") (random 5))))
      (set-user-session-current-page! user-session action)
      (load-balancer user-session ref-id action))))

;; Main simulation loop
(define (run-multi-region-simulation num-requests)
  (let* ((user-sessions (map (lambda (id) (make-user-session (number->string id)))
                             (iota 5))))
    (do ((i 0 (1+ i)))
        ((= i num-requests))
      (let ((user-session (list-ref user-sessions (random (length user-sessions)))))
        (format #t "~%~a~%" (color-text (format #f "--- Simulating User Journey ~a (User ~a) ---"
                                                (1+ i)
                                                (user-session-id user-session)) 'red))
        (simulate-user-journey user-session))
      (when (= i (quotient num-requests 2))
        (admin-trigger-failover))
      (sleep 1))))

;; Main execution
(define (main args)
  (init-jaeger #:verbose? #t #:debug? #f)
  (random-source-randomize! default-random-source)
  (run-multi-region-simulation 20)
  (exit)) ; Exit after running the simulation

;; Check if the script is being run directly
(if (equal? (current-module) (resolve-module '(guile-user)))
    (main '()) ; Run the simulation if called directly
    (display "Multi-Region Failover Simulator loaded. Use (main '()) to run the simulation.\n"))
