(use-modules (ice-9 format)
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
(define (feature-flag? obj) (pair? obj))

(define (make-service name)
name)
(define (service-name service) service)
(define (service? obj) (string? obj))

;; Slot tracking
(define (make-slot id name)
(cons id name))
(define (slot-id slot) (car slot))
(define (slot-name slot) (cdr slot))

;; User session tracking
(define (make-user-session id)
(vector id #f '())) ; id, current-page, viewed-slots
(define (user-session-id session) (vector-ref session 0))
(define (user-session-current-page session) (vector-ref session 1))
(define (user-session-viewed-slots session) (vector-ref session 2))
(define (set-user-session-current-page! session page)
(vector-set! session 1 page))
(define (add-user-session-viewed-slot! session slot)
(vector-set! session 2 (cons slot (user-session-viewed-slots session))))

;; Utility Functions
(define (generate-uuid)
(format #f "~4,'0x~4,'0x-~4,'0x-~4,'0x-~4,'0x-~4,'0x~4,'0x~4,'0x"
 (random (expt 16 4)) (random (expt 16 4))
 (random (expt 16 4)) (random (expt 16 4))
 (random (expt 16 4)) (random (expt 16 4))
 (random (expt 16 4)) (random (expt 16 4))))

(define (log-request component requester action ref-id)
(let ((requester-id (if (vector? requester)
                 (user-session-id requester)
                 requester)))
(format #t "~a: ~a by ~a ~a. RefID: ~a~%"
   (color-text component 'cyan)
   action
   (if (vector? requester) "User" "Service")
   (color-text requester-id 'yellow)
   (color-text ref-id 'green))))

(define (format-feature-flags flags)
(string-join 
(map (lambda (flag) 
 (format #f "~a: ~a" 
         (color-text (feature-flag-name flag) 'magenta)
         (color-text (if (feature-flag-enabled flag) "enabled" "disabled") 'blue)))
flags)
", "))

;; User Event Functions
(define (simulate-user-action user-session ref-id action)
(format #t "~%~a~%" (make-string 50 #\=))
(format #t "~a~%" (color-text (format #f "USER ACTION: User ~a - ~a" 
                               (user-session-id user-session)
                               (string-upcase action)) 'red))
(format #t "~a~%" (make-string 50 #\=))
(log-request "Frontend" user-session action ref-id)
(set-user-session-current-page! user-session action)
(simulate-api-gateway user-session ref-id action))

(define (simulate-api-gateway user-session ref-id action)
(log-request "API Gateway" user-session (string-append "Routing " action) ref-id)
(simulate-service-mesh user-session ref-id action))

(define (simulate-service-mesh user-session ref-id action)
(log-request "Service Mesh" user-session (string-append "Directing " action) ref-id)
(case (string->symbol action)
((home-page) (simulate-home-page-visit user-session ref-id))
((product-search) (simulate-product-search user-session ref-id "summer shoes"))
((product-list) (simulate-product-list-view user-session ref-id "electronics"))
((product-detail) (simulate-product-detail-view user-session ref-id "PROD-123"))
((order-view) (simulate-order-view user-session ref-id "ORD-456"))))

(define (simulate-home-page-visit user-session ref-id)
(log-request "HomeService" user-session "Fetching home page content" ref-id))

(define (simulate-product-search user-session ref-id query)
(log-request "SearchService" user-session (format #f "Searching for '~a'" query) ref-id))

(define (simulate-product-list-view user-session ref-id category)
(log-request "ProductService" user-session (format #f "Fetching product list for category '~a'" category) ref-id))

(define (simulate-product-detail-view user-session ref-id product-id)
(log-request "ProductService" user-session (format #f "Fetching product details for product ID ~a" product-id) ref-id))

(define (simulate-order-view user-session ref-id order-id)
(log-request "OrderService" user-session (format #f "Fetching order details for order ID ~a" order-id) ref-id))

;; Simulation Functions
(define (simulate-cdn-edge user-session ref-id)
(log-request "CDN/Edge" user-session "Request received" ref-id)
ref-id)

(define (simulate-frontend-proxy user-session ref-id)
(log-request "Frontend Proxy" user-session "Routing request" ref-id)
ref-id)

(define (simulate-bff user-session ref-id feature-flags)
(log-request "BFF" user-session "Processing request" ref-id)
(values ref-id feature-flags))

(define (simulate-microservice service user-session ref-id feature-flags)
(log-request service user-session "Processing request" ref-id)
(format #t "~a: Active feature flags: ~a~%"
 (color-text service 'cyan)
 (format-feature-flags (filter feature-flag-enabled feature-flags)))

;; Simulate database call
(log-request "Database" service "Querying data" ref-id)

;; Simulate calls to additional services
(simulate-prometheus service ref-id)
(simulate-elasticsearch service ref-id)
(simulate-logstash service ref-id))

(define (simulate-observability user-session ref-id feature-flags)
(log-request "Observability" user-session "Logging event" ref-id)
(format #t "Observability: Feature flags state: ~a~%" (format-feature-flags feature-flags)))

(define (simulate-analytics user-session ref-id feature-flags)
(log-request "Analytics" user-session "Sending event to GA" ref-id)
(format #t "Analytics: Active feature flags: ~a~%"
 (format-feature-flags (filter feature-flag-enabled feature-flags))))

(define (simulate-prometheus service ref-id)
(log-request "Prometheus" service "Collecting metrics" ref-id))

(define (simulate-elasticsearch service ref-id)
(log-request "Elasticsearch" service "Indexing logs" ref-id))

(define (simulate-logstash service ref-id)
(log-request "Logstash" service "Processing logs" ref-id))

(define (simulate-kibana user-session ref-id)
(log-request "Kibana" user-session "Visualizing data" ref-id))

;; Slot viewing simulation
(define (simulate-slot-view user-session ref-id slot)
(format #t "~a~%" (color-text (format #f "SLOT VIEWED: ~a" (slot-name slot)) 'magenta))
(add-user-session-viewed-slot! user-session slot)
(log-request "SlotService" user-session (format #f "Viewing slot ~a" (slot-id slot)) ref-id))

;; User Journey Simulation
(define (simulate-user-journey user-session services feature-flags slots)
(let ((ref-id (generate-uuid)))
(simulate-cdn-edge user-session ref-id)
(simulate-frontend-proxy user-session ref-id)
(simulate-bff user-session ref-id feature-flags)

;; Simulate random user action
(let ((action (list-ref '("home-page" "product-search" "product-list" "product-detail" "order-view") (random 5))))
(simulate-user-action user-session ref-id action)

;; Simulate viewing random slots on the page
(for-each
(lambda (slot)
(when (zero? (random 2)) ; 50% chance to view each slot
  (simulate-slot-view user-session ref-id slot)))
slots))

(for-each
(lambda (service)
(simulate-microservice service user-session ref-id feature-flags))
services)

(simulate-observability user-session ref-id feature-flags)
(simulate-analytics user-session ref-id feature-flags)
(simulate-kibana user-session ref-id)))

;; Interleaved simulation
(define (run-interleaved-simulation num-requests)
(let* ((user-sessions (map (lambda (id) (make-user-session (number->string id)))
                    (iota 5)))
(feature-flags (list (make-feature-flag "new_ui" #t)
                     (make-feature-flag "recommendation_engine" #f)
                     (make-feature-flag "fast_checkout" #t)))
(services (map make-service '("CustomerService" "OrderService" "ProductService")))
(slots (list (make-slot "1" "Top Banner")
             (make-slot "2" "Side Bar")
             (make-slot "3" "Bottom Recommendation"))))
(do ((i 0 (1+ i)))
((= i num-requests))
(let ((user-session (list-ref user-sessions (random (length user-sessions)))))
(format #t "~%~a~%" (color-text (format #f "--- Simulating User Journey ~a (User ~a) ---" 
                                       (1+ i) 
                                       (user-session-id user-session)) 'red))
(simulate-user-journey user-session services feature-flags slots))
(sleep 1))))

;; Main execution
(define (main args)
(random-source-randomize! default-random-source)
(run-interleaved-simulation 20)
(exit)) ; Exit after running the simulation

;; Check if the script is being run directly
(if (equal? (current-module) (resolve-module '(guile-user)))
(main '()) ; Run the simulation if called directly
(display "ObserveX Simulator loaded. Use (main '()) to run the simulation.\n"))