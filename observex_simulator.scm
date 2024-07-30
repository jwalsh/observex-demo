(use-modules (ice-9 format)
    (srfi srfi-1)
    (srfi srfi-19)
    (srfi srfi-27))

;; ANSI color codes (unchanged)
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

;; Data Structures (unchanged)
(define (make-user id session-id)
(cons id session-id))
(define (user-id user) (car user))
(define (user-session-id user) (cdr user))
(define (user? obj) (pair? obj))

(define (make-feature-flag name enabled)
(cons name enabled))
(define (feature-flag-name flag) (car flag))
(define (feature-flag-enabled flag) (cdr flag))
(define (feature-flag? obj) (pair? obj))

(define (make-service name)
name)
(define (service-name service) service)
(define (service? obj) (string? obj))

;; Utility Functions (mostly unchanged)
(define (generate-uuid)
(format #f "~4,'0x~4,'0x-~4,'0x-~4,'0x-~4,'0x-~4,'0x~4,'0x~4,'0x"
 (random (expt 16 4)) (random (expt 16 4))
 (random (expt 16 4)) (random (expt 16 4))
 (random (expt 16 4)) (random (expt 16 4))
 (random (expt 16 4)) (random (expt 16 4))))

(define (log-request component requester action ref-id)
(let* ((requester-type (if (user? requester) "User" "Service"))
(requester-id (if (user? requester)
                  (user-id requester)
                  (service-name requester))))
(format #t "~a: ~a by ~a ~a. RefID: ~a~%"
   (color-text component 'cyan)
   action
   requester-type
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

;; User Event Functions (updated)
(define (simulate-user-action user ref-id action)
(log-request "Frontend" user action ref-id)
(simulate-api-gateway user ref-id action))

(define (simulate-api-gateway user ref-id action)
(log-request "API Gateway" user (string-append "Routing " action) ref-id)
(simulate-service-mesh user ref-id action))

(define (simulate-service-mesh user ref-id action)
(log-request "Service Mesh" user (string-append "Directing " action) ref-id)
(case (string->symbol action)
((home-page) (simulate-home-page-visit user ref-id))
((product-search) (simulate-product-search user ref-id "summer shoes"))
((product-list) (simulate-product-list-view user ref-id "electronics"))
((product-detail) (simulate-product-detail-view user ref-id "PROD-123"))
((order-view) (simulate-order-view user ref-id "ORD-456"))))

(define (simulate-home-page-visit user ref-id)
(log-request "HomeService" user "Fetching home page content" ref-id))

(define (simulate-product-search user ref-id query)
(log-request "SearchService" user (format #f "Searching for '~a'" query) ref-id))

(define (simulate-product-list-view user ref-id category)
(log-request "ProductService" user (format #f "Fetching product list for category '~a'" category) ref-id))

(define (simulate-product-detail-view user ref-id product-id)
(log-request "ProductService" user (format #f "Fetching product details for product ID ~a" product-id) ref-id))

(define (simulate-order-view user ref-id order-id)
(log-request "OrderService" user (format #f "Fetching order details for order ID ~a" order-id) ref-id))

;; Simulation Functions (mostly unchanged)
(define (simulate-cdn-edge user ref-id)
(log-request "CDN/Edge" user "Request received" ref-id)
ref-id)

(define (simulate-frontend-proxy user ref-id)
(log-request "Frontend Proxy" user "Routing request" ref-id)
ref-id)

(define (simulate-bff user ref-id feature-flags)
(log-request "BFF" user "Processing request" ref-id)
(values ref-id feature-flags))

(define (simulate-microservice service user ref-id feature-flags)
(log-request (service-name service) user "Processing request" ref-id)
(format #t "~a: Active feature flags: ~a~%"
 (color-text (service-name service) 'cyan)
 (format-feature-flags (filter feature-flag-enabled feature-flags)))

;; Simulate database call
(log-request "Database" service "Querying data" ref-id)

;; Simulate calls to additional services
(simulate-prometheus service ref-id)
(simulate-elasticsearch service ref-id)
(simulate-logstash service ref-id))

(define (simulate-observability user ref-id feature-flags)
(log-request "Observability" user "Logging event" ref-id)
(format #t "Observability: Feature flags state: ~a~%" (format-feature-flags feature-flags)))

(define (simulate-analytics user ref-id feature-flags)
(log-request "Analytics" user "Sending event to GA" ref-id)
(format #t "Analytics: Active feature flags: ~a~%"
 (format-feature-flags (filter feature-flag-enabled feature-flags))))

(define (simulate-prometheus service ref-id)
(log-request "Prometheus" service "Collecting metrics" ref-id))

(define (simulate-elasticsearch service ref-id)
(log-request "Elasticsearch" service "Indexing logs" ref-id))

(define (simulate-logstash service ref-id)
(log-request "Logstash" service "Processing logs" ref-id))

(define (simulate-kibana user ref-id)
(log-request "Kibana" user "Visualizing data" ref-id))

;; Updated User Journey Simulation
(define (simulate-user-journey users services feature-flags)
(let* ((user (list-ref users (random (length users))))
(ref-id (generate-uuid)))
(set! ref-id (simulate-cdn-edge user ref-id))
(set! ref-id (simulate-frontend-proxy user ref-id))
(call-with-values 
(lambda () (simulate-bff user ref-id feature-flags))
(lambda (new-ref-id new-flags)
(set! ref-id new-ref-id)
(set! feature-flags new-flags)))

;; Simulate random user events with corrected flow
(simulate-user-action user ref-id "home-page")
(case (random 3)
((0) (simulate-user-action user ref-id "product-search"))
((1) (simulate-user-action user ref-id "product-list"))
((2) (simulate-user-action user ref-id "product-detail")))
(when (zero? (random 2))
(simulate-user-action user ref-id "order-view"))

(for-each
(lambda (service)
(simulate-microservice service user ref-id feature-flags))
services)

(simulate-observability user ref-id feature-flags)
(simulate-analytics user ref-id feature-flags)
(simulate-kibana user ref-id)))

(define (run-simulation num-requests)
(let* ((users (map (lambda (id) (make-user (number->string id) (generate-uuid)))
            (iota 10)))
(feature-flags (list (make-feature-flag "new_ui" #t)
                     (make-feature-flag "recommendation_engine" #f)
                     (make-feature-flag "fast_checkout" #t)))
(services (map make-service '("CustomerService" "OrderService" "ProductService"))))
(do ((i 0 (1+ i)))
((= i num-requests))
(format #t "~%~a~%" (color-text (format #f "--- Simulating User Journey ~a ---" (1+ i)) 'red))
(simulate-user-journey users services feature-flags)
(sleep 1))))

;; Main execution
(define (main args)
(random-source-randomize! default-random-source)
(run-simulation 5))

(main '())