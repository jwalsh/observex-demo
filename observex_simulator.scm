(use-modules (ice-9 format)
    (srfi srfi-1)
    (srfi srfi-19)
    (srfi srfi-27))

;; Data Structures
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

;; Utility Functions
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
   component action requester-type requester-id ref-id)))

;; Simulation Functions
(define (simulate-cdn-edge user ref-id)
(log-request "CDN/Edge" user "Request received" ref-id)
ref-id)

(define (simulate-frontend-proxy user ref-id)
(log-request "Frontend Proxy" user "Routing request" ref-id)
ref-id)

(define (simulate-bff user ref-id feature-flags)
(log-request "BFF" user "Processing request" ref-id)
(values ref-id feature-flags))

(define (simulate-api-gateway user ref-id feature-flags)
(log-request "API Gateway" user "Authenticating" ref-id)
(values ref-id feature-flags))

(define (simulate-service-mesh user ref-id feature-flags)
(log-request "Service Mesh" user "Routing request" ref-id)
(values ref-id feature-flags))

(define (simulate-microservice service user ref-id feature-flags)
(log-request (service-name service) user "Processing request" ref-id)
(format #t "~a: Active feature flags: ~a~%"
 (service-name service)
 (filter feature-flag-enabled feature-flags))

;; Simulate database call
(log-request "Database" service "Querying data" ref-id)

;; Simulate calls to additional services
(simulate-prometheus service ref-id)
(simulate-elasticsearch service ref-id)
(simulate-logstash service ref-id))

(define (simulate-observability user ref-id feature-flags)
(log-request "Observability" user "Logging event" ref-id)
(format #t "Observability: Feature flags state: ~a~%" feature-flags))

(define (simulate-analytics user ref-id feature-flags)
(log-request "Analytics" user "Sending event to GA" ref-id)
(format #t "Analytics: Active feature flags: ~a~%"
 (filter feature-flag-enabled feature-flags)))

(define (simulate-prometheus service ref-id)
(log-request "Prometheus" service "Collecting metrics" ref-id))

(define (simulate-elasticsearch service ref-id)
(log-request "Elasticsearch" service "Indexing logs" ref-id))

(define (simulate-logstash service ref-id)
(log-request "Logstash" service "Processing logs" ref-id))

(define (simulate-kibana user ref-id)
(log-request "Kibana" user "Visualizing data" ref-id))

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
(call-with-values 
(lambda () (simulate-api-gateway user ref-id feature-flags))
(lambda (new-ref-id new-flags)
(set! ref-id new-ref-id)
(set! feature-flags new-flags)))
(call-with-values 
(lambda () (simulate-service-mesh user ref-id feature-flags))
(lambda (new-ref-id new-flags)
(set! ref-id new-ref-id)
(set! feature-flags new-flags)))

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
(format #t "~%--- Simulating User Journey ~a ---~%" (1+ i))
(simulate-user-journey users services feature-flags)
(sleep 1))))

;; Main execution
(define (main args)
(random-source-randomize! default-random-source)
(run-simulation 5))

(main '())