(use-modules (ice-9 format))
(add-to-load-path ".")
(use-modules (jaeger))

;; Define a helper function to generate random trace IDs
(define (generate-trace-id)
  (format #f "~36,16,'0x" (random (expt 2 128))))

;; Define a helper function to send a batch of spans
(define (send-batch-spans spans trace-id)
  (for-each
   (lambda (span)
     (send-span-to-jaeger
      (car span)
      (cadr span)
      (caddr span)
      trace-id))
   spans))

(define (simulate-microservices-request)
  (let ((trace-id (generate-trace-id)))
    (send-batch-spans
     '(("CDN/Edge Network" "receive-request" 50)
       ("Frontend Proxy/Load Balancer" "route-request" 50)
       ("Backend for Frontend (BFF)" "process-request" 100)
       ("API Gateway" "route-request" 50)
       ("Service Mesh" "direct-request" 50)
       ("Home Service" "fetch-home-page-content" 200)
       ("Customer Service" "process-request" 100)
       ("Database" "query-data" 100))
     trace-id)))

(define (simulate-order-processing)
  (let ((trace-id (generate-trace-id)))
    (send-batch-spans
     '(("Order Service" "process-order" 200)
       ("Inventory Service" "check-inventory" 100))
     trace-id)))

(define (simulate-product-catalog-request)
  (let ((trace-id (generate-trace-id)))
    (send-batch-spans
     '(("Catalog Service" "fetch-catalog-data" 200)
       ("Product Service" "fetch-product-data" 100)
       ("Pricing Service" "calculate-price" 50))
     trace-id)))

(define (simulate-analytics-event)
  (let ((trace-id (generate-trace-id)))
    (send-batch-spans
     '(("Customer Analytics" "track-event" 50)
       ("Google Analytics" "send-event" 50))
     trace-id)))

(define (main args)
  ;; Initialize Jaeger with verbose and debug modes enabled
  (init-jaeger #:verbose? #t #:debug? #t)
  
  ;; Simulate different scenarios
  (simulate-microservices-request)
  (simulate-order-processing)
  (simulate-product-catalog-request)
  (simulate-analytics-event)
  
  ;; Force a reset of the Jaeger client (if needed for testing)
  (init-jaeger #:verbose? #t #:debug? #t)
  
  ;; Additional simulation after reset (if needed)
  (simulate-microservices-request))

;; Run the main function
(main '())
