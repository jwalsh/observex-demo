(add-to-load-path ".")
(use-modules (jaeger))


(define (main args)
  (init-jaeger #:verbose? #t #:debug? #t)

  ; Mock calls to test Jaeger integration
  (send-span-to-jaeger "CDN/Edge Network" "receive-request" 50 "2cfd63d4-5a76-e219-dbd0-93f69c0ec02f")
  (send-span-to-jaeger "Frontend Proxy/Load Balancer" "route-request" 50 "2cfd63d4-5a76-e219-dbd0-93f69c0ec02f")
  (send-span-to-jaeger "Backend for Frontend (BFF)" "process-request" 100 "2cfd63d4-5a76-e219-dbd0-93f69c0ec02f")
  (send-span-to-jaeger "API Gateway" "route-request" 50 "2cfd63d4-5a76-e219-dbd0-93f69c0ec02f")
  (send-span-to-jaeger "Service Mesh" "direct-request" 50 "2cfd63d4-5a76-e219-dbd0-93f69c0ec02f")
  (send-span-to-jaeger "Home Service" "fetch-home-page-content" 200 "2cfd63d4-5a76-e219-dbd0-93f69c0ec02f")
  (send-span-to-jaeger "Customer Service" "process-request" 100 "2cfd63d4-5a76-e219-dbd0-93f69c0ec02f")
  (send-span-to-jaeger "Database" "query-data" 100 "2cfd63d4-5a76-e219-dbd0-93f69c0ec02f")

  (send-span-to-jaeger "Order Service" "process-order" 200 "order-123")
  (send-span-to-jaeger "Inventory Service" "check-inventory" 100 "order-123")
  (send-span-to-jaeger "Catalog Service" "fetch-catalog-data" 200 "product-456")
  (send-span-to-jaeger "Product Service" "fetch-product-data" 100 "product-456")
  (send-span-to-jaeger "Pricing Service" "calculate-price" 50 "product-456")

  (send-span-to-jaeger "Customer Analytics" "track-event" 50 "event-789")
  (send-span-to-jaeger "Google Analytics" "send-event" 50 "event-789")
)

(main '())