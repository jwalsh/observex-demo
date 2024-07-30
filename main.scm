;; Distributed Tracing Demo Review Order

;; Define the directories and files to review
(define demo-dir "internal-developer-platform/demo/distributed-tracing")
(define platform-dir "internal-developer-platform/platform")
(define teams-dir "internal-developer-platform/teams")

;; The order in which files should be reviewed for a comprehensive understanding of the demo
(define review-order
  '(
    ;; 1. Readme and setup files
    ;; Start with the README file for an overview and setup instructions
    (,(string-append demo-dir "/README.org"))
    ;; Setup scripts to configure the environment
    (,(string-append demo-dir "/_setup-docker-config.sh"))
    (,(string-append demo-dir "/start-services.sh"))

    ;; 2. Configuration files
    ;; Configuration for various services and tools used in the demo
    (,(string-append demo-dir "/elasticsearch.yml"))
    (,(string-append demo-dir "/kibana.yml"))
    (,(string-append demo-dir "/logstash.conf"))
    (,(string-append demo-dir "/prometheus.yml"))

    ;; 3. Docker and containerization
    ;; Dockerfile for building the demo container
    (,(string-append demo-dir "/Dockerfile"))

    ;; 4. Distributed tracing and monitoring
    ;; Documentation and scripts related to distributed tracing and monitoring
    (,(string-append platform-dir "/innersource-distributed-tracing/README.org"))
    (,(string-append platform-dir "/monitoring/alerts"))
    (,(string-append platform-dir "/monitoring/dashboards"))
    (,(string-append platform-dir "/monitoring/grafana"))

    ;; 5. Team-specific configurations and services
    ;; Configurations and services for different teams
    (,(string-append teams-dir "/customer-experience/deployment/config"))
    (,(string-append teams-dir "/customer-experience/services/customer-service.js"))
    (,(string-append teams-dir "/order-fulfillment/deployment/config"))
    (,(string-append teams-dir "/order-fulfillment/services/order-service.java"))
    (,(string-append teams-dir "/product-catalog/deployment/config"))
    (,(string-append teams-dir "/product-catalog/services/catalog-service.java"))

    ;; 6. Testing and verification
    ;; Scripts and configurations for testing and verifying the demo setup
    (,(string-append demo-dir "/simulate_user_behavior.py"))
    (,(string-append teams-dir "/customer-experience/testing"))
    (,(string-append teams-dir "/order-fulfillment/testing"))
    (,(string-append teams-dir "/product-catalog/testing"))
    ))

;; Print the review order for reference
(for-each (lambda (file)
            (display file)
            (newline))
          review-order)

;; Additional instructions and information to enhance the demo experience

;; Ensure the environment is correctly set up before starting the review
(display "Ensure Docker and other dependencies are installed and configured correctly.")
(newline)
(display "Follow the setup instructions in the README file.")
(newline)

;; Example of expected outputs
(display "After running start-services.sh, you should see the following services up and running:")
(newline)
(display "- Elasticsearch: http://localhost:9200")
(newline)
(display "- Kibana: http://localhost:5601")
(newline)

;; Troubleshooting tips
(display "If you encounter issues with Elasticsearch not starting, check the elasticsearch.yml configuration.")
(newline)
(display "For Kibana, ensure it can connect to Elasticsearch and that the index patterns are correctly set up.")
(newline)

;; Architecture diagram
(display "Refer to the architecture diagram at internal-developer-platform/docs/architecture-diagram.png for an overview of the system.")
(newline)

;; Performance metrics
(display "Monitor the following metrics to ensure the system is functioning correctly:")
(newline)
(display "- Elasticsearch: Heap usage, indexing rate, search rate")
(newline)
(display "- Kibana: Response time, error rate")
(newline)
(display "- Logstash: Event processing rate, pipeline efficiency")
(newline)
(display "- Prometheus: Metrics scraping rate, alerting efficiency")
(newline)