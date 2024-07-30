(define-module (jaeger)
  #:export (init-jaeger send-span-to-jaeger))

(define (random-integer max)
  (let ((rand (random max)))
    (if (eq? rand max)
        (random-integer max)
        rand)))

(define (current-time)
  (let ((time (gettimeofday)))
    (+ (car time) (* 1000 (cdr time)))))

(define (string-join strings delimiter)
  (cond ((null? strings) "")
        ((null? (cdr strings)) (car strings))
        (else (string-append (car strings) delimiter (string-join (cdr strings) delimiter)))))

(define (json-encode data)
  (cond ((null? data) "null")
        ((boolean? data) (if data "true" "false"))
        ((number? data) (number->string data))
        ((string? data) (string-append "\"" data "\""))
        ((list? data)
         (if (pair? (car data))
             (string-append "{" (string-join (map (lambda (pair) (string-append (json-encode (car pair)) ":" (json-encode (cdr pair)))) data) ",") "}")
             (string-append "[" (string-join (map json-encode data) ",") "]")))
        (else (error "Unsupported data type"))))

(define jaeger-url "http://localhost:14268/api/traces")
(define jaeger-health-url "http://localhost:14268/health")

(define jaeger-available? #f)
(define debug-mode? #f)

(define (init-jaeger . args)
  (let* ((verbose? (if (member #:verbose? args) (cadr (memq #:verbose? args)) #t))
         (debug? (if (member #:debug? args) (cadr (memq #:debug? args)) #f)))
    (set! debug-mode? debug?)
    (set! jaeger-available?
          (eq? (system* "curl" "-s" "-o" "/dev/null" "-w" "%{http_code}" jaeger-health-url) "200"))
    (unless jaeger-available?
      (display "Jaeger is not available, ignoring spans...\n")
      (when verbose?
        (display "Warning: Jaeger initialization failed.\n")))))

(define (debug-print-span span-data)
  (when debug-mode?
    (display "Span Data:\n")
    (display (json-encode span-data))
    (newline)))

(define (send-span-to-jaeger service-name span-name duration-ms ref-id)
  (let ((span-data (list
                     (cons "traceId" (number->string (random-integer 10000000000000000000)))
                     (cons "id" (number->string (random-integer 10000000000000000)))
                     (cons "name" span-name)
                     (cons "kind" "SERVER")
                     (cons "timestamp" (number->string (current-time)))
                     (cons "duration" (number->string duration-ms))
                     (cons "localEndpoint" (list
                                             (cons "serviceName" service-name)
                                             (cons "ipv4" "127.0.0.1")
                                             (cons "port" "8080")))
                     (cons "tags" (list
                                    (cons "http.method" "GET")
                                    (cons "http.status_code" "200")
                                    (cons "refId" ref-id))))))
    (debug-print-span span-data)
    (if jaeger-available?
        (begin
          (system* "curl" "-X" "POST" "-H" "Content-Type: application/json" "-d" (json-encode span-data) jaeger-url)
          #t)
        #f)))