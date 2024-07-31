(define-module (jaeger)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:export (init-jaeger send-span-to-jaeger))

(define jaeger-url "http://localhost:14268/api/traces")
(define jaeger-health-url "http://localhost:14268/health")

(define jaeger-initialized? #f)
(define jaeger-available? #f)
(define debug-mode? #f)

(define (random-integer max)
  (let ((rand (random max)))
    (if (= rand max)
        (random-integer max)
        rand)))

(define (current-time-ms)
  (let ((time (gettimeofday)))
    (+ (* 1000 (car time)) (cdr time))))

(define (json-escape-string str)
  (string-join
   (string-split
    (string-map
     (lambda (c)
       (case c
         ((#\" #\\) (string #\\ c))
         ((#\newline) "\\n")
         ((#\return) "\\r")
         ((#\tab) "\\t")
         (else c)))
     str)
    (string->char-set ""))
   ""))

(define (json-encode data)
  (define (encode-pair pair)
    (string-append
     (json-encode (car pair))
     ":"
     (json-encode (cdr pair))))
  
  (cond
   ((null? data) "null")
   ((boolean? data) (if data "true" "false"))
   ((number? data) (number->string data))
   ((string? data) (string-append "\"" (json-escape-string data) "\""))
   ((pair? data)
    (if (list? data)
        (if (and (not (null? data)) (pair? (car data)))
            (string-append
             "{"
             (string-join (map encode-pair data) ",")
             "}")
            (string-append
             "["
             (string-join (map json-encode data) ",")
             "]"))
        (encode-pair data)))
   ((vector? data)
    (string-append
     "["
     (string-join (map json-encode (vector->list data)) ",")
     "]"))
   (else (error "Unsupported data type" data))))

(define (debug-print message)
  (when debug-mode?
    (display message)
    (newline)))

(define (execute-curl-command command)
  (let ((temp-file (tmpnam)))
    (system (string-append command " > " temp-file " 2>&1"))
    (let ((output (call-with-input-file temp-file get-string-all)))
      (delete-file temp-file)
      output)))

(define (init-jaeger . args)
  (when (not jaeger-initialized?)
    (set! jaeger-initialized? #t)
    (let* ((verbose? (if (and (not (null? args)) (eq? (car args) #:verbose?))
                         (cadr args)
                         #t))
           (debug? (if (and (> (length args) 2) (eq? (caddr args) #:debug?))
                       (cadddr args)
                       #f)))
      (set! debug-mode? debug?)
      (when verbose?
        (display "Initializing Jaeger...\n")
        (format #t "Jaeger Health URL: ~a\n" jaeger-health-url)
        (format #t "Jaeger URL: ~a\n" jaeger-url))
      (let ((curl-command (format #f "curl -s -o /dev/null -w %{http_code} ~a" jaeger-health-url)))
        (debug-print (string-append "Executing command: " curl-command))
        (let ((status-code (execute-curl-command curl-command)))
          (set! jaeger-available? (string=? (string-trim-both status-code) "200"))
          (debug-print (format #f "Status code: ~a" status-code))))
      (unless jaeger-available?
        (display "Jaeger is not available, ignoring spans...\n")
        (when verbose?
          (display "Warning: Jaeger initialization failed.\n"))))))

(define (send-span-to-jaeger service-name span-name duration-ms ref-id)
  (when (not jaeger-initialized?)
    (init-jaeger))
  (let* ((trace-id (number->string (random-integer 10000000000000000000)))
         (span-data 
          (list
           (cons "traceId" trace-id)
           (cons "payload"
                 (list
                  (cons "id" (number->string (random-integer 10000000000000000)))
                  (cons "name" span-name)
                  (cons "kind" "SERVER")
                  (cons "timestamp" (number->string (current-time-ms)))
                  (cons "duration" (number->string duration-ms))
                  (cons "localEndpoint" 
                        (list
                         (cons "serviceName" service-name)
                         (cons "ipv4" "127.0.0.1")
                         (cons "port" 8080)))
                  (cons "tags" 
                        (list
                         (cons "http.method" "GET")
                         (cons "http.status_code" "200")
                         (cons "refId" ref-id))))))))
    (let ((json-data (json-encode span-data)))
      (debug-print (format #f "Sending span to Jaeger: ~a" json-data))
      (if jaeger-available?
          (let ((curl-command (string-append 
                               "curl -X POST -H \"Content-Type: application/json\" --data '"
                               json-data
                               "' " jaeger-url)))
            (debug-print (string-append "Executing command: " curl-command))
            (let ((response (execute-curl-command curl-command)))
              (debug-print (format #f "Curl response: ~a" response))
              (if (string-contains response "\"status\": \"success\"")
                  (begin
                    (display "Span sent successfully\n")
                    #t)
                  (begin
                    (display "Error sending span: ")
                    (display response)
                    (newline)
                    #f))))
          (begin
            (display "Jaeger is not available. Span not sent.\n")
            #f)))))
