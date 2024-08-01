;; File: config-tests.scm

;; Add current directory to load path
(set! %load-path (cons "." %load-path))

(use-modules (srfi srfi-64)
             (config)
             (ice-9 rdelim)
             (ice-9 regex))

;; Helper function to create a temporary configuration file
(define (create-temp-config content)
  (let* ((filename (string-append "/tmp/config-test-" (number->string (random 10000)) ".cfg"))
         (port (open-output-file filename)))
    (display content port)
    (close-output-port port)
    filename))

;; Test cases
(test-begin "Config Module Tests")

;; Test 1: Load a simple configuration
(test-assert "Load simple configuration"
  (let* ((config-content "key1=value1\nkey2=value2\n")
         (filename (create-temp-config config-content))
         (config (load-config filename)))
    (and (equal? (hash-ref config "key1") "value1")
         (equal? (hash-ref config "key2") "value2"))))

;; Test 2: Load configuration with spaces
(test-assert "Load configuration with spaces"
  (let* ((config-content "key1 = value1\n key2 = value2 \n")
         (filename (create-temp-config config-content))
         (config (load-config filename)))
    (and (equal? (hash-ref config "key1") "value1")
         (equal? (hash-ref config "key2") "value2"))))

;; Test 3: Load configuration with empty lines and comments
(test-assert "Load configuration with empty lines and comments"
  (let* ((config-content "\n# This is a comment\nkey1=value1\n\nkey2=value2\n")
         (filename (create-temp-config config-content))
         (config (load-config filename)))
    (and (equal? (hash-ref config "key1") "value1")
         (equal? (hash-ref config "key2") "value2"))))

;; Test 4: Load configuration with missing values
(test-assert "Load configuration with missing values"
  (let* ((config-content "key1=\nkey2=value2\n")
         (filename (create-temp-config config-content))
         (config (load-config filename)))
    (and (equal? (hash-ref config "key1") "")
         (equal? (hash-ref config "key2") "value2"))))

(test-end "Config Module Tests")

;; Run the tests if the script is executed directly
(when (equal? (current-module) (resolve-module '(guile-user)))
  (exit (test-runner-fail-count (test-runner-current))));; File: config-tests.scm

;; Add current directory to load path
(set! %load-path (cons "." %load-path))

(use-modules (srfi srfi-64)
             (config)
             (ice-9 rdelim)
             (ice-9 regex))

;; Helper function to create a temporary configuration file
(define (create-temp-config content)
  (let* ((filename (string-append "/tmp/config-test-" (number->string (random 10000)) ".cfg"))
         (port (open-output-file filename)))
    (display content port)
    (close-output-port port)
    filename))

;; Test cases
(test-begin "Config Module Tests")

;; Test 1: Load a simple configuration
(test-assert "Load simple configuration"
  (let* ((config-content "key1=value1\nkey2=value2\n")
         (filename (create-temp-config config-content))
         (config (load-config filename)))
    (and (equal? (hash-ref config "key1") "value1")
         (equal? (hash-ref config "key2") "value2"))))

;; Test 2: Load configuration with spaces
(test-assert "Load configuration with spaces"
  (let* ((config-content "key1 = value1\n key2 = value2 \n")
         (filename (create-temp-config config-content))
         (config (load-config filename)))
    (and (equal? (hash-ref config "key1") "value1")
         (equal? (hash-ref config "key2") "value2"))))

;; Test 3: Load configuration with empty lines and comments
(test-assert "Load configuration with empty lines and comments"
  (let* ((config-content "\n# This is a comment\nkey1=value1\n\nkey2=value2\n")
         (filename (create-temp-config config-content))
         (config (load-config filename)))
    (and (equal? (hash-ref config "key1") "value1")
         (equal? (hash-ref config "key2") "value2"))))

;; Test 4: Load configuration with missing values
(test-assert "Load configuration with missing values"
  (let* ((config-content "key1=\nkey2=value2\n")
         (filename (create-temp-config config-content))
         (config (load-config filename)))
    (and (equal? (hash-ref config "key1") "")
         (equal? (hash-ref config "key2") "value2"))))

(test-end "Config Module Tests")

;; Run the tests if the script is executed directly
(when (equal? (current-module) (resolve-module '(guile-user)))
  (exit (test-runner-fail-count (test-runner-current))))
