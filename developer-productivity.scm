; developer-productivity.scm - GitHub/Git Stats Checker

; Description:
;   Fetches and displays developer productivity statistics from either the local
;   Git log or a GitHub repository, controlled by a command-line flag.

; Author: Jason Walsh <j@wal.sh>
; Date: 2024-08-01

; -- Dependencies --
(use-modules (web uri)          ; For URL handling (GitHub)
             (web client)       ; For HTTP requests (GitHub)
             (json)            ; For JSON parsing (GitHub)
             (ice-9 popen)     ; For subprocesses (Git)
             (srfi srfi-1)     ; For list functions)

; -- Configuration --
(define default-use-github? #f) ; Default: Check local Git log
(define github-token (getenv "GITHUB_TOKEN"))

; Default Git Log Settings
(define default-git-repo ".")  ; Current directory
(define default-since "1 week ago")

; -- Argument Parsing --
(define (parse-args args)
  (let loop ((args args) (use-github? default-use-github?))
    (cond ((null? args) use-github?)
          ((string=? (car args) "--github") (loop (cdr args) #t))
          (else (error "Invalid command-line argument: " (car args))))))

; -- Utility Functions --

(define (run-git-command . args)
  (let ((port (open-input-pipe (string-append "git " (string-join args " ")))))
    (let loop ((lines '()))
      (let ((line (read-line port)))
        (if (eof-object? line)
            (reverse lines)
            (loop (cons line lines)))))))

(define (get-commit-activity)
  (if (or github-token (and (not github-token) default-use-github?)) ; Use GitHub if token or flag is set
      (let ((endpoint (string-append "/repos/" target-owner "/" target-repo "/commits?per_page=100"))
            (commits (github-api-request endpoint)))
        (filter (lambda (commit) (> (time->seconds (current-time))
                                   (- (time->seconds (string->time (hash-ref commit 'commit 'author 'date)))
                                      (* 604800))))  ; 7 days in seconds
                commits))
      ;; Otherwise, use the local Git log
      (let ((commits (run-git-command "log" "--since=" default-since "--oneline")))
        (length commits))))

(define (github-api-request endpoint)
  (let ((url (string-append "https://api.github.com" endpoint)))
    (let ((response (http-get url
                             (list (header "Authorization" (string-append "Bearer " github-token))))))
      (if (equal? (car response) 200)   ; Check for success (200 OK)
          (json-read (cadr response))  ; Parse the JSON response body
          (error "GitHub API request failed")))))

; -- Main Function --
(define (main args)
  (let* ((use-github? (parse-args args))
         (target-owner (and (pair? args) (car args)))
         (target-repo (and (pair? (cdr args)) (cadr args))))
    (if (and use-github? (not github-token))
        (error "GITHUB_TOKEN environment variable not set"))
    (format #t "Commits in the past week: ~a\n" (get-commit-activity))
    ; ... (Print other statistics here) ...
    ))

(main (command-line))
