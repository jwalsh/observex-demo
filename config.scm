;; File: config.scm

(define-module (config)
  #:export (load-config))

(use-modules (ice-9 rdelim)
             (ice-9 regex))

(define (load-config filename)
  (let ((config (make-hash-table)))
    (with-input-from-file filename
      (lambda ()
        (let loop ((line (read-line)))
          (if (not (eof-object? line))
              (let ((match (string-match "^([^=]+)=(.*)$" line)))
                (when match
                  (hash-set! config
                             (string-trim-both (match:substring match 1))
                             (string-trim-both (match:substring match 2))))
                (loop (read-line)))))))
    config))
