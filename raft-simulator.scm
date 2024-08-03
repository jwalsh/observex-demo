;;; File: raft-simulator.scm
;;; An enhanced Raft consensus algorithm simulator in Guile Scheme

(use-modules (ice-9 format) (srfi srfi-1) (srfi srfi-2))
(load "simulator-utils.scm")

(define *nodes* '())
(define *leader* #f)
(define *term* 0)
(define *logs* (make-hash-table))
(define *commit-index* 0)
(define *partitioned-nodes* '())
(define *failed-nodes* '())
(define *time* 0)

;;; Utility functions
(define (log message color)
  (display (color-text (format #f "[Time ~a] ~a\n" *time* message) color)))

;;; Initialize nodes
(define (init-nodes n)
  (set! *nodes* (iota n))
  (for-each (lambda (node) (hash-set! *logs* node '())) *nodes*))

;;; Start an election
(define (start-election candidate)
  (if (or (member candidate *partitioned-nodes*)
          (member candidate *failed-nodes*))
      (log (format "Node ~a is partitioned or failed and cannot start an election" candidate) 'red)
      (begin
        (set! *term* (+ *term* 1))
        (let ((votes 1) ;; Candidate votes for itself
              (quorum (/ (+ (length *nodes*) 2) 2))) ;; Majority needed
          (for-each
           (lambda (node)
             (unless (or (eq? node candidate)
                         (member node *partitioned-nodes*)
                         (member node *failed-nodes*))
               (when (< (random-uniform) 0.8) ;; 80% chance of voting for the candidate
                 (set! votes (+ votes 1)))))
           *nodes*)
          (if (>= votes quorum)
              (begin
                (set! *leader* candidate)
                (log (format "Node ~a is elected as the leader for term ~a" candidate *term*) 'green))
              (log (format "Node ~a failed to get enough votes for term ~a" candidate *term*) 'yellow))))))

;;; Append an entry to the log
(define (append-entry leader command)
  (if (eq? leader *leader*)
      (let ((quorum (/ (+ (length *nodes*) 2) 2))
            (acks 1)) ;; Leader acknowledges itself
        (for-each
         (lambda (node)
           (unless (or (member node *partitioned-nodes*)
                       (member node *failed-nodes*))
             (when (< (random-uniform) 0.8) ;; 80% chance of appending the entry
               (hash-set! *logs* node (append (hash-ref *logs* node) (list command)))
               (set! acks (+ acks 1)))))
         *nodes*)
        (if (>= acks quorum)
            (begin
              (hash-set! *logs* leader (append (hash-ref *logs* leader) (list command)))
              (set! *commit-index* (+ *commit-index* 1))
              (log (format "Command ~a committed by leader ~a" command leader) 'blue))
            (log "Failed to achieve quorum for the command" 'red)))
      (log "Not the leader" 'red)))

;;; Partition nodes
(define (partition-nodes nodes)
  (set! *partitioned-nodes* nodes)
  (log (format "Partitioned nodes: ~a" nodes) 'magenta))

;;; Heal partition
(define (heal-partition)
  (set! *partitioned-nodes* '())
  (log "All partitions healed" 'cyan))

;;; Fail nodes
(define (fail-nodes nodes)
  (set! *failed-nodes* nodes)
  (log (format "Failed nodes: ~a" nodes) 'red))

;;; Recover nodes
(define (recover-nodes nodes)
  (set! *failed-nodes* (remove* nodes *failed-nodes*))
  (log (format "Recovered nodes: ~a" nodes) 'green))

;;; Show logs
(define (show-logs)
  (for-each
   (lambda (node)
     (log (format "Node ~a log: ~a" node (hash-ref *logs* node)) 'yellow))
   *nodes*))

;;; Advance time
(define (advance-time steps)
  (do ((i 0 (+ i 1)))
      ((= i steps))
    (set! *time* (+ *time* 1))
    ;; You can add time-based events or checks here
    ))

;;; Main simulation
(define (run-simulation)
  (init-nodes 5)
  (advance-time 1)
  (start-election 1)
  (advance-time 1)
  (append-entry 1 '("set x 10"))
  (advance-time 1)
  (partition-nodes '(2 3))
  (advance-time 1)
  (append-entry 1 '("set y 20"))
  (advance-time 1)
  (heal-partition)
  (advance-time 1)
  (fail-nodes '(4))
  (advance-time 1)
  (append-entry 1 '("set z 30"))
  (advance-time 1)
  (recover-nodes '(4))
  (advance-time 1)
  (show-logs))

(run-simulation)
