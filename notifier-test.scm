; notifier-test.scm - Test script for the IRC bot notifier

; Description:
;   This script demonstrates how to interact with the IRC bot defined in notifier.scm.
;   It includes test cases for joining a channel, sending a message, and quitting the bot.

; Author: Your Name (or leave as is)
; Date: 2024-08-01

; -- Load Path Setup --
(use-modules (ice-9 rdelim)) ; For better path manipulation
(add-to-load-path (dirname (current-filename)))

; -- Import --
(import (notifier))

; -- Sane Defaults --
(define default-server "irc.freenode.net")
(define default-port 6667)
(define default-nick "guile-observer")
(define default-channel "#observers")

; -- Test Functions --
(define (test-join-channel notifier)
  (post-message notifier (string-append "join " default-channel))
  (print "Test: Joining Channel"))

(define (test-say-message notifier)
  (post-message notifier (string-append "say " default-channel " Hello from the test script!"))
  (print "Test: Sending Message"))

(define (test-quit notifier)
  (post-message notifier "quit")
  (print "Test: Quitting"))

; -- Main --
(define (main)
  (let ((notifier (make-notifier "Notifier Test")))
    (if notifier
        (begin
          (test-join-channel notifier)
          (test-say-message notifier)
          (test-quit notifier))
        (error "Failed to create notifier."))))

(main)
