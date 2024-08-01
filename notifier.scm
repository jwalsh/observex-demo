; notifier.scm - IRC bot core module

; Description:
;   This module provides the core functionality for the IRC bot.
;   It handles message dispatch, connecting to the server, joining channels,
;   and sending messages.

; Author: Your Name (or leave as is)
; Date: 2024-08-01

; -- Module Initialization --
(define-module (notifier)
  #:use-module (ice-9 receive)   ; For pattern matching
  #:use-module (ice-9 rdelim)     ; For better path manipulation
  #:use-module (rnrs bytevectors) ; For binary data handling
  #:use-module (srfi srfi-13)    ; For string library functions
  #:use-module (srfi srfi-18)    ; For multithreading

  #:export (make-notifier
            post-message
            wait-for-message))

; -- Default Values --
(define default-server "irc.freenode.net")
(define default-port 6667)
(define default-nick "guile-notifier")
(define default-channel "#observers")

; -- Data Structures --

(define-record-type <notifier>
  (make-notifier name)
  notifier-name
  (mutable notifier-channel #f)) ; For internal message passing

; -- Core Functions --

;; Creates a new notifier object and initializes it
(define (make-notifier name)
  (let ((notifier (make-notifier name)))
    (set-notifier-channel! notifier (make-channel))
    notifier))

;; Posts a message to the notifier
(define (post-message notifier message)
  (channel-put! (notifier-channel notifier) message))

;; Waits for a message from the notifier, blocking until one is available
(define (wait-for-message notifier)
  (channel-get (notifier-channel notifier)))


; -- IRC Functionality --
(define (irc-bot-loop notifier)
  (let ((server (get-parameter notifier 'server default-server))
        (port (get-parameter notifier 'port default-port))
        (nick (get-parameter notifier 'nick default-nick))
        (channel (get-parameter notifier 'channel default-channel)))

    ;; Connect to the IRC server
    (let ((connection (open-tcp-stream server port)))
      ;; Send registration commands (NICK, USER)
      (display (format "NICK ~a\r\n" nick) connection)
      (display (format "USER ~a 0 * :Guile IRC Bot\r\n" nick) connection)

      ;; Join the specified channel
      (if channel
          (display (format "JOIN ~a\r\n" channel) connection))

      ;; Main loop: read and process messages
      (let loop ()
        (receive (line (read-line connection))
          ;; Handle PING messages to keep the connection alive
          (if (string-prefix? "PING :" line)
              (begin
                (display (string-append "PONG :" (substring line 6) "\r\n") connection)
                (loop))
              ;; Process other messages from the server
              (post-message notifier line))
          ;; Process messages from the notifier object
          (receive (message (wait-for-message notifier))
            (if (string=? message "quit")
                (exit)
                (display (string-append message "\r\n") connection)))))))

(define (get-parameter notifier keyword default)
  (let ((value (assoc keyword (notifier-parameters notifier))))
    (if value
        (cadr value)
        default)))

; Start the bot thread
(thread-start! (lambda () (irc-bot-loop (make-notifier "IRC Bot"))))
