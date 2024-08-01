; notifier.scm

(define* (make-notifier #:key (driver 'console) (simulator-name "") (irc-settings '()) (slack-settings '()) (email-settings '()) (pagerduty-settings '()))
  (define (notify-start simulation-name)
    (case driver
      ((irc)
       (irc-notify-start irc-settings simulation-name))
      ((slack)
       (slack-notify-start slack-settings simulation-name))
      ((email)
       (email-notify-start email-settings simulation-name))
      ((pagerduty)
       (pagerduty-notify-start pagerduty-settings simulation-name))
      (else
       (console-notify-start simulation-name))))

  (define (notify-stop simulation-name)
    (case driver
      ((irc)
       (irc-notify-stop irc-settings simulation-name))
      ((slack)
       (slack-notify-stop slack-settings simulation-name))
      ((email)
       (email-notify-stop email-settings simulation-name))
      ((pagerduty)
       (pagerduty-notify-stop pagerduty-settings simulation-name))
      (else
       (console-notify-stop simulation-name))))

  (define (notify-info message)
    (case driver
      ((irc)
       (irc-notify-info irc-settings message))
      ((slack)
       (slack-notify-info slack-settings message))
      ((email)
       (email-notify-info email-settings message))
      ((pagerduty)
       (pagerduty-notify-info pagerduty-settings message))
      (else
       (console-notify-info message))))

  (define (notify-warn message)
    (case driver
      ((irc)
       (irc-notify-warn irc-settings message))
      ((slack)
       (slack-notify-warn slack-settings message))
      ((email)
       (email-notify-warn email-settings message))
      ((pagerduty)
       (pagerduty-notify-warn pagerduty-settings message))
      (else
       (console-notify-warn message))))

  (define (notify-error message)
    (case driver
      ((irc)
       (irc-notify-error irc-settings message))
      ((slack)
       (slack-notify-error slack-settings message))
      ((email)
       (email-notify-error email-settings message))
      ((pagerduty)
       (pagerduty-notify-error pagerduty-settings message))
      (else
       (console-notify-error message))))

  (define (irc-notify-start irc-settings simulation-name)
    (let ((server (assoc-ref irc-settings 'server))
          (port (assoc-ref irc-settings 'port))
          (channel (assoc-ref irc-settings 'channel))
          (nick (assoc-ref irc-settings 'nick))
          (user (assoc-ref irc-settings 'user))
          (password (assoc-ref irc-settings 'password)))
      ; Use the IRC API to send a notification
      ))

  (define (irc-notify-stop irc-settings simulation-name)
    (let ((server (assoc-ref irc-settings 'server))
          (port (assoc-ref irc-settings 'port))
          (channel (assoc-ref irc-settings 'channel))
          (nick (assoc-ref irc-settings 'nick))
          (user (assoc-ref irc-settings 'user))
          (password (assoc-ref irc-settings 'password)))
      ; Use the IRC API to send a notification
      ))

  (define (slack-notify-start slack-settings simulation-name)
    (let ((token (assoc-ref slack-settings 'token))
          (channel (assoc-ref slack-settings 'channel))
          (username (assoc-ref slack-settings 'username))
          (icon (assoc-ref slack-settings 'icon)))
      ; Use the Slack API to send a notification
      ))

  (define (slack-notify-stop slack-settings simulation-name)
    (let ((token (assoc-ref slack-settings 'token))
          (channel (assoc-ref slack-settings 'channel))
          (username (assoc-ref slack-settings 'username))
          (icon (assoc-ref slack-settings 'icon)))
      ; Use the Slack API to send a notification
      ))

  (define (email-notify-start email-settings simulation-name)
    (let ((smtp-server (assoc-ref email-settings 'smtp-server))
          (smtp-port (assoc-ref email-settings 'smtp-port))
          (from-address (assoc-ref email-settings 'from-address))
          (to-address (assoc-ref email-settings 'to-address))
          (subject (assoc-ref email-settings 'subject)))
      ; Use an email library to send a notification
      ))

  (define (email-notify-stop email-settings simulation-name)
    (let ((smtp-server (assoc-ref email-settings 'smtp-server))
          (smtp-port (assoc-ref email-settings 'smtp-port))
          (from-address (assoc-ref email-settings 'from-address))
          (to-address (assoc-ref email-settings 'to-address))
          (subject (assoc-ref email-settings 'subject)))
      ; Use an email library to send a notification
      ))

  (define (pagerduty-notify-start pagerduty-settings simulation-name)
    (let ((api-key (assoc-ref pagerduty-settings 'api-key))
          (service-id (assoc-ref pagerduty-settings 'service-id))
          (event-action (assoc-ref pagerduty-settings 'event-action)))
      ; Use the PagerDuty API to send a notification
      ))

  (define (pagerduty-notify-stop pagerduty-settings simulation-name)
    (let ((api-key (assoc-ref pagerduty-settings 'api-key))
          (service-id (assoc-ref pagerduty-settings 'service-id))
          (event-action (assoc-ref pagerduty-settings 'event-action)))
      ; Use the PagerDuty API to send a notification
      ))

  (let ((notification-level (getenv (string-append (string-upcase simulator-name) "_NOTIFICATION_LEVEL"))))
    (case notification-level
      ((info)
       (set! notify-info (lambda (message) (notify-info message))))
      ((warn)
       (set! notify-warn (lambda (message) (notify-warn message))))
      ((error)
       (set! notify-error (lambda (message) (notify-error message))))
      (else
       (set! notify-info (lambda (message) (notify-info message))))))

  (lambda (method . args)
    (case method
      ((notify-start) (apply notify-start args))
      ((notify-stop) (apply notify-stop args))
      ((notify-info) (apply notify-info args))
      ((notify-warn) (apply notify-warn args))
      ((notify-error) (apply notify-error args))
      (else (error "Unknown method")))))
