; irc-notifier-test.scm

(add-to-load-path ".")

; -- Sane Defaults --
(define default-server "irc.freenode.net")
(define default-port 6667)
(define default-nick "guile-observer")
(define default-channel "#guile-observer-test")
(define default-user "guile-observer")
(define default-password "")

; -- Load IRC Notifier --
(load "irc-notifier.scm")

; -- Test IRC Notifier --
(define (test-irc-notifier)
  (let ((irc-settings `((server . ,default-server)
                        (port . ,default-port)
                        (nick . ,default-nick)
                        (channel . ,default-channel)
                        (user . ,default-user)
                        (password . ,default-password))))
    (irc-notify-start irc-settings "Test Simulation")
    (irc-notify-info irc-settings "Test message")
    (irc-notify-warn irc-settings "Test warning")
    (irc-notify-error irc-settings "Test error")
    (irc-notify-stop irc-settings "Test Simulation")))

; -- Run Tests --
(test-irc-notifier)
