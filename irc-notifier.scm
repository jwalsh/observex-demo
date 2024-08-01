; irc-notifier.scm

(define (irc-notify-start irc-settings simulation-name)
  (let ((server (assoc-ref irc-settings 'server))
        (port (assoc-ref irc-settings 'port))
        (channel (assoc-ref irc-settings 'channel))
        (nick (assoc-ref irc-settings 'nick))
        (user (assoc-ref irc-settings 'user))
        (password (assoc-ref irc-settings 'password)))
    (irc-connect server port nick user password)
    (irc-join channel)
    (irc-send-message channel (format #f "~a started at ~a" simulation-name (current-time)))
    (irc-disconnect)))

(define (irc-notify-stop irc-settings simulation-name)
  (let ((server (assoc-ref irc-settings 'server))
        (port (assoc-ref irc-settings 'port))
        (channel (assoc-ref irc-settings 'channel))
        (nick (assoc-ref irc-settings 'nick))
        (user (assoc-ref irc-settings 'user))
        (password (assoc-ref irc-settings 'password)))
    (irc-connect server port nick user password)
    (irc-join channel)
    (irc-send-message channel (format #f "~a stopped at ~a" simulation-name (current-time)))
    (irc-disconnect)))

(define (irc-notify-info irc-settings message)
  (let ((server (assoc-ref irc-settings 'server))
        (port (assoc-ref irc-settings 'port))
        (channel (assoc-ref irc-settings 'channel))
        (nick (assoc-ref irc-settings 'nick))
        (user (assoc-ref irc-settings 'user))
        (password (assoc-ref irc-settings 'password)))
    (irc-connect server port nick user password)
    (irc-join channel)
    (irc-send-message channel (format #f "~a: ~a" nick message))
    (irc-disconnect)))

(define (irc-notify-warn irc-settings message)
  (let ((server (assoc-ref irc-settings 'server))
        (port (assoc-ref irc-settings 'port))
        (channel (assoc-ref irc-settings 'channel))
        (nick (assoc-ref irc-settings 'nick))
        (user (assoc-ref irc-settings 'user))
        (password (assoc-ref irc-settings 'password)))
    (irc-connect server port nick user password)
    (irc-join channel)
    (irc-send-message channel (format #f "~a: ~a" nick message))
    (irc-disconnect)))

(define (irc-notify-error irc-settings message)
  (let ((server (assoc-ref irc-settings 'server))
        (port (assoc-ref irc-settings 'port))
        (channel (assoc-ref irc-settings 'channel))
        (nick (assoc-ref irc-settings 'nick))
        (user (assoc-ref irc-settings 'user))
        (password (assoc-ref irc-settings 'password)))
    (irc-connect server port nick user password)
    (irc-join channel)
    (irc-send-message channel (format #f "~a: ~a" nick message))
    (irc-disconnect)))

(define (irc-connect server port nick user password)
  ; Connect to the IRC server using the provided credentials
  (let ((connection (make-irc-connection server port)))
    (irc-login connection nick user password)
    connection))

(define (irc-join channel)
  ; Join the specified IRC channel
  (irc-send-command (format #f "JOIN ~a" channel)))

(define (irc-send-message channel message)
  ; Send a message to the specified IRC channel
  (irc-send-command (format #f "PRIVMSG ~a :~a" channel message)))

(define (irc-disconnect)
  ; Disconnect from the IRC server
  (irc-send-command "QUIT"))

(define (irc-send-command command)
  ; Send a raw IRC command to the server
  (display command)
  (newline))

(define (make-irc-connection server port)
  ; Create a new IRC connection to the specified server and port
  (let ((connection (make-tcp-client server port)))
    connection))

(define (irc-login connection nick user password)
  ; Login to the IRC server using the provided credentials
  (irc-send-command (format #f "NICK ~a" nick))
  (irc-send-command (format #f "USER ~a ~a ~a ~a" user user user user))
  (irc-send-command (format #f "PASS ~a" password)))
