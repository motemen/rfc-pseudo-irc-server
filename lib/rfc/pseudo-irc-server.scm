(define-module rfc.pseudo-irc-server
  (use srfi-1)
  (use srfi-13)
  (use gauche.net)
  (use gauche.selector)
  (use gauche.parameter)
  (export
    <pseudo-irc-server>
    <pseudo-irc-server-client>
    <irc-message>
    <irc-message-prefix>
    irc-server-start
    irc-server-register-callback
    irc-server-register-default-callbacks
    irc-on-command
    irc-send-message-to-client
    irc-server-send-message-to-all-clients
    irc-server-send-message-to-channel
    irc-send-notice-to-client
    irc-send-notice-to-channel
    irc-send-privmsg-to-client
    irc-send-privmsg-to-channel
    irc-send-message-to
    irc-privmsg-to
    irc-notice-to
    irc-prefix-of
    irc-message->string
    irc-message->params-string
    irc-message-prefix->string
    make-irc-message
    make-irc-message-prefix
    current-irc-server
    ))
(select-module rfc.pseudo-irc-server)

;;; 疑似IRCサーバ
(define-class <pseudo-irc-server> ()
  ((listen-port :init-keyword :listen-port
                :init-value 6667)
   (clients     :init-value '())
   (channels    :init-value '())

   (callbacks   :init-thunk make-hash-table)

   (server-socket)
   (selector)

   (name    :init-value "pseudo-irc-server/gauche" :init-keyword :name)
   (version :init-value 0.01)))

(define current-irc-server (make-parameter #f))

(define-method initialize ((self <pseudo-irc-server>) initargs)
  (next-method)
  (current-irc-server self))

;; <pseudo-irc-server> に接続しているクライアント
(define-class <pseudo-irc-server-client> ()
  ((socket   :init-keyword :socket)
   (nick     :init-keyword :nick     :init-value #f)
   (user     :init-keyword :user     :init-value #f)
   (password :init-keyword :password :init-value #f)
   (channels                         :init-value '())))

;; IRCメッセージ
(define-class <irc-message> ()
  ((prefix  :init-keyword :prefix)   ; コマンドのプレフィックス <irc-message-prefix> または #f
   (command :init-keyword :command)  ; コマンド名
   (params  :init-keyword :params))) ; パラメータのリスト

(define-method initialize ((obj <irc-message>) initargs)
  (next-method)
  (unless (is-a? (slot-ref obj 'prefix) <irc-message-prefix>)
    (slot-set! obj 'prefix (parse-irc-message-prefix (slot-ref obj 'prefix)))))

;; プレフィックス
(define-class <irc-message-prefix> ()
  ((nick :init-keyword :nick)
   (user :init-keyword :user)
   (host :init-keyword :host)))

;; 疑似IRCサーバを開始
(define-method irc-server-start ((self <pseudo-irc-server>))
  (let ((selector      (make <selector>))
        (server-socket (make-server-socket (slot-ref self 'listen-port) :reuse-addr? #t)))
    (slot-set! self 'selector      selector)
    (slot-set! self 'server-socket server-socket)
    (selector-add!
      selector
      (socket-fd server-socket)
      (pa$ pseudo-irc-server-accept-handler self) '(r))

    (log-info #`"Pseudo IRC server is running port ,(slot-ref self 'listen-port)...")

    (do () (#f)
      (selector-select selector '(5 0)))))

(define-method irc-server-start ()
  (irc-server-start (current-irc-server)))

;; クライアントの接続
(define-method pseudo-irc-server-accept-handler ((self <pseudo-irc-server>) sock flag)
  (let* ((client-socket (socket-accept (slot-ref self 'server-socket)))
         (client        (make <pseudo-irc-server-client> :socket client-socket)))
    (log-info #`"Client ,client has connected.")

    (slot-push! self 'clients client)
    (selector-add!
      (slot-ref self 'selector)
      (socket-input-port client-socket :buffering :none)
      (pa$ pseudo-irc-server-client-input-handler self client)
      '(r))))

;; クライアントからの入力
(define-method pseudo-irc-server-client-input-handler ((self <pseudo-irc-server>) (client <pseudo-irc-server-client>) (port <port>) flag)
  (or
    (and-let*
        (( (not (port-closed? port)) )
         (line (guard (e (else #f)) (read-line port)))
         ( (not (eof-object? line)) )
         (irc-message (parse-irc-message line)))
      (log-info #`",client ,line")
      (pseudo-irc-server-handle-callback self client irc-message))
    (begin
      (log-info #`",client has disconnected.")
      (slot-delete! self 'clients client)
      (selector-delete! (slot-ref self 'selector) port #f #f)
      (socket-close (slot-ref client 'socket)))))

;; IRCコマンドに対応するコールバック関数を呼ぶ
(define-method pseudo-irc-server-handle-callback ((self <pseudo-irc-server>) (client <pseudo-irc-server-client>) (message <irc-message>))
  (for-each
    (cut <> self client message)
    (append (reverse (hash-table-get (slot-ref self 'callbacks) (slot-ref message 'command) '()))
            (reverse (hash-table-get (slot-ref self 'callbacks) '*                          '())))))

;; コールバックを登録
(define-method irc-server-register-callback ((self <pseudo-irc-server>) command callback)
  (hash-table-push!
    (slot-ref self 'callbacks)
    (string->symbol (string-upcase (x->string command)))
    callback))

;; マクロ版
(define-syntax irc-on-command
  (syntax-rules (current-irc-server)
    ((_ command (?param ...) ?body ...)
     (irc-server-register-callback (current-irc-server) 'command
       (lambda (_ client message)
         (apply* (lambda (?param ...) ?body ...) client (ref message 'params)))))
    ((_ command () ?body ...)
     (irc-server-register-callback (current-irc-server) 'command
       (lambda (_ client message)
         ?body ...)))
    ((_ command ?params ?body ...)
     (irc-server-register-callback (current-irc-server) 'command
       (lambda (_ client message)
         (apply (lambda ?params ?body ...) client (ref message 'params)))))
    ))

(define (apply* proc . args)
  (let ((ar (arity proc))
        (args (apply cons* args)))
    (if (arity-at-least? ar)
      (apply proc args)
      (apply proc (take (append args (circular-list #f)) ar)))))

;; クライアントにメッセージを送信
(define-method irc-send-message-to-client ((client <pseudo-irc-server-client>) (message <irc-message>))
  (display #`",(irc-message->string message)\r\n" (socket-output-port (slot-ref client 'socket))))

(define-method irc-send-message-to-client ((client <pseudo-irc-server-client>) prefix command . params)
  (irc-send-message-to-client client (make <irc-message> :prefix prefix :command command :params params)))

;; 接続しているクライアント全員にメッセージを送信
(define-method irc-server-send-message-to-all-clients ((server <pseudo-irc-server>) (message <irc-message>))
  (for-each
    (cut irc-send-message-to-client <> message)
    (slot-ref server 'clients)))

(define-method irc-server-send-message-to-all-clients ((server <pseudo-irc-server>) prefix command . params)
  (irc-server-send-message-to-all-clients server (make <irc-message> :prefix prefix :command command :params params)))

;; 特定のチャンネルにいるクライアント全員にメッセージを送信
(define-method irc-server-send-message-to-channel ((server <pseudo-irc-server>) channel (message <irc-message>))
  (for-each
    (cut irc-send-message-to-client <> message)
    (filter (lambda (client)
              (and (member channel (slot-ref client 'channels))
                   (not (string= (slot-ref (slot-ref message 'prefix) 'nick) (slot-ref client 'nick)))))
            (slot-ref server 'clients))))

(define-method irc-server-send-message-to-channel ((server <pseudo-irc-server>) channel prefix command . params)
  (irc-server-send-message-to-channel server channel (make <irc-message> :prefix prefix :command command :params params)))

(define-method irc-send-privmsg-to-client (sender (client <pseudo-irc-server-client>) msg)
  (irc-send-message-to-client client (irc-prefix-of sender) 'PRIVMSG msg))

(define-method irc-send-notice-to-client (sender (client <pseudo-irc-server-client>) msg)
  (irc-send-message-to-client client (irc-prefix-of sender) 'NOTICE msg))

(define-method irc-send-privmsg-to-channel ((server <pseudo-irc-server>) channel msg)
  (irc-send-privmsg-to-channel server server channel msg))

(define-method irc-send-privmsg-to-channel ((server <pseudo-irc-server>) sender channel msg)
  (irc-server-send-message-to-channel server channel (irc-prefix-of sender) 'PRIVMSG channel msg))

(define-method irc-send-notice-to-channel ((server <pseudo-irc-server>) channel msg)
  (irc-send-notice-to-channel server server channel msg))

(define-method irc-send-notice-to-channel ((server <pseudo-irc-server>) sender channel msg)
  (irc-server-send-message-to-channel server channel (irc-prefix-of sender) 'NOTICE channel msg))

;;
(define-method irc-send-message-to ((client <pseudo-irc-server-client>) (message <irc-message>))
  (irc-send-message-to-client client message))

(define-method irc-send-message-to ((channel <string>) (message <irc-message>))
  (irc-server-send-message-to-channel (current-irc-server) channel message))

(define-method irc-send-message-to ((channel <symbol>) (message <irc-message>))
  (irc-server-send-message-to-all-clients (current-irc-server) message))

(define-method irc-send-message-to (to sender (command <symbol>) . params)
  (irc-send-message-to
    to
    (apply
      make-irc-message
      (irc-prefix-of (or sender (current-irc-server)))
      command
      params)))

(define-method irc-privmsg-to ((channel <string>) (sender <irc-message-prefix>) (privmsg <string>))
  (irc-server-send-message-to-channel
    (current-irc-server)
    channel
    (make-irc-message
      sender
      'PRIVMSG
      channel
      privmsg)))

(define-method irc-privmsg-to ((client <pseudo-irc-server-client>) (sender <irc-message-prefix>) (privmsg <string>))
  (irc-send-message-to-client
    client
    (make-irc-message
      sender
      'PRIVMSG
      channel
      privmsg)))

(define-method irc-privmsg-to (channel-or-client sender (privmsg <string>))
  (irc-privmsg-to channel-or-client (irc-prefix-of sender) privmsg))

(define-method irc-notice-to ((client <pseudo-irc-server-client>) (sender <irc-message-prefix>) (notice <string>))
  (irc-send-message-to-client
    client
    (make-irc-message
      sender
      'NOTICE
      channel
      notice)))

(define-method irc-notice-to ((channel <string>) (sender <irc-message-prefix>) (notice <string>))
  (irc-server-send-message-to-channel
    (current-irc-server)
    channel
    (make-irc-message
      sender
      'NOTICE
      channel
      notice)))

(define-method irc-notice-to (channel-or-client sender (notice <string>))
  (irc-notice-to channel-or-client (irc-prefix-of sender) notice))

;;
(define-method write-object ((client <pseudo-irc-server-client>) port)
  (display #`"<pseudo-irc-server-client ,(irc-message-prefix->string (irc-prefix-of client))>" port))

;;; デフォルトのハンドラ
(define-method irc-server-register-default-callbacks ((server <pseudo-irc-server>))
  (irc-server-register-callback server 'PASS set-client-password)
  (irc-server-register-callback server 'NICK set-client-nick)
  (irc-server-register-callback server 'USER send-welcome-message)
  (irc-server-register-callback server 'USER set-client-user)
  (irc-server-register-callback server 'JOIN join-channel)
  (irc-server-register-callback server 'PART part-channel)
  (irc-server-register-callback server 'QUIT quit-server))

(define-method irc-server-register-default-callbacks ()
  (irc-server-register-default-callbacks (current-irc-server)))

;; password スロットを更新
(define-method set-client-password ((server <pseudo-irc-server>) (client <pseudo-irc-server-client>) (message <irc-message>))
  (slot-set! client 'password (first (slot-ref message 'params))))

;; nick スロットを更新
(define-method set-client-nick ((server <pseudo-irc-server>) (client <pseudo-irc-server-client>) (message <irc-message>))
  (slot-set! client 'nick (first (slot-ref message 'params))))

;; user スロットを更新
(define-method set-client-user ((server <pseudo-irc-server>) (client <pseudo-irc-server-client>) (message <irc-message>))
  (slot-set! client 'user (first (slot-ref message 'params))))

;; ログイン完了メッセージを送信
(define-method send-welcome-message ((server <pseudo-irc-server>) (client <pseudo-irc-server-client>) (message <irc-message>))
  (for-each
    (cut irc-send-message-to-client client (slot-ref server 'name) <> (slot-ref client 'nick) <>)
    '(001 002)
    `(,#`"Welcome to the Internet Relay Network ,(irc-message-prefix->string (irc-prefix-of client))"
      ,#`"Your host is ,(slot-ref server 'name), running version ,(slot-ref server 'version)")))

;; チャンネルにJOIN
(define-method join-channel ((server <pseudo-irc-server>) (client <pseudo-irc-server-client>) (message <irc-message>))
  (for-each
    (lambda (channel)
      (irc-send-message-to-client client (irc-prefix-of client) 'JOIN channel)
      (unless (member channel (slot-ref server 'channels))
        (slot-push! server 'channels channel))
      (unless (member channel (slot-ref client 'channels))
        (slot-push! client 'channels channel)))
    (string-split (first (slot-ref message 'params)) ",")))

;; チャンネルからPART
(define-method part-channel ((server <pseudo-irc-server>) (client <pseudo-irc-server-client>) (message <irc-message>))
  (let1 channel (first (slot-ref message 'params))
    (irc-send-message-to-client client (irc-prefix-of client) 'PART channel)
    (slot-delete! server 'channels channel)
    (slot-delete! client 'channels channel)))

;; QUIT
(define-method quit-server ((server <pseudo-irc-server>) (client <pseudo-irc-server-client>) (message <irc-message>))
  (slot-delete! server 'clients client)
  (selector-delete! (slot-ref server 'selector) (socket-input-port (slot-ref client 'socket)) #f #f)
  (socket-close (slot-ref client 'socket)))

;; EVAL
(define-method eval-message ((server <pseudo-irc-server>) (client <pseudo-irc-server-client>) (message <irc-message>))
  (let1 result (guard (e (else e))
                 (eval (call-with-input-string (irc-message->params-string message) read) (current-module)))
    (irc-send-notice-to-client server client result)))

;; prefix
(define-method irc-prefix-of ((server <pseudo-irc-server>))
  (let1 addr (sockaddr-addr (socket-address (slot-ref server 'server-socket)))
    (make <irc-message-prefix>
          :nick (slot-ref server 'name)
          :user "localhost"
          :host (inet-address->string addr AF_INET))))

(define-method irc-prefix-of ((client <pseudo-irc-server-client>))
  (let1 addr (sockaddr-addr (socket-address (slot-ref client 'socket)))
    (make <irc-message-prefix>
          :nick (slot-ref client 'nick)
          :user (slot-ref client 'user)
          :host (inet-address->string addr AF_INET))))

(define-method irc-prefix-of ((nick <string>))
  (or (parse-irc-message-prefix nick)
      (parse-irc-message-prefix #`",|nick|!pseudo@localhost")))

(define-method irc-prefix-of ((message <irc-message>))
  (slot-ref message 'prefix))

(define-method irc-prefix-of (_)
  (irc-prefix-of (current-irc-server)))

;;; IRCコマンドの解析
(define (parse-irc-message raw-line)
  (rxmatch-let (#/^(:(\S+?) )?([a-zA-Z]+|\d\d\d)(.*)/ raw-line)
      (#f #f prefix command params)
    (make <irc-message>
          :prefix  (parse-irc-message-prefix prefix)
          :command (string->symbol (string-upcase command))
          :params  (split-irc-message-params params))))

(define (parse-irc-message-prefix prefix)
  (rxmatch-if (and prefix (#/^(.*?)!(.*?)@(.*)$/ prefix))
      (#f nick user host)
    (make <irc-message-prefix>
          :nick nick
          :user user
          :host host)
    #f))

(define (make-irc-message prefix command . params)
  (make <irc-message>
        :prefix  prefix
        :command command
        :params  params))

(define (make-irc-message-prefix nick user host)
  (make <irc-message-prefix>
        :nick nick
        :user user
        :host host))

(define-method object-apply ((server <pseudo-irc-server>) command . params)
  (apply make-irc-message (irc-prefix-of server) command params))

(define-method object-apply ((client <pseudo-irc-server-client>) command . params)
  (apply make-irc-message (irc-prefix-of client) command params))

(define-method irc-message-prefix->string ((prefix <irc-message-prefix>))
  #`",(slot-ref prefix 'nick)!,(slot-ref prefix 'user)@,(slot-ref prefix 'host)")

(define-method write-object ((prefix <irc-message-prefix>) port)
  (display #`"<irc-message-prefix ,(irc-message-prefix->string prefix)>" port))

(define (split-irc-message-params raw-params)
  (rxmatch-let (#/^(.*?)( :(.*))?$/ raw-params)
      (#f params #f trail)
    (append
      (remove string-null? (string-split params " "))
      (if trail (list trail) '()))))

(define-method irc-message->string ((message <irc-message>))
  (let ((prefix  (slot-ref message 'prefix))
        (command (slot-ref message 'command))
        (params  (slot-ref message 'params)))
    (let1 line (string-append
                 (string-upcase (format #f "~3,,,'0@a" command))
                 (if (or (not params) (null? params))
                   ""
                   (string-join `(,@(map x->string (drop-right params 1)) ,#`":,(last params)") " " 'prefix)))
      (if prefix #`":,(irc-message-prefix->string prefix) ,line" line))))

(define-method irc-message->params-string ((message <irc-message>))
  (string-join (ref message 'params)))

;;; ユーティリティ
(define (slot-delete! obj slot x)
  (slot-set! obj slot (delete x (slot-ref obj slot))))

(define log-info print)

(provide "rfc/pseudo-irc-server")
