SYNOPSIS
========
    (use rfc.pseudo-irc-server)

    (make <pseudo-irc-server>) ; server is stored in parameter (current-irc-server) automatically

    ; register callbacks needed for the server to work properly.
    (irc-server-register-default-callbacks)

    ; make any users join a channel on login.
    (irc-on-command USER (client)
      (irc-send-message-to client (client 'JOIN "#default-room")))

    ; show welcome message on channel join.
    (irc-on-command JOIN (client channel)
      (irc-send-message-to channel ((current-irc-server) 'NOTICE channel #`"welcome to ,channel, ,(ref client 'nick)")))

    ; start IRC server.
    (irc-server-start)

EXPORTED FUNCTIONS
==================
`<pseudo-irc-server>`
---------------------
 * listen-port
 * name
 * version

`<pseudo-irc-server-client>`
----------------------------
 * nick
 * user
 * password

`<irc-message>`
---------------
 * prefix
 * command
 * params

`<irc-message-prefix>`
---------------------
 * nick
 * user
 * host

`current-irc-server`
--------------------
Parameter that specifies last `make`-ed `<pseudo-irc-server>`. Most of methods below, `(current-irc-server)` is used if server is not specified.

`(irc-server-start server)`
---------------------------
`(irc-server-start)`
--------------------
Start pseudo-IRC-server.

`(irc-send-message-to client message)`
--------------------------------------
Send `message` to `client`.

`(irc-send-message-to channel message)`
---------------------------------------
Send `message` to all clients in `channel`.

`(irc-send-message-to 'all message)`
------------------------------------
Send `message` to all clients.

`(irc-send-message-to client-or-channel-or-all sender command params ...)`
---------------------
Send message composed of `sender`, `command` and `params` to `client-or-channel-or-all`. If `sender` is `#f`, `(current-irc-server)` is used.

`(irc-on-command command (param ...) body ...)`
------------------------------------------------
`(irc-server-register-callback server command callback`
-------------------------------------------------------
Register callbacks.

`irc-server-register-default-callbacks`
---------------------------------------
Register callbacks for PASS, NICK, USER, JOIN, PART, QUIT, EVAL commands.

`irc-send-message-to-client`
----------------------------
`irc-server-send-message-to-all-clients`
----------------------------------------
`irc-server-send-message-to-channel`
------------------------------------
`irc-send-notice-to-client`
---------------------------
`irc-send-notice-to-channel`
----------------------------
`irc-send-privmsg-to-client`
----------------------------
`irc-send-privmsg-to-channel`
-----------------------------
`irc-prefix-of`
---------------
`irc-message->string`
---------------------
`irc-message->params-string`
----------------------------
`irc-message-prefix->string`
----------------------------
`make-irc-message`
------------------
`make-irc-message-prefix`
-------------------------
