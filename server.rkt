(require net/rfc6455 net/url)
(include "./slackrequest.rkt")
(include "./channel.rkt")
(include "./user.rkt")
(include "./exceptions.rkt")

(define Server%
  (class* object% (equal<%>)
    #|
    The Server object owns the websocket connection
    and all attached channel information.
    |#
    (super-new)

    (init serverToken [serverConnect? #t] [serverProxies #f])
    (define token        serverToken)
    (define username     #f)
    (define domain       #f)
    (define loginData    #f)
    (define websocket    #f)
    (define users        (hash))
    (define channels     '())
    (define connected    #f)
    (define wsURL        #f)
    (define proxies      serverProxies)
    (define apiRequester (new SlackRequest% [slackRequestProxies proxies]))
    (when serverConnect?
      (send this rtm-connect))

    (define/public (equal-to? compareObj recur)
      (let ([d (send compareObj get-domain)] [t (send compareObj get-token)])
        (or (string=? d domain) (string=? t token))))
    (define/public (equal-hash-code-of hash-code)
      (hash-code token))
    (define/public (equal-secondary-hash-code-of hash-code)
      (hash-code token))

    (define/public (->string)
      (string-append
        "username     : " (if/return username  "None") "\n"
        "domain       : " (if/return domain    "None") "\n"
        "websocket    : " (if/return websocket "None") "\n"
        "users        : " (string-append (sequence-fold
                                           (lambda (previous key value)
                                             (string-append previous "("   key
                                                            " . "    value ")"))
                                           "#hash("
                                           (in-hash users)) ")") "\n"
        "loginData    : " (if/return loginData "None") "\n"
        "apiRequester : " (send apiRequester ->string) "\n"
        "channels     : " (list->string channels)      "\n"
        "token        : " token                        "\n"
        "connected    : " (if connected "#t" "#f")     "\n"
        "wsURL        : " (if/return wsURL     "None") "\n"))

    (define/public (get-domain)   domain)
    (define/public (get-token)    token)
    (define/public (get-channels) channels)

    (define/public (append-user-agent name versn)
      (send apiRequester append-user-agent name versn))

    (define/public (rtm-connect #:reconnect   [reconnect       #f]
                                #:timeout     [timeout         #f]
                                #:useRtmStart [useRtmStart     #t]
                                #:postData    [**kwargs    (hash)])
      ;; rtm.start returns user and channel info.; rtm.connect does not.
      (let* ([reply  (send apiRequester call token      (if useRtmStart
                                                            "rtm.start"
                                                          "rtm.connect")
                                             #:timeout  timeout
                                             #:postData **kwargs)]
             [status (cdr (assoc 'status reply))]
             [body   (cdr (assoc 'body   reply))])
        (if (bytes=? (subbytes status (- (bytes-length status) 6)) #"200 OK")
            (if (hash-ref body 'ok #f)
                (begin
                  (set! wsURL     (hash-ref body 'url))
                  (set! websocket (ws-connect (string->url wsURL)))
                  (unless reconnect
                    (set! loginData body)
                    (set! domain    (hash-ref
                                      (hash-ref loginData 'team)
                                      'domain))
                    (set! username  (hash-ref (hash-ref loginData 'self) 'name))

                    (when useRtmStart
                      (send this parse-channel-data
                                   (hash-ref loginData 'channels))
                      (send this parse-channel-data
                                   (hash-ref loginData 'groups))
                      (send this parse-user-data    (hash-ref loginData 'users))
                      (send this parse-channel-data
                                   (hash-ref loginData 'ims)))))
              (SlackLoginError reply))
          (SlackConnectionError reply))))

    (define/public (attach-channel name id [members #f])
      (set! channels (cons (new Channel%
                                  [channelServer     this]
                                  [channelName       name]
                                  [channelID           id]
                                  [channelMembers members]) channels)))

    (define/public (rtm-send-message channel      message
                                     [aThread #f] [replyBroadcast #f])
      #|
      Sends a message to a given channel.

      :Args:
          channel        (str)       - the string identifier for a channel or
                                           channel name (e.g. 'C1234ABC',
                                                              'bot-test', or
                                                              '#bot-test')
          message        (message)   - the string you'd like to send to the
                                           channel
          aThread        (str or #f) - the parent message ID, if sending to a
                                           thread
          replyBroadcast (boolean)   - if messaging a thread, whether to also
                                           send the message back to the channel

      :Returns:
          None
      |#
      (send this send-to-websocket
                   (apply hash (append
                                 (list 'type "message" 'channel channel
                                       'text message)
                                 (if aThread
                                     (append
                                       (list 'thread_ts aThread)
                                       (if replyBroadcast
                                           (list 'reply_broadcast #t)
                                         '()))
                                   '())))))

    (define/public (ping)
      (send this send-to-websocket (hash 'type "ping")))

    (define/public (websocket-safe-read)
      (string-append (ws-recv websocket) "\n"))

    (define/public (join-channel name [timeout #f])
      #|
      Join a channel by name.

      Note: this action is not allowed by bots, they must be invited to channels
      |#
      (send this api-call "channels.join" timeout (hash 'channel name)))

    (define/public (api-call method [timeout #f] #:postData [**kwargs (hash)])
      #|
      Call the Slack Web API as documented here: https://api.slack.com/web

      :Args:
          method     (str):   The API Method to call.
                                  See here for a list:
                                      https://api.slack.com/methods
      :Kwargs:
          (optional) timeout: stop waiting for a response after
                                  a given number of seconds
          (optional) kwargs:  any arguments passed here will be bundled and sent
                                  to the api requester as post_data and will be
                                  passed along to the API.

      Example::
          sc.server.api_call("channels.setPurpose",
                             channel="CABC12345",
                             purpose="Writing some code!")

      Returns:
          str -- returns HTTP response text and headers as JSON.

      Examples::
          u'{"ok":true,"purpose":"Testing bots"}'
          or
          u'{"ok":false,"error":"channel_not_found"}'

      See here for more information on responses: https://api.slack.com/web
      |#
      (let ([reply (send apiRequester call token      method
                                           #:postData **kwargs
                                           #:timeout  timeout)])
        (hash-set
          (cdr (assoc 'body reply))
          'headers
          (foldl
            (lambda (elem previous)
              (let ([keyValue (string-split (bytes->string/utf-8 elem) ":")])
                (hash-set
                  previous
                  (string->symbol (car keyValue))
                  (cadr keyValue))))
            (hash)
            (cdr (assoc 'headers reply))))))

    (define/private (send-to-websocket data)
      #|
      Send a JSON message directly to the websocket.
      See `RTM documentation <https://api.slack.com/rtm` for allowed types.

      :Args:
          data (dict) - the key/values to send the websocket.
      |#
      (with-handlers ([exn:fail? (lambda (e) (send this rtm-connect
                                                          #:reconnect #t))])
        (ws-send! websocket (jsexpr->string data))))

    (define/private (parse-channel-data channelData)
      (for-each
        (lambda (channel)
          (when (null? (find-channel (hash-ref channel 'id) channels))
            (send this attach-channel
                         (let ([cn  (hash-ref channel 'name   #f)])
                           (if (not cn)  (hash-ref channel 'id) cn))
                         (hash-ref channel 'id)
                         (let ([mem (hash-ref channel 'member #f)])
                           (if (not mem) '()                    mem)))))
        channelData))

    (define/private (parse-user-data userData)
      (for-each
        (lambda (user)
          (set! users (hash-set
                        users
                        (hash-ref user 'id)
                        (new User%
                               [userServer   this]
                               [userName     (hash-ref user 'name)]
                               [userID       (hash-ref user 'id)]
                               [userRealName (when (hash-ref user 'real_name #f)
                                               (hash-ref user 'name))]
                               [userTZ       (when (hash-ref user 'tz        #f)
                                               "unknown")]
                               [userEmail    (when (hash-ref
                                                     (hash-ref user 'profile)
                                                     'email
                                                     #f)
                                               "")]))))
        userData))
    (define/private (find-user searchString)
      (if-let ([u (hash-ref users searchString)])
          u
        (sequence-fold
          (lambda (previous uID user)
            (if (string=? (send user get-name) searchString) user previous)
          #f
          (in-hash users)))))))
