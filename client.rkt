#lang racket
(require racket/class racket/include)
(include "./server.rkt")
(include "./util.rkt")

(define SlackClient%
  (class object%
    #|
    The SlackClient makes API Calls to the `Slack Web API
    <https://api.slack.com/web>`_ as well as managing connections to the
    `Real-time Messaging API via websocket <https://api.slack.com/rtm>`_

    It also manages some of the Client state for Channels that the associated
    token (User or Bot) is associated with.

    For more information, check out the `Slack API Docs
    <https://api.slack.com/>`_

    Init:
        :Args:
            token   (str):  Your Slack Authentication token. You can find or
                                generate a test token `here
                                <https://api.slack.com/docs/oauth-test-tokens>`_
                                Note: Be `careful with your token
                                     <https://api.slack.com/docs/oauth-safety>`_
            proxies (dict): Proxies to use when create websocket or api
                                calls, declare http and websocket proxies using
                                {'http': 'http://127.0.0.1'}, and https proxy
                                using {'https': 'https://127.0.0.1:443'}
    |#
    (super-new)

    (init-field clientToken [clientProxies #f])
    (define token  clientToken)
    (define server (new Server%
                          [serverToken            token]
                          [serverConnect?            #f]
                          [serverProxies  clientProxies]))

    (define/public (append-user-agent name versn)
      (send server append-user-agent name versn))

    (define/public (rtm-connect #:withTeamStat [withTeamStat     #t]
                                #:postData     [**kwargs     (hash)])
      #|
      Connects to the RTM Websocket

      :Args:
          with_team_state (bool): Connect via `rtm.start` to pull
                                      workspace state information.
                                      `False` connects via `rtm.connect`,
                                      which is lighter weight and better
                                      for very large teams.

      :Returns:
          False on exceptions
      |#
      (with-handlers ([exn:fail? (lambda (e) #f)])
        (send server rtm-connect #:useRtmStart withTeamStat #:postData **kwargs)
        #t))

    (define/public (api-call method [timeout #f] #:postData [**kwargs (hash)])
      #|
      Call the Slack Web API as documented here: https://api.slack.com/web

      :Args:
          method     (str):  The API Method to call. See `the full list
                                 here <https://api.slack.com/methods>`_
      :Kwargs:
          (optional) kwargs: any arguments passed here will be bundled and sent
                                 to the api requester as post_data and will be
                                 passed along to the API.

          Example::
              sc.server.api_call("channels.setPurpose",
                                 channel="CABC12345",
                                 purpose="Writing some code!")

      :Returns:
          str -- returns the text of the HTTP response.

          Examples::
              u'{"ok":true,"purpose":"Testing bots"}'
              or
              u'{"ok":false,"error":"channel_not_found"}'

          See here for more information on responses: https://api.slack.com/web
      |#
      (define rBody (send server api-call method timeout #:postData **kwargs))

      (let ([okInBody (hash-ref rBody 'ok #f)])
        (cond
          [(string=? method "im.open")
                (when okInBody
                  (send server attach-channel
                                 (hash-ref **kwargs 'user)
                                 (hash-ref (hash-ref rBody 'channel) 'id)))]
          [(member method '("mpim.open" "groups.create" "groups.createchild"))
                (when okInBody
                  (send server attach-channel
                                 (hash-ref (hash-ref rBody 'group) 'name)
                                 (hash-ref (hash-ref rBody 'group) 'id)
                                 (hash-ref (hash-ref rBody 'group) 'members)))]
          [(member method '("channels.create" "channels.join"))
                (when okInBody
                  (send server attach-channel
                                 (hash-ref (hash-ref rBody 'channel) 'name)
                                 (hash-ref (hash-ref rBody 'channel) 'id)
                                 (hash-ref
                                   (hash-ref rBody 'channel)
                                   'members)))]))

      rBody)

    (define/public (rtm-read)
      #|
      Reads from the RTM Websocket stream then calls
          `self.process_changes(item)` for each line in the returned data.

      Multiple events may be returned, always returns a list
          [], which is empty if there are no incoming messages.

      :Args:
          None

      :Returns:
          data (json) - The server response. For example::
                            [{u'presence': u'active',
                              u'type':     u'presence_change',
                              u'user':     u'UABC1234'}]

      :Raises:
          SlackNotConnected if self.server is not defined.
      |#
      ;; in the future, this should handle some events internally
      ;;     i.e. channel creation
      (map
        (lambda (line)
          (let ([lineHash (string->jsexpr line)])
            (send this process-changes lineHash)
            lineHash))
        (string-split (send server websocket-safe-read) "\n")))

    (define/public (rtm-send-message channel      message
                                     [aThread #f] [replyBroadcast #f])
      #|
      Sends a message to a given channel.

      :Args:
          channel        (str)         - the string identifier for a channel or
                                             channel name (e.g. 'C1234ABC',
                                                                'bot-test', or
                                                                '#bot-test')
          message        (message)     - the string you'd like to
                                             send to the channel
          thread         (str or None) - the parent message ID, if
                                             sending to a thread
          replyBroadcast (boolean)     - if messaging a thread, whether to also
                                             send the message back to the
                                             channel

      :Returns:
          None
      |#
      ;; The `channel` argument can be a channel name or an ID. At first its
      ;; assumed to be a name and an attempt is made to find the ID in the
      ;; workspace state cache. If that lookup fails, the argument is used as
      ;; the channel ID.
      (send server rtm-send-message
                     (let ([foundChannels (find-channel
                                            channel
                                            (send server get-channels))])
                       (if (= (length foundChannels) 1)
                           (send (car foundChannels) get-id)
                         channel))
                     message
                     aThread
                     replyBroadcast))

    (define/public (process-changes data)
      #|
      Internal method which processes RTM events and
          modifies the local data store accordingly.

      Stores new channels when joining a group
          (Multi-party DM), IM (DM) or channel.

      Stores user data on a team join event.
      |#
      (when (member 'type (hash-keys data))
        (let ([type (hash-ref data 'type)])
          (when (member type '("channel_created" "group_joined"))
            (let ([channel (hash-ref data 'channel)])
              (send server attach-channel
                             (hash-ref channel 'name)
                             (hash-ref channel 'id)
                             '())))
          (when (string=? type "im_created")
            (let ([channel (hash-ref data 'channel)])
              (send server attach-channel
                             (hash-ref channel 'user)
                             (hash-ref channel 'id)
                             '())))
          (when (string=? type "team_join")
            (send server parse-user-data (list (hash-ref data 'user)))))))))
