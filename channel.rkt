(define Channel%
  (class* object% (equal<%>)
    #|
    A Channel represents a public or private Slack Channel instance
    |#
    (super-new)

    (init channelServer channelName channelID [channelMembers '()])
    (define server  channelServer)
    (define name    channelName)
    (define id      channelID)
    (define members channelMembers)

    (define/public (equal-to? compareObj recur)
      (let ([compareObjName (send compareObj get-name)])
        (or
          (string=? compareObjName                     name)
          (string=? (string-append "#" compareObjName) name)
          (string=? (send compareObj get-id)             id))))
    (define/public (equal-hash-code-of hash-code)
      (hash-code id))
    (define/public (equal-secondary-hash-code-of hash-code)
      (hash-code id))

    (define/public (->string)
      (string-append
        "server  : " (send server ->string) "\n"
        "name    : " name                   "\n"
        "ID      : " id                     "\n"
        "members : " (list->string members) "\n"))

    (define/public (get-id)   id)
    (define/public (get-name) name)

    (define/public (send-message message [mssgThread #f] [replyBroadcast #f])
      #|
      Sends a message to a this Channel.

      Include the parent message's thread_ts
      value in `thread` to send to a thread.

      :Args:
          message        (message)   - the string you'd like to
                                           send to the channel
          thread         (str or #f) - the parent message ID, if
                                           sending to a thread
          replyBroadcast (boolean)   - if messaging a thread, whether to also
                                           send the message back to the channel

      :Returns:
          #t
      |#
      (send server rtm-send-message id message mssgThread replyBroadcast))))
