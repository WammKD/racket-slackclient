(define (SlackClientError #:error [errorType 'SlackClientError]
                          #:msg   [msg       (string-append
                                               "An error occurred in "
                                               "the SlackClient library")] . xs)
  #|
  Base exception for all errors raised by the SlackClient library
  |#
  (apply raise-arguments-error (append (list errorType msg) xs)))

(define (ParseResponseError responseBody originalException)
  #|
  Error raised when responses to Web API methods cannot be parsed as valid JSON
  |#
  (SlackClientError (string-append
                      "Slack API response body could not be parsed: "
                      responseBody
                      ". Original exception: "
                      originalException)))

(define (SlackConnectionError #:message [message #f] #:reply [reply #f])
  (if (not reply)
      (if message
          (SlackClientError #:error 'SlackConnectionError #:msg message)
        (SlackClientError #:error 'SlackConnectionError))
    (if (not message)
        (SlackClientError #:error 'SlackConnectionError "reply" reply)
      (SlackClientError #:error 'SlackConnectionError
                        #:msg   message               "reply" reply))))

(define (SlackLoginError      #:message [message #f] #:reply [reply #f])
  (if (not reply)
      (if message
          (SlackClientError #:error 'SlackLoginError #:msg message)
        (SlackClientError #:error 'SlackLoginError))
    (if (not message)
        (SlackClientError #:error 'SlackLoginError "reply" reply)
      (SlackClientError #:error 'SlackLoginError #:msg message "reply" reply))))
