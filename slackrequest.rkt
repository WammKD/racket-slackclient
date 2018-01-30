(require net/http-client json)
(include "./version.rkt")

(define SlackRequest%
  (let ([CLIENT_NAME    MODULE_NAME]
        [CLIENT_VERSION     VERSION])
    (class object%
      (super-new)

      (init [slackRequestProxies #f])
      (define userAgent (list
                          (cons 'client (string-append CLIENT_NAME "/"
                                                       CLIENT_VERSION))
                          (cons 'racket (string-append "Racket/" (version)))
                          (cons 'system (let ([m (string-split
                                                   (system-type 'machine))])
                                          (string-append (car   m) "/"
                                                         (caddr m))))
                          (cons 'custom "")))
      (define proxies   slackRequestProxies)

      (define/public (get-user-agent)
        (foldl (lambda (keyValue previous)
                 (string-append
                   previous
                   (cdr keyValue)
                   (if (eq? (car keyValue) 'custom) "" " "))) "" userAgent))

      (define/public (append-user-agent name versn)
        ;; Concatenate and format the user-agent
        ;; string to be passed into request headers
        (set! userAgent (list-set userAgent 3 (string-append
                                                (cdr (assoc 'custom userAgent))
                                                (string-replace name  "/" ":")
                                                " "
                                                (string-replace versn "/" ":")
                                                " ")))
        userAgent)

      (define/public (call token
                           [request "?"]
                           #:postData [postData (hash)]
                           #:timeout  [timeout  #f]
                           #:domain   [domain   "slack.com"])
        #|
        Perform a POST request to the Slack Web API

        Args:
            token    (str):   your authentication token
            request  (str):   the method to call from the Slack API.
                                  For example: 'channels.list'
            timeout  (float): stop waiting for a response after
                                  a given number of seconds
            postData (hash):  key/value arguments to pass for the request.
                                  For example: {'channel': 'CABC12345'}
            domain   (str):   if for some reason you want to send your
                                  request to something other than slack.com
        |#
        ;; Check for plural fields and convert them
        ;; to comma-separated strings, if needed
        (define finalPD (sequence-fold
                          (lambda (previous key value)
                            (hash-set
                              previous
                              key
                              (if (and (list? value) (member key '(users types
                                                                   channels)))
                                  (string-join value ",")
                                value)))
                          (hash)
                          (in-hash postData)))
        ;; Pull file out so it isn't JSON encoded like normal fields.
        ;; Only do this for requests that are UPLOADING files; downloading files
        (when-let ([files (and
                            (string=? request "files.upload")
                            (hash-ref finalPD 'file          #f))])
          (hash 'file files)
          (hash-remove finalPD 'file))
        ;; Fix this if we can send files

        (define-values (status headers in)
          (http-sendrecv
            domain
            (string-append "/api/" request)
            #:ssl?    #t
            #:method  "POST"
            #:headers (list
                        (string-append
                          "user-agent: "
                          (send this get-user-agent))
                        "Content-Type: application/json"
                        (string-append
                          "Authorization: Bearer "
                          (or token (hash-ref finalPD 'token #f))))
            #:data    (jsexpr->string finalPD)
            ;; Check if you can use a timeout
            ;; Check if you can use proxies
            ))

        (define body (string->jsexpr (port->string in)))
        (close-input-port in)

        (list (cons 'status status) (cons 'headers headers)
              (cons 'body   body))))))
