(define User%
  (class* object% (equal<%>)
    (super-new)

    (init userServer userName userID userRealName userTZ userEmail)
    (define tz       userTZ)
    (define name     userName)
    (define realName userRealName)
    (define server   userServer)
    (define id       userID)
    (define email    userEmail)

    (define/public (equal-to? compareObj recur)
      (let ([i (send compareObj get-id)] [n (send compareObj get-name)])
        (or (string=? i id) (string=? n name))))
    (define/public (equal-hash-code-of hash-code)
      (hash-code id))
    (define/public (equal-secondary-hash-code-of hash-code)
      (hash-code id))

    (define/public (->string)
      (string-append "tz : "       tz       "\n" "name : " name "\n"
                     "realName : " realName "\n" "id : "   id   "\n"
                     "email : "    email    "\n"))

    (define/public (get-tz)       tz)
    (define/public (get-name)     name)
    (define/public (get-realName) realName)
    (define/public (get-server)   server)
    (define/public (get-id)       id)
    (define/public (get-email)    email)))
