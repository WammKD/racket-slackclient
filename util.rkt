(require syntax/parse/define)

(define-simple-macro (if/return value:expr else:expr)
  (if value value else))
(define-simple-macro (if-let [(binding:id value:expr) ...] then:expr else:expr)
  (let ([binding value] ...)
    (if (and binding ...) then else)))
(define-simple-macro (when-let [(binding:id value:expr) ...] body:expr ...+)
  (let ([binding value] ...)
    (when (and binding ...) body ...)))

(define (find-channel name chnls)
  (foldl
    (lambda (chnl prev)
      (cond
        [(list? chnl)                  (append (find-channel name chnl) prev)]
        [(string=? (send chnl get-name) name)               (cons chnls prev)]
        [else                                                            prev]))
    chnls))
