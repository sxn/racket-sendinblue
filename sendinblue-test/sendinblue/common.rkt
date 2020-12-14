#lang racket/base

(require json
         sendinblue
         web-server/dispatch
         web-server/http
         web-server/servlet-dispatch
         web-server/web-server)

(provide start-server
         stop-server)


(define (response/json #:code [code 200]
                       #:status [status #"OK"]
                       #:json [json (hasheq)])
  (response/full
   code status
   (current-seconds) #"application/json; charset=utf-8"
   (list) (list (jsexpr->bytes json))))

(define ((guard-auth f) req)
  (define api-key-header
    (headers-assq* #"api-key" (request-headers/raw req)))

  (cond
    [(or (not api-key-header)
         (not (bytes=? (header-value api-key-header) #"supersecret")))
     (response/json
      #:code 401
      #:status #"Unauthorized"
      #:json (hasheq 'message "Key not found"
                     'code "unauthorized"))]

    [else (f req)]))

(define (echo req)
  (response/json
   #:json (bytes->jsexpr (request-post-data/raw req))))

(define-values (start _)
  (dispatch-rules
   [("v3" "smtp" "email") #:method "post" (guard-auth echo)]))

(define server-stopper #f)

(sendinblue-host "127.0.0.1")
(sendinblue-port 9922)
(sendinblue-ssl? #f)

(define (start-server)
  (set! server-stopper (serve #:dispatch (dispatch/servlet start)
                              #:listen-ip (sendinblue-host)
                              #:port (sendinblue-port)))
  (sleep 1))

(define (stop-server)
  (server-stopper))
