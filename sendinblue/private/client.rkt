#lang racket/base

(require json
         net/http-client)

(provide (struct-out sendinblue)
         send-post-request
         sendinblue-host
         sendinblue-port
         sendinblue-ssl?) 

(define-logger sendinblue)

(define USER-AGENT
  (format "sendinblue Client for Racket ~a" (version)))

(define sendinblue-host
  (make-parameter "api.sendinblue.com"))

(define sendinblue-port
  (make-parameter 443))

(define sendinblue-ssl?
  (make-parameter #t))

(struct sendinblue (api-key))

(define (call-with-connection fun)
  (fun (http-conn-open (sendinblue-host)
                       #:port (sendinblue-port)
                       #:ssl? (sendinblue-ssl?)))) 

(define (send-post-request client #:uri uri #:json json)
  (define data (jsexpr->string json))
  (define headers
    (list "Accept: application/json"
          "Content-Type: application/json; charset=utf-8"
          (format "User-Agent: ~a" USER-AGENT)
          (format "api-key: ~a" (sendinblue-api-key client))))

  (log-sendinblue-info "POST ~a" uri)
  (log-sendinblue-info "Data: ~a" data)

  (call-with-connection
   (lambda (conn)
     (define-values (status-line _ in)
       (http-conn-sendrecv! conn uri #:method "POST" #:headers headers #:data data)) 

     (define response (read-json in))
     (when (hash-ref response 'code #f)
       (error 'send-post-request (jsexpr->string response)))
     response)))
