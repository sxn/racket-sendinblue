#lang racket/base

(require json
         net/http-client
         racket/contract
         racket/function)

(provide sendinblue-send-email
         (struct-out sendinblue)
         (struct-out contact)
         (struct-out named-contact)
         (struct-out url-attachment)
         (struct-out base64-attachment))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; structs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct sendinblue (api-key))

(struct contact (email)
  #:transparent)

(struct named-contact contact (name)
  #:transparent)

(struct url-attachment (name url)
  #:transparent)

(struct base64-attachment (name content)
  #:transparent)

(define/contract (sendinblue-send-email
                  client
                  #:sender sender
                  #:to to
                  #:bcc [bcc '()]
                  #:cc [cc '()]
                  #:html-content [html-content #f]
                  #:text-content [text-content #f]
                  #:subject [subject #f]
                  #:reply-to [reply-to #f]
                  #:attachment [attachment '()]
                  #:headers [headers #f]
                  #:template-id [template-id #f]
                  #:params [params #f]
                  #:tags [tags #f]) 
  (->* (sendinblue?
        #:sender (or/c contact? named-contact?)
        #:to (listof (or/c contact? named-contact?)))
       (#:bcc (listof (or/c contact? named-contact?))
        #:cc (listof (or/c contact? named-contact?))
        #:html-content (or/c false/c string?)
        #:text-content (or/c false/c string?)
        #:subject (or/c false/c string?)
        #:reply-to (or/c false/c (or/c contact? named-contact?))
        #:attachment (listof (or/c url-attachment? base64-attachment?))
        #:headers (or/c false/c (hash/c symbol? string?))
        #:template-id (or/c false/c exact-nonnegative-integer?)
        #:params (or/c false/c (hash/c symbol? string?))
        #:tags (or/c false/c (listof string?)))
       jsexpr?)

  (unless (or text-content html-content)
    (raise-user-error 'sendinblue-send-email "You must provide at least one of text-content or html-content."))

  (define mail
    (remove-false-params
     (hasheq 'sender      (contact->hash sender)
             'to          (filter identity (map contact->hash to))
             'bcc         (filter identity (map contact->hash bcc))
             'cc          (filter identity (map contact->hash cc))
             'htmlContent html-content
             'textContent text-content
             'subject     subject
             'replyTo     (contact->hash reply-to)
             'attachment  (filter identity (map attachment->hash attachment))
             'headers     headers
             'templateId  template-id
             'params      params
             'tags        tags)))

  (send-post-request client #:uri "/v3/smtp/email" #:json mail))


(define sendinblue-host
  (make-parameter "api.sendinblue.com"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; private ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define USER-AGENT
  (format "SendInBlue Client for Racket ~a" (version)))

(define (call-with-connection fun)
  (fun (http-conn-open (sendinblue-host)
                       #:port 443
                       #:ssl? #t))) 

(define (send-post-request client #:uri uri #:json json)
  (call-with-connection
   (lambda (conn)
     (define-values (status-line _ in)
       (http-conn-sendrecv! conn
                            uri
                            #:method "POST"
                            #:headers (list "Accept: application/json"
                                            "Content-Type: application/json; charset=utf-8"
                                            (format "User-Agent: ~a" USER-AGENT)
                                            (format "api-key: ~a" (sendinblue-api-key client)))
                            #:data (jsexpr->string json)))

     (define response (read-json in))
     (when (hash-ref response 'code #f)
       (error 'send-post-request (jsexpr->string response)))
     response)))

(define (remove-false-params params)
  (for/fold ([params (hasheq)])
            ([(name value) params]
             #:when (and value (not (null? value))))
    (hash-set params name value)))

(define/contract (contact->hash the-contact)
  (-> (or/c false/c contact? named-contact?) (or/c false/c (hash/c symbol? string?)))
  (cond
    [(named-contact? the-contact) (hasheq 'email (contact-email the-contact)
                                          'name  (named-contact-name the-contact))]
    [(contact? the-contact) (hasheq 'email (contact-email the-contact))]
    [else #f]))

(define/contract (attachment->hash the-attachment)
  (-> (or/c false/c url-attachment? base64-attachment?) (or/c false/c (hash/c symbol? string?)))
  (cond
    [(url-attachment?) (hasheq 'name (url-attachment-name the-attachment)
                               'url (url-attachment-url the-attachment))]
    [(base64-attachment?) (hasheq 'name (base64-attachment-name the-attachment)
                                  'content (base64-attachment-content the-attachment))]
    [else #f]))
                                                          
