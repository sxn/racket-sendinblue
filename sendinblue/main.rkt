#lang racket/base

(require json
         net/head
         racket/contract
         racket/function
         "./private/client.rkt")

(provide (struct-out sendinblue)
         sendinblue-send-email
         sendinblue-host
         sendinblue-port
         sendinblue-ssl?) 

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
                  #:headers [headers #f]
                  #:template-id [template-id #f]
                  #:params [params #f]
                  #:tags [tags #f]) 
  (->* (sendinblue?
        #:sender string?
        #:to (listof string?)) 
       (#:bcc (listof string?) 
        #:cc (listof string?) 
        #:html-content (or/c false/c string?)
        #:text-content (or/c false/c string?)
        #:subject (or/c false/c string?)
        #:reply-to (or/c false/c string?) 
        #:headers (or/c false/c (hash/c symbol? string?))
        #:template-id (or/c false/c exact-nonnegative-integer?)
        #:params (or/c false/c (hash/c symbol? string?))
        #:tags (or/c false/c (listof string?)))
       jsexpr?)

  (unless (or text-content html-content)
    (raise-user-error 'sendinblue-send-email
                      "You must provide at least one of text-content or html-content."))

  (define mail
    (remove-false-params
     (hasheq 'sender      (contact-string->hash sender)
             'to          (filter identity (map contact-string->hash to))
             'bcc         (filter identity (map contact-string->hash bcc))
             'cc          (filter identity (map contact-string->hash cc))
             'htmlContent html-content
             'textContent text-content
             'subject     subject
             'replyTo     (contact-string->hash (or reply-to sender))
             'headers     headers
             'templateId  template-id
             'params      params
             'tags        tags)))

  (send-post-request client #:uri "/v3/smtp/email" #:json mail))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; private ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (remove-false-params params)
  (for/fold ([params (hasheq)])
            ([(name value) params]
             #:when (and value (not (null? value))))
    (hash-set params name value)))
                                                          
(define (contact-string->hash contact)
  (define email (car (extract-addresses contact 'address)))
  (define name (car (extract-addresses contact 'name)))

  (remove-false-params
   (hasheq 'email email
           'name (if (not (string=? email name))
                     name
                     #f)))) 
