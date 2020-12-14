#lang racket/base

(require json
         rackunit
         rackunit/text-ui
         sendinblue
         "common.rkt")

(define sendblue-tests
  (test-suite
   "sendinblue-send-email"
   #:before start-server
   #:after stop-server
   (test-case "neither `text-content` nor `html-content` being passed raises a user exception"
     (check-exn
      exn:fail:user?
      (lambda ()
        (sendinblue-send-email (sendinblue "supersecret")
                               #:sender "hello@sorinmuntean.ro"
                               #:subject "hi!"
                               #:to (list "hello@sorinmuntean.ro")))))
   (test-case "filters out falsy parameters before sending the request"
     (check-equal?
      (sendinblue-send-email (sendinblue "supersecret")
                             #:bcc '()
                             #:cc '()
                             #:headers #f
                             #:html-content #f
                             #:params #f
                             #:sender "hello@sorinmuntean.ro"
                             #:subject "hi!"
                             #:template-id #f
                             #:text-content "bar"
                             #:to (list "hello@sorinmuntean.ro"))
      (hasheq 'replyTo (hasheq 'email "hello@sorinmuntean.ro")
              'sender (hasheq 'email "hello@sorinmuntean.ro")
              'subject "hi!"
              'textContent "bar"
              'to (list (hasheq 'email "hello@sorinmuntean.ro")))))
   (test-case "extracts names and emails and formats them correctly before sending the request"
     (check-equal?
      (sendinblue-send-email (sendinblue "supersecret")
                             #:bcc '()
                             #:cc '()
                             #:headers #f
                             #:html-content "<div>bar</div>"
                             #:params #f
                             #:reply-to "hello@sorinmuntean.ro (Noreply)"
                             #:sender "Sorin <hello@sorinmuntean.ro>"
                             #:subject "hi!"
                             #:template-id #f
                             #:text-content "bar"
                             #:to (list "hello@sorinmuntean.ro"))
      (hasheq 'htmlContent "<div>bar</div>"
              'replyTo (hasheq 'email "hello@sorinmuntean.ro"
                               'name "Noreply")
              'sender (hasheq 'email "hello@sorinmuntean.ro"
                              'name "Sorin")
              'subject "hi!"
              'textContent "bar"
              'to (list (hasheq 'email "hello@sorinmuntean.ro")))))))

(module+ test (run-tests sendblue-tests))
