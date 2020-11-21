#lang info

(define collection "sendinblue")
(define scribblings '(("sendinblue.scrbl")))

(define deps '("base"))
(define build-deps '("scribble-lib"
                     "sendinblue-lib"
                     "net-doc"
                     "racket-doc"))

(define update-implies '("sendinblue-lib"))
