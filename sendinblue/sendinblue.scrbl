#lang scribble/manual

@(require (for-label json racket sendinblue))

@title{Sendinblue API Client}
@author[(author+email "Sorin Muntean" "hello@sorinmuntean.ro")]

@defmodule[sendinblue]


@;; Introduction ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@section[#:tag "intro"]{Introduction}

This library lets you send emails with @link["https://www.sendinblue.com/"]{Sendinblue}
from Racket. To use it, you'll need to have a valid @link["https://account.sendinblue.com/advanced/api"]{API ke}.

@;; Reference ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
