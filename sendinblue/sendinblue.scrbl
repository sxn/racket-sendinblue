#lang scribble/manual

@(require (for-label json racket postmark))

@title{Sendinblue API Client}
@author[(author+email "Sorin Muntean" "hello@sorinmuntean.ro")]

@defmodule[sendinblue]


@;; Introduction ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@section[#:tag "intro"]{Introduction}

This library lets you send emails with @link["https://www.sendinblue.com/"]{Sendinblue}
from Racket.
