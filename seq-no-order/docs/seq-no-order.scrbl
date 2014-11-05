#lang scribble/manual

@(require scribble/eval)

@(require (for-label racket/base
                     syntax/parse
                     seq-no-order))

@title{seq-no-order}

@defmodule[seq-no-order]

source code: @url["https://github.com/AlexKnauth/seq-no-order"]

Provides @racket[~seq-no-order] and @racket[~no-order] as @racket[pattern-expander]s that work with
@racketmodname[syntax/parse] for racket version 6.1.1 or higher.

@defform[(~seq-no-order pat ...)]{
a @racket[pattern-expander] for use with @racketmodname[syntax/parse].  

Like @racket[~seq], except that it matches the @racket[pat]s in any order.  
It also handles ellipses.

@examples[
  (require syntax/parse seq-no-order)
  (syntax-parse #'(1 2 3 4 5 6)
    [((~seq-no-order 6 2 y ...))
     #'(y ...)])
  (define (parse-KE stx)
    (syntax-parse stx
      [(KE (~seq-no-order (~seq #:m m) (~seq #:v v)))
       #'(* 1/2 m (sqr v))]))
  (parse-KE #'(KE #:m 2 #:v 1))
  (parse-KE #'(KE #:v 1 #:m 2))
]}

@defform[(~no-order pat ...)]{
a @racket[pattern-expander] for use with @racketmodname[syntax/parse].  

Like @racket[~seq-no-order], except that it matches a syntax-list.  
Also, @racket[~no-order] can be used as an identifier within a pattern, so that
@racket[(pat0 ... ~no-order pat1 ...)] is equivalent to @racket[(pat0 ... (~seq-no-order pat1 ...))]. 

@examples[
  (require syntax/parse seq-no-order)
  (syntax-parse #'(1 2 3 4 5 6)
    [(~no-order 6 2 y ...+)
     #'(y ...)])
  (define (parse-app stx)
    (syntax-parse stx
      [(#%app f ~no-order (~seq kw:keyword kw-arg:expr) ... arg:expr ...)
       #'(keyword-apply f '(kw ...) (list kw-arg ...) arg ... '())]))
  (syntax->datum (parse-app #'(#%app f #:kw1 "kw1" "0" #:kw2 "kw2" "1")))
  (define (parse-KE stx)
    (syntax-parse stx
      [(KE ~no-order (~seq #:m m) (~seq #:v v))
       #'(* 1/2 m (sqr v))]))
  (parse-KE #'(KE #:m 2 #:v 1))
  (parse-KE #'(KE #:v 1 #:m 2))
]}

