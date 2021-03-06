~seq-no-order [![Build Status](https://travis-ci.org/AlexKnauth/seq-no-order.png?branch=master)](https://travis-ci.org/AlexKnauth/seq-no-order)
===

documentation: http://pkg-build.racket-lang.org/doc/seq-no-order/index.html

Provides ~seq-no-order and ~no-order as pattern-expanders that work with
[syntax/parse](http://docs.racket-lang.org/syntax/stxparse.html) for racket version 6.1.1 or higher.

Examples:
```racket
> (require syntax/parse seq-no-order)
> (syntax-parse #'(1 2 3 4 5 6)
    [((~seq-no-order 6 2 y ...))
     #'(y ...)])
#<syntax (1 3 4 5)>
> (syntax-parse #'(1 2 3 4 5 6)
    [(~no-order 6 2 y ...+)
     #'(y ...)])
#<syntax (1 3 4 5)>
> (syntax-parse #'(2 6)
    [(~no-order 6 2 y ...+)
     #'(y ...)]
    [_ #f])
#f
> (define (parse-KE stx)
    (syntax-parse stx
      [(KE (~seq-no-order
            (~seq #:m m)
            (~seq #:v v)))
       #'(* 1/2 m (sqr v))]))
> (parse-KE #'(KE #:m 2 #:v 1))
#<syntax (* 1/2 2 (sqr 1))>
> (parse-KE #'(KE #:v 1 #:m 2))
#<syntax (* 1/2 2 (sqr 1))>
> (define (parse-app stx)
    (syntax-parse stx
      [(#%app f ~no-order (~seq kw:keyword kw-arg:expr) ... arg:expr ...)
       #'(keyword-apply f '(kw ...) (list kw-arg ...) arg ... '())]))
> (parse-app #'(#%app f #:kw1 "kw1" "0" #:kw2 "kw2" "1"))
#<syntax (keyword-apply f '(#:kw1 #:kw2) (list "kw1" "kw2") "0" "1" '())>
```
