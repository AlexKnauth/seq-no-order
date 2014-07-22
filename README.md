~seq-no-order
============

Provides ~seq-no-order and ~no-order as pattern-expanders that work with
[syntax-parse-with-pattern-expanders](https://github.com/AlexKnauth/syntax-parse-with-pattern-expanders).

Examples:
```racket
> (require syntax-parse-with-pattern-expanders
           seq-no-order)
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
> (define (parse-app stx)
    (syntax-parse stx
      [(#%app f ~no-order (~seq kw:keyword kw-arg:expr) ... arg:expr ...)
       #'(keyword-apply f '(kw ...) (list kw-arg ...) arg ... '())]))
> (parse-app #'(#%app f #:kw1 "kw1" "0" #:kw2 "kw2" "1"))
#<syntax (keyword-apply f '(#:kw1 #:kw2) (list "kw1" "kw2") "0" "1" '())>
```
