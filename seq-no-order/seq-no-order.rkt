#lang racket/base

(provide ~seq-no-order
         ~no-order)

(require syntax/parse
         (for-syntax racket/base
                     syntax/parse))

(module+ test
  (require rackunit
           racket/math))

(begin-for-syntax
  (define-syntax-class pat
    [pattern (~not (~or (~literal ...) (~literal ...+)))])
  (define-splicing-syntax-class sno-pat
    #:attributes (or-pat)
    [pattern {~seq {~and ({~literal ~optional} . _) pat:pat}}
      #:with or-pat #'pat]
    [pattern (~seq pat:pat)
      #:with or-pat #'(~once pat)]
    [pattern (~seq pat:pat (~literal ...))
      #:with or-pat #'pat]
    [pattern (~seq pat:pat (~literal ...+))
      #:with or-pat #'(~between pat 1 +inf.0)]
    ))

(define-syntax ~seq-no-order
  (pattern-expander
   (lambda (stx)
     (syntax-parse stx
       [(seq-no-order pat:sno-pat ...)
        (with-syntax ([ooo '...])
          #'(~seq (~or pat.or-pat ...) ooo))]))))

(define-syntax ~no-order
  (pattern-expander
   (lambda (stx)
     (syntax-parse stx
       [(no-order pat:sno-pat ...)
        (with-syntax ([ooo '...])
          #'((~or pat.or-pat ...) ooo))]))))

(module+ test
  (define (parse-KE stx)
    (syntax-parse stx
      [(KE (~seq-no-order
            (~seq #:m m)
            (~seq #:v v)))
       #'(* 1/2 m (sqr v))]))
  
  (check-equal? (syntax->datum (parse-KE #'(KE #:m 2 #:v 1)))
                (syntax->datum #'(* 1/2 2 (sqr 1))))
  (check-equal? (syntax->datum (parse-KE #'(KE #:v 1 #:m 2)))
                (syntax->datum #'(* 1/2 2 (sqr 1))))
  
  (check-equal? (syntax->datum
                 (syntax-parse #'(1 2 3 4 5 6)
                   [((~seq-no-order 6 2 y ...))
                    #'(y ...)]))
                (syntax->datum
                 #'(1 3 4 5)))
  
  (check-equal? (syntax->datum
                 (syntax-parse #'(1 2 3 4 5 6)
                   [(~no-order 6 2 y ...+)
                    #'(y ...)]))
                (syntax->datum
                 #'(1 3 4 5)))
  
  (check-equal? (syntax-parse #'(2 6)
                  [(~no-order 6 2 y ...+)
                   #'(y ...)]
                  [_ #f])
                #f)

  (check-true (syntax-parse #'()
                [(~no-order {~optional #:a} {~optional #:b}) #true]
                [_                                           #false]))
  
  
  (define (parse-app stx)
    (syntax-parse stx
      [(#%app f ~no-order (~seq kw:keyword kw-arg:expr) ... arg:expr ...)
       #'(keyword-apply f '(kw ...) (list kw-arg ...) arg ... '())]))
  
  (check-equal? (syntax->datum
                 (parse-app #'(#%app f #:kw1 "kw1" "0" #:kw2 "kw2" "1")))
                (syntax->datum
                 #'(keyword-apply f '(#:kw1 #:kw2) (list "kw1" "kw2") "0" "1" '())))
                
  
  )
