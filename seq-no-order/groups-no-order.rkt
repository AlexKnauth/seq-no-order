#lang racket/base

(provide ~groups-no-order)

;; from my answer to this stack overflow question:
;; https://stackoverflow.com/questions/34551203

(require syntax/parse
         "seq-no-order.rkt"
         (for-syntax racket/base
                     racket/syntax
                     syntax/stx))
(module+ test
  (require rackunit))

(define-syntax ~groups-no-order
  (pattern-expander
   (lambda (stx)
     (syntax-case stx ()
       [(groups [group-name member-pat ...] ...)
        (with-syntax ([ooo (quote-syntax ...)])
          (define/with-syntax [[member-tmp ...] ...]
            (stx-map generate-temporaries #'[[member-pat ...] ...]))
          (define/with-syntax [group-tmp ...]
            (generate-temporaries #'[group-name ...]))
          #'(~and (~seq-no-order (~and (~seq (~var member-tmp) ooo)
                                       member-pat)
                                 ... ...)
                  (~parse [[(~var group-tmp) ooo] ooo] #'[[member-tmp ooo] ...])
                  ...
                  (~parse [group-name ooo] #'[group-tmp ooo ooo])
                  ...))]))))

(module+ test
  (define-splicing-syntax-class opts
    #:attributes ([first-opts 1] [second-opts 1])
    [pattern (~groups-no-order
              [first-opts
               (~optional (~seq #:a a))
               (~optional (~seq #:b b))]
              [second-opts
               (~optional (~seq #:x x))
               (~optional (~seq #:y y))])])
  
  (syntax-parse #'(foobar #:b 3 #:y 7 #:a 2)
    [(foobar opts:opts)
     (check-equal? (syntax->datum
                    #'(opts.first-opts ...))
                   '(#:a 2 #:b 3))
     (check-equal? (syntax->datum
                    #'(opts.second-opts ...))
                   '(#:y 7))])
  )
