#lang racket/base

(provide ~find)

(require syntax/parse
         (for-syntax racket/base
                     syntax/parse))
(module+ test
  (require rackunit))

(begin-for-syntax
  (define-splicing-syntax-class find-opts
    [pattern (~seq) #:with [once-opt ...] #'[]]
    [pattern (~seq #:too-few msg) #:with [once-opt ...] #'[#:too-few msg]]))

(define-syntax ~find
  (pattern-expander
   (syntax-parser
     [(_ pat opts:find-opts)
      #:with ooo (quote-syntax ...)
      #'[(~or (~once pat opts.once-opt ...) _) ooo]])))

;; ----------------------------------------------------------------------------

(module+ test
  (define (pred1 a b)
    (and (<= 4 (syntax-e a))
         (<= (syntax-e b) 8)))
  (define (pred2 a b)
    (and (<= 5 (syntax-e a))
         (<= (syntax-e b) 8)))
  (define (pred3 a b)
    (and (<= 5 (syntax-e a))
         (<= (syntax-e b) 7)))

  ;; should first the first one that matches: the g expression.
  (check-equal? (syntax-parse #'(m 0 (f 1 2 3) (g 4 5 6) (h 7 8 9))
                  [(_ thing candidate ...)
                   #:with
                   (~find
                    (~and (some-pattern a b c)
                          (~fail #:when (and (not (pred1 #'a #'b)) #'a)
                                 "some message"))
                    #:too-few "no candidate matched")
                   #'[candidate ...]
                   (syntax->datum #'c)])
                6)

  ;; should find the first one that matches: the h expression.
  (check-equal? (syntax-parse #'(m 0 (f 1 2 3) (g 4 5 6) (h 7 8 9))
                  [(_ thing candidate ...)
                   #:with
                   (~find
                    (~and (some-pattern a b c)
                          (~fail #:when (and (not (pred2 #'a #'b)) #'a)
                                 "some message"))
                    #:too-few "no candidate matched")
                   #'[candidate ...]
                   (syntax->datum #'c)])
                9)

  ;; none match
  (check-exn #rx"no candidate matched"
             (λ ()
               (syntax-parse #'(m 0 (f 1 2 3) (g 4 5 6) (h 7 8 9))
                 [(_ thing candidate ...)
                  #:with
                  (~find
                   (~and (some-pattern a b c)
                         (~fail #:when (and (not (pred3 #'a #'b)) #'a)
                                "some message"))
                   #:too-few "no candidate matched")
                  #'[candidate ...]
                  (syntax->datum #'c)])))
  )
