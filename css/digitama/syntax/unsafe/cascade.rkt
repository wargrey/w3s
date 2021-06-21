#lang typed/racket/base

(provide (all-defined-out))

(require typed/racket/unsafe)

(require "../digicore.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module unsafe racket/base
  (provide (all-defined-out))
  
  (define css-filter?
    (lambda [f]
      (and (procedure? f)
           (procedure-arity-includes? f 1))))
  
  (define css-parser?
    (lambda [f]
      (and (procedure? f)
           (procedure-arity-includes? f 2)))))

(unsafe-require/typed/provide
 (submod "." unsafe)
 [css-filter? (-> Any Boolean : (CSS:Filter Any))]
 [css-parser? (-> Any Boolean : (CSS-Parser (Listof Any)))])
