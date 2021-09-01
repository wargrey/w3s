#lang typed/racket/base

(provide (all-defined-out))

(require "compact.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type RNG-Grammar-Content (U RNG-Element RNG-Foreign-Element RNG-Grammar))
(define-type RNG-Definitions (Immutable-HashTable Symbol RNG-Element))

(struct rng-element
  ([name : (U Keyword Name-Class)]
   [attributes : (Listof (Pairof Symbol String))]
   [children : (Listof RNG-Element)])
  #:transparent
  #:type-name RNG-Element)

(struct rng-foreign-element
  ([name : Symbol]
   [attributes : (Listof (Pairof Symbol String))]
   [children : (Listof (U RNG-Foreign-Element String))])
  #:transparent
  #:type-name RNG-Foreign-Element)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct rng-grammar
  ([tagname : Symbol]
   [location : (U String Symbol)]
   [attributes : (Listof (Pairof Symbol String))]
   [children : (Listof RNG-Grammar-Content)]
   [start : (Option RNG-Element)]
   [siblings : (Listof RNG-Grammar-Content)]
   [definitions : RNG-Definitions])
  #:transparent
  #:type-name RNG-Grammar)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define rng-annotation-element->foreign-element : (-> Annotation-Element RNG-Foreign-Element)
  (lambda [src]
    (rng-foreign-element (annotation-element-name src)
                         (annotation-element-attributes src)
                         (for/list : (Listof (U RNG-Foreign-Element String)) ([child (in-list (annotation-element-content src))])
                           (cond [(annotation-element? child) (rng-annotation-element->foreign-element child)]
                                 [else child])))))
