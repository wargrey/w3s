#lang typed/racket/base

(provide (all-defined-out))

(require "compact.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type RNG-Grammar-Content (U RNG-Element RNG-Grammar))

(struct rng-element
  ([name : (U Keyword Name-Class)] [attributes : (Listof (Pairof Symbol String))] [children : (Listof RNG-Element)])
  #:transparent
  #:type-name RNG-Element)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct rng-grammar
  ([tagname : Symbol]
   [location : (U String Symbol)]
   [attributes : (Listof (Pairof Symbol String))]
   [children : (Listof RNG-Grammar-Content)])
  #:transparent
  #:type-name RNG-Grammar)
