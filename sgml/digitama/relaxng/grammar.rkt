#lang typed/racket/base

(provide (all-defined-out) XML-Token)

(require "recognizer.rkt")

(require "../digicore.rkt")
(require "../doctype.rkt")

(require "../tokenizer/errno.rkt")
(require "../tokenizer/delimiter.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct rng-namespace
  ([name : Symbol]
   [literal : (U String Symbol)]
   [default? : Boolean])
  #:transparent
  #:type-name RNG-Namespace)

(struct rng-grammar
  ([location : (U String Symbol)]
   [tokens : (Listof XML-Token)])
  #:transparent
  #:type-name RNG-Grammar)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define <:rnc-namespace:> : (-> (XML-Parser (Listof RNG-Namespace)))
  (let ()
    (define (xml->default-namespace [data : (Listof (U String Symbol))]) : RNG-Namespace
      (cond [(null? data) '#:deadcode (rng-namespace '|| "" #true)]
            [(null? (cdr data)) (rng-namespace '|| (car data) #true)]
            [else (rng-namespace (assert (car data) symbol?) (cadr data) #true)]))
    (define (xml->namespace [data : (Listof (U String Symbol))]) : RNG-Namespace
      (cond [(or (null? data) (null? (cdr data))) '#:deadcode (rng-namespace '|| "" #false)]
            [else (rng-namespace (assert (car data) symbol?) (cadr data) #false)]))
    (lambda []
      (RNC<*> (RNC<+> (RNC<~> (RNC<&> ((inst RNC:<_> (Listof (U String Symbol))) (<rnc-keyword> '#:namespace))
                                      (RNC:<^> (<rnc-identify-or-keyword>)) ((inst <:rnc-assign:> (Listof (U String Symbol)))) (RNC:<^> (<rnc-ns:literal>)))
                              xml->namespace)
                      (RNC<~> (RNC<&> ((inst RNC<_> (Listof (U String Symbol))) (RNC:<&> (<rnc-keyword> '#:default) (<rnc-keyword> '#:namespace)))
                                      (RNC:<*> (<rnc-identify-or-keyword>) '?) ((inst <:rnc-assign:> (Listof (U String Symbol)))) (RNC:<^> (<rnc-ns:literal>)))
                              xml->default-namespace))
              '*))))
