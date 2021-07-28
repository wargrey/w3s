#lang typed/racket/base

(provide (all-defined-out) XML-Token)

(require "recognizer.rkt")

(require "../digicore.rkt")
(require "../doctype.rkt")

(require "../tokenizer/errno.rkt")
(require "../tokenizer/delimiter.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct rng-declaration ([prefix : Symbol]) #:transparent #:type-name RNG-Declaration)
(struct rng-namespace rng-declaration ([uri : (U String Symbol)] [default? : Boolean]) #:transparent #:type-name RNG-Namespace)
(struct rng-datatype rng-declaration ([uri : String]) #:transparent #:type-name RNG-Datatype)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define <:rnc-declarations:> : (-> (XML-Parser (Listof RNG-Declaration)))
  (let ([xml:uri "http://www.w3.org/XML/1998/namespace"]
        [xsd:uri "http://www.w3.org/2001/XMLSchema-datatypes"]
        [<default> (<rnc-keyword> '#:default)]
        [<namespace> (<rnc-keyword> '#:namespace)]
        [<datatypes> (<rnc-keyword> '#:datatypes)])

    (define (prefix-filter [NS : (Listof RNG-Declaration)] [ns : RNG-Declaration] [tokens : (Listof XML-Token)]) : (XML-Option True)
      (define-values (prefix uri default?)
        (if (rng-namespace? ns)
            (values (rng-declaration-prefix ns) (rng-namespace-uri ns) (rng-namespace-default? ns))
            (values (rng-declaration-prefix ns) (rng-datatype-uri (assert ns rng-datatype?)) #false)))

      (let check-duplicate ([prefixes : (Listof RNG-Declaration) NS])
        (cond [(null? prefixes) #true]
              [else (let-values ([(self rest) (values (car prefixes) (cdr prefixes))])
                      (cond [(and default? (rng-namespace? self) (rng-namespace-default? self))
                             (make-exn:xml:duplicate tokens)]
                            [(and (eq? prefix (rng-declaration-prefix self)) (eq? (object-name ns) (object-name prefix)))
                             (make-exn:xml:duplicate tokens)]
                            [else (check-duplicate rest)]))]))
      
      #;(cond [(eq? prefix 'xml) (if (equal? uri xml:uri) #true (make-exn:rnc:uri tokens))]
              [(eq? prefix 'xsd) (if (equal? uri xsd:uri) #true (make-exn:rnc:uri tokens))]
              [(eq? prefix 'xmlns) (make-exn:rnc:prefix tokens)]
              [(equal? uri xml:uri) (if (eq? prefix 'xml) #true (make-exn:rnc:prefix tokens))]
              [else #true]))

    (define (make-xml->namespace [default? : Boolean]) : (-> (Listof (U String Symbol)) RNG-Declaration)
      (λ [data]
        (cond [(null? data) '#:deadcode (rng-namespace '|| "" default?)]
              [(null? (cdr data)) (rng-namespace '|| (car data) default?)]
              [else (rng-namespace (assert (car data) symbol?) (cadr data) default?)])))

    (define (xml->datatypes [data : (Listof (U String Symbol))]) : RNG-Declaration
      (cond [(or (null? data) (null? (cdr data))) '#:deadcode (rng-datatype '|| "")]
            [else (rng-datatype (assert (car data) symbol?) (assert (cadr data) string?))]))

    (lambda []
      (RNC<*> (RNC<?> [<namespace> (RNC<~> (RNC<&> (RNC:<^> (<rnc-id-or-keyword>)) ((inst <:=:> (Listof (U String Symbol)))) (<:rnc-ns:literal:>))
                                           (make-xml->namespace #false) prefix-filter)]
                      [<datatypes> (RNC<~> (RNC<&> (RNC:<^> (<rnc-id-or-keyword>)) ((inst <:=:> (Listof (U String Symbol)))) (<:rnc-literal:>))
                                           xml->datatypes prefix-filter)]
                      [<default>   (RNC<~> (RNC<&> ((inst RNC:<_> (Listof (U String Symbol))) <namespace>)
                                                   (RNC:<*> (<rnc-id-or-keyword>) '?) ((inst <:=:> (Listof (U String Symbol)))) (<:rnc-ns:literal:>))
                                           (make-xml->namespace #true) prefix-filter)])
              '*))))
