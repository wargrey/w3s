#lang typed/racket/base

(provide (all-defined-out) XML-Token)

(require "recognizer.rkt")

(require "../digicore.rkt")
(require "../namespace.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct rng-env ([prefix : Symbol]) #:transparent #:type-name RNG-ENV)
(struct rng-namespace rng-env ([uri : (U String Symbol)] [default? : Boolean]) #:transparent #:type-name RNG-Namespace)
(struct rng-datatype rng-env ([uri : String]) #:transparent #:type-name RNG-Datatype)

(struct rng-pattern () #:transparent #:type-name RNG-Pattern)
(struct rng-name-class () #:transparent #:type-name RNG-Name-Class)
(struct rng-grammar-content () #:transparent #:type-name RNG-Grammar-Content)
(struct rng-parameter ([name : Symbol] [value : String]) #:transparent #:type-name RNG-Parameter)

(struct rng:simple rng-pattern ([element : Keyword]) #:transparent #:type-name RNG:Simple)
(struct rng:ref rng-pattern ([element : Symbol]) #:transparent #:type-name RNG:Ref)
(struct rng:parent rng:ref () #:transparent #:type-name RNG:Parent)
(struct rng:grammar rng-pattern ([contents : (Listof RNG-Grammar-Content)]) #:transparent #:type-name RNG:Grammar)
(struct rng:element rng-pattern ([name : (U Keyword RNG-Name-Class)] [child : RNG-Pattern]) #:transparent #:type-name RNG:Element)
(struct rng:attribute rng-pattern ([name : RNG-Name-Class] [child : RNG-Pattern]) #:transparent #:type-name RNG:Attribute)
(struct rng:value rng-pattern ([ns : (Option Symbol)] [name : (U Symbol Keyword False)] [literal : String]) #:transparent #:type-name RNG:Value)
(struct rng:data rng-pattern ([ns : (Option Symbol)] [name : (U Symbol Keyword)] [params : (Listof RNG-Parameter)] [except : (Option RNG-Pattern)])
  #:transparent #:type-name RNG:Data)

(struct rng-name rng-name-class ([id : Symbol]) #:transparent #:type-name RNG-Name)
(struct rng-any-name rng-name-class ([ns : (Option Symbol)] [except : (Option RNG-Name-Class)]) #:transparent #:type-name RNG-Any-Name)
(struct rng-alt-name rng-name-class ([options : (Listof RNG-Name-Class)]) #:transparent #:type-name RNG-Alt-Name)

(struct rng-start rng-grammar-content ([combine : (Option Char)] [pattern : RNG-Pattern]) #:transparent #:type-name RNG-Start)
(struct rng-define rng-grammar-content ([name : Symbol] [combine : (Option Char)] [pattern : RNG-Pattern]) #:transparent #:type-name RNG-Define)
(struct rng-div rng-grammar-content ([contents : (Listof RNG-Grammar-Content)]) #:transparent #:type-name RNG-Div)
(struct rng-include rng-grammar-content ([href : String] [inherit : (Option Symbol)] [contents : (Listof RNG-Grammar-Content)]) #:transparent #:type-name RNG-Include)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define rnc-grammar-parse : (All (a) (-> (XML-Parser (Listof a)) (Listof XML-Token) (Values (Listof a) (Listof XML-Token))))
  (lambda [parse tokens]
    (define-values (grammar rest) (parse null tokens))

    (cond [(not grammar) (values null rest)]
          [(exn:xml? grammar) (values null rest)]
          [else (values (reverse grammar) rest)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define <:rnc-decl*:> : (-> (XML-Parser (Listof RNG-ENV)))
  (lambda []
    (RNC<*> (<:rnc-declaration:>) '*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define <:rnc-declaration:> : (-> (XML-Parser (Listof RNG-ENV)))
  (let (;[xml:uri "http://www.w3.org/XML/1998/namespace"]
        ;[xsd:uri "http://www.w3.org/2001/XMLSchema-datatypes"]
        [<default> (<rnc-keyword> '#:default)]
        [<namespace> (<rnc-keyword> '#:namespace)]
        [<datatypes> (<rnc-keyword> '#:datatypes)])

    (define (prefix-filter [NS : (Listof RNG-ENV)] [ns : RNG-ENV] [tokens : (Listof XML-Token)]) : (XML-Option True)
      (define-values (prefix uri default?)
        (if (rng-namespace? ns)
            (values (rng-env-prefix ns) (rng-namespace-uri ns) (rng-namespace-default? ns))
            (values (rng-env-prefix ns) (rng-datatype-uri (assert ns rng-datatype?)) #false)))

      (let check-duplicate ([prefixes : (Listof RNG-ENV) NS])
        (cond [(null? prefixes) #true]
              [else (let-values ([(self rest) (values (car prefixes) (cdr prefixes))])
                      (cond [(and default? (rng-namespace? self) (rng-namespace-default? self))
                             (make+exn:xml:duplicate tokens)]
                            [(and (eq? prefix (rng-env-prefix self)) (eq? (object-name ns) (object-name self)))
                             (make+exn:xml:duplicate tokens)]
                            [else (check-duplicate rest)]))]))
      
      #;(cond [(eq? prefix 'xml) (if (equal? uri xml:uri) #true (make+exn:rnc:uri tokens))]
              [(eq? prefix 'xsd) (if (equal? uri xsd:uri) #true (make+exn:rnc:uri tokens))]
              [(eq? prefix 'xmlns) (make+exn:rnc:prefix tokens)]
              [(equal? uri xml:uri) (if (eq? prefix 'xml) #true (make+exn:rnc:prefix tokens))]
              [else #true]))

    (define (make-xml->namespace [default? : Boolean]) : (-> (Listof (U String Symbol)) RNG-ENV)
      (λ [data]
        (cond [(null? data) '#:deadcode (rng-namespace '|| "" default?)]
              [(null? (cdr data)) (rng-namespace '|| (car data) default?)]
              [else (rng-namespace (assert (car data) symbol?) (cadr data) default?)])))

    (define (xml->datatypes [data : (Listof (U String Symbol))]) : RNG-ENV
      (cond [(or (null? data) (null? (cdr data))) (rng-datatype '|| "deadcode")]
            [else (rng-datatype (assert (car data) symbol?) (assert (cadr data) string?))]))

    (lambda []
      (RNC<?> [<namespace> (RNC<~> (RNC<&> (RNC:<^> (<rnc-id-or-keyword>)) ((inst <:=:> (Listof (U String Symbol)))) (<:rnc-ns:literal:>))
                                   (make-xml->namespace #false) prefix-filter)]
              [<datatypes> (RNC<~> (RNC<&> (RNC:<^> (<rnc-id-or-keyword>)) ((inst <:=:> (Listof (U String Symbol)))) (<:rnc-literal:>))
                                   xml->datatypes prefix-filter)]
              [<default>   (RNC<~> (RNC<&> ((inst RNC:<_> (Listof (U String Symbol))) <namespace>)
                                           (RNC:<*> (<rnc-id-or-keyword>) '?) ((inst <:=:> (Listof (U String Symbol)))) (<:rnc-ns:literal:>))
                                   (make-xml->namespace #true) prefix-filter)]))))

(define <:rnc-pattern:> : (-> (XML-Parser (Listof RNG-Pattern)))
  (let ([<list> (<rnc-keyword> '#:list)]
        [<mixed> (<rnc-keyword> '#:mixed)]
        [<parent> (<rnc-keyword> '#:parent)]
        [<grammar> (<rnc-keyword> '#:grammar)]
        [<element> (<rnc-keyword> '#:element)]
        [<attribute> (<rnc-keyword> '#:attribute)])
    (define (rnc-qname-split [cname : Any]) : (Values (Option Symbol) (U Symbol Keyword))
      (cond [(keyword? cname) (values #false cname)]
            [else (let-values ([(ns name) (xml-qname-split (assert cname symbol?))])
                    (values ns name))]))
    
    (define (make-rnc->prefab-element [type : Keyword]) : (-> (Listof RNG-Pattern) RNG-Pattern)
      (λ [[data : (Listof RNG-Pattern)]]
        (cond [(null? data) '#:deadcode (rng-pattern)]
              [else (rng:element type (car data))])))

    (define (rnc->value [data : (Listof (U Symbol Keyword String))]) : RNG-Pattern
      (cond [(null? data) '#:deadcode (rng-pattern)]
            [(null? (cdr data)) (rng:value #false #false (assert (car data) string?))]
            [else (let-values ([(ns name) (rnc-qname-split (car data))])
                    (rng:value ns name (assert (cadr data) string?)))]))

    (define (rnc->param [data : (Listof (U Symbol String))]) : RNG-Parameter
      (cond [(or (null? data) (null? (cdr data))) (rng-parameter 'dead "code")]
            [else (rng-parameter (assert (car data) symbol?) (assert (cadr data) string?))]))

    (define (rnc->data [data : (Listof (U Symbol Keyword RNG-Parameter RNG-Pattern))]) : RNG-Pattern
      (define-values (params rest) (partition rng-parameter? data))
      (cond [(null? rest) '#:deadcode (rng-pattern)]
            [else (let-values ([(ns name) (rnc-qname-split (car rest))])
                    (cond [(null? (cdr rest)) (rng:data ns name params #false)]
                          [else (rng:data ns name params (assert (cadr rest) rng-pattern?))]))]))

    (define (rnc->element [data : (Listof (U RNG-Name-Class RNG-Pattern))]) : RNG-Pattern
      (cond [(or (null? data) (null? (cdr data))) '#:deadcode (rng-pattern)]
            [else (rng:element (assert (car data) rng-name-class?) (assert (cadr data) rng-pattern?))]))

    (define (rnc->attribute [data : (Listof (U RNG-Name-Class RNG-Pattern))]) : RNG-Pattern
      (cond [(or (null? data) (null? (cdr data))) '#:deadcode (rng-pattern)]
            [else (rng:attribute (assert (car data) rng-name-class?) (assert (cadr data) rng-pattern?))]))

    (define (rnc->multiple-pattern [data : (Listof (U RNG-Pattern Char Symbol))]) : RNG-Pattern
      (cond [(null? data) '#:deadcode (rng-pattern)]
            [(null? (cdr data)) (assert (car data) rng-pattern?)]
            [else (rng:element (case (cadr data) [(#\+) '#:+] [(#\?) '#:?] [else '#:*]) (assert (car data) rng-pattern?))]))

    (define (<datatype:name>) : (XML:Filter (U Symbol Keyword))
      (RNC:<+> (<rnc-cname>) (<rnc-keyword> '(#:string #:token))))

    (define (<:except-pattern:>) : (XML-Parser (Listof RNG-Pattern))
      (RNC<&> ((inst <:-:> (Listof RNG-Pattern))) (RNC<λ> <:rnc-pattern:>)))

    (define (<:param:>) : (XML-Parser (Listof RNG-Parameter))
      (RNC<~> (RNC<&> (RNC:<^> (<rnc-id-or-keyword>)) ((inst <:=:> (Listof (U String Symbol)))) (<:rnc-literal:>))
              rnc->param))
    
    (lambda []
      (RNC<+> ((inst RNC:<^> RNG-Pattern) (RNC:<~> (<rnc-keyword> '(#:empty #:text #:notAllowed)) rng:simple))
              ((inst RNC:<^> RNG-Pattern) (RNC:<~> (<rnc-id>) rng:ref))
              (RNC<~> (RNC<&> (RNC:<*> (<datatype:name>) '?) (<:rnc-literal:>)) rnc->value)
              (RNC<~> (RNC<&> (RNC:<^> (<datatype:name>)) (RNC<*> (<:rnc-brace:> (<:param:>) '+) '?) (RNC<*> (<:except-pattern:>) '?)) rnc->data)
              
              (RNC<?> [<element>   (RNC<~> (RNC<&> (<:rnc-name-class:>) (<:rnc-brace:> (RNC<λ> <:rnc-pattern:>))) rnc->element)]
                      [<attribute> (RNC<~> (RNC<&> (<:rnc-name-class:>) (<:rnc-brace:> (RNC<λ> <:rnc-pattern:>))) rnc->attribute)]
                      [<list>      (RNC<~> (<:rnc-brace:> (RNC<λ> <:rnc-pattern:>)) (make-rnc->prefab-element '#:list))]
                      [<mixed>     (RNC<~> (<:rnc-brace:> (RNC<λ> <:rnc-pattern:>)) (make-rnc->prefab-element '#:mixed))]
                      [<parent>    ((inst RNC:<^> RNG-Pattern) (RNC:<~> (<rnc-id>) rng:parent))]
                      [<grammar>   ((inst RNC<~> RNG-Grammar-Content RNG-Pattern) (<:rnc-brace:> (RNC<λ> <:rnc-grammar-content:>) '+) rng:grammar)])

              #;(RNC<~> (RNC<&> (RNC<λ> <:rnc-pattern:>) (RNC:<^> (<xml:delim> '(#\* #\? #\+))) '?) rnc->multiple-pattern)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define <:rnc-name-class:> : (-> (XML-Parser (Listof RNG-Name-Class)))
  (let ()
    (define (rnc->any-name [data : (Listof (U Symbol RNG-Name-Class))]) : RNG-Name-Class
      (cond [(null? data) '#:livecode (rng-any-name #false #false)]
            [(pair? (cdr data)) (rng-any-name (xml-qname-prefix (assert (car data) symbol?)) (assert (cadr data) rng-name-class?))]
            [else (let ([datum (car data)])
                    (cond [(symbol? datum) (rng-any-name (xml-qname-prefix datum) #false)]
                          [else (rng-any-name #false (assert datum rng-name-class?))]))]))

    (define (rnc->name-choice [data : (Listof RNG-Name-Class)]) : RNG-Name-Class
      (cond [(null? data) '#:deadcode (rng-name-class)]
            [(null? (cdr data)) (car data)]
            [else (rng-alt-name data)]))

    (define (<:except-name:>) : (XML-Parser (Listof RNG-Name-Class))
      (RNC<&> ((inst <:-:> (Listof RNG-Name-Class))) (RNC<λ> <:rnc-name-class:>)))

    (define (<:simple-class-name:>) : (XML-Parser (Listof RNG-Name-Class))
      (RNC<+> (RNC<~> (RNC<&> (RNC:<*> (<rnc-nsname>) '?) ((inst <:*:> (Listof Symbol))) (RNC<*> (<:except-name:>) '?)) rnc->any-name)
              (RNC:<^> ((inst RNC:<~> Symbol RNG-Name-Class) (RNC:<+> (<rnc-id-or-keyword>) (<rnc-cname>)) rng-name))))
    
    (lambda []
      (RNC<~> (RNC<+> (RNC<&> (<:simple-class-name:>)
                              (RNC<*> (RNC<&> ((inst <:/:> (Listof RNG-Name-Class))) (RNC<λ> <:simple-class-name:>)) '*))
                      (<:rnc-parenthesis:> (RNC<λ> <:rnc-name-class:>)))
              rnc->name-choice))))

(define <:rnc-grammar-content:> : (-> (XML-Parser (Listof RNG-Grammar-Content)))
  (let ([<div> (<rnc-keyword> '#:div)]
        [<start> (<rnc-keyword> '#:start)]
        [<include> (<rnc-keyword> '#:include)]
        [<inherit> (<rnc-keyword> '#:inherit)])
    (define (rnc->definition [data : (Listof (U Symbol Char RNG-Pattern))]) : RNG-Grammar-Content
      (cond [(or (null? data) (null? (cdr data))) (rng-start #\= (rng:ref 'deadcode))]
            [(null? (cddr data)) (rng-start (assert (car data) char?) (assert (cadr data) rng-pattern?))]
            [else (rng-define (assert (car data) symbol?) (assert (cadr data) char?) (assert (caddr data) rng-pattern?))]))

    (define (rnc->include-content [data : (Listof (U String Symbol RNG-Grammar-Content))]) : RNG-Grammar-Content
      (define-values (contents datum) (partition rng-grammar-content? data))
      (cond [(null? datum) (rng-include "deadcode" #false contents)]
            [(null? (cdr datum)) (rng-include (assert (car datum) string?) #false contents)]
            [else (rng-include (assert (car datum) string?) (assert (cadr datum) symbol?) contents)]))
    
    (define (<:start+define:>) : (XML-Parser (Listof RNG-Grammar-Content))
      (RNC<?> [<start> (RNC<~> (RNC<&> (RNC:<^> (<rnc-assign-method>)) (<:rnc-pattern:>)) rnc->definition)]
              [else (RNC<~> (RNC<&> (RNC:<^> (<rnc-id>)) (RNC:<^> (<rnc-assign-method>)) (<:rnc-pattern:>)) rnc->definition)]))

    (define (<:inherit:>) : (XML-Parser (Listof Symbol))
      (RNC<&> ((inst RNC<_> (Listof Symbol)) (RNC:<^> <inherit>))
              ((inst <:=:> (Listof Symbol)))
              (RNC:<^> (<rnc-id-or-keyword>))))

    (define (<:include-content:>) : (XML-Parser (Listof RNG-Grammar-Content))
      (RNC<+> (<:start+define:>)
              (RNC<?> [<div> (RNC<~> (<:rnc-brace:> (RNC<λ> <:include-content:>) '+) rng-div)])))
    
    (lambda []
      (RNC<+> (<:start+define:>)
              (RNC<?> [<div>     ((inst RNC<~> RNG-Grammar-Content RNG-Grammar-Content) (<:rnc-brace:> (RNC<λ> <:rnc-grammar-content:>) '+) rng-div)]
                      [<include> (RNC<~> (RNC<&> (<:rnc-literal:>) (RNC<*> (<:inherit:>) '?) (RNC<*> (<:rnc-brace:> (RNC<λ> <:include-content:>) '+) '?))
                                         rnc->include-content)])))))
