#lang typed/racket/base

(provide (all-defined-out) XML-Token)

(require "recognizer.rkt")

(require "../digicore.rkt")
(require "../namespace.rkt")
(require "../misc.rkt")

(require (for-syntax racket/base))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct rng-annotation ([attributes : (Listof RNG-Parameter)] [documentations : (Listof String)] [elements : (Listof RNG-Annotation-Element)])
  #:transparent #:type-name RNG-Annotation)
(struct rng-annotation-element ([name : Symbol] [attributes : (Listof RNG-Parameter)] [content : (Listof (U RNG-Annotation-Element String))])
  #:transparent #:type-name RNG-Annotation-Element)

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
(struct rng:external rng-pattern ([href : String] [inherit : (Option Symbol)]) #:transparent #:type-name RNG:External)
(struct rng:grammar rng-pattern ([contents : (Listof RNG-Grammar-Content)]) #:transparent #:type-name RNG:Grammar)
(struct rng:element rng-pattern ([name : (U Keyword RNG-Name-Class)] [child : RNG-Pattern]) #:transparent #:type-name RNG:Element)
(struct rng:particle rng-pattern ([name : Keyword] [children : (Listof RNG-Pattern)]) #:transparent #:type-name RNG:Particle)
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

;; stupid design or bad-written specs
; https://relaxng.org/compact-20021121.html#d0e331
; https://relaxng.org/compact-20021121.html#d0e377
(struct rng-annotated-parameter rng-parameter ([initial : RNG-Annotation])
  #:transparent #:type-name RNG-Annotated-Parameter)

(struct rng-annotated-pattern rng-pattern ([initial : (Option RNG-Annotation)] [primary : RNG-Pattern] [follows : (Listof RNG-Annotation-Element)])
  #:transparent #:type-name RNG-Annotated-Pattern)

(struct rng-annotated-class rng-name-class ([initial : (Option RNG-Annotation)] [simple : RNG-Name-Class] [follows : (Listof RNG-Annotation-Element)])
  #:transparent #:type-name RNG-Annotated-Class)

(struct rng-annotated-content rng-grammar-content ([initial : RNG-Annotation] [component : RNG-Grammar-Content])
  #:transparent #:type-name RNG-Annotated-Content)

; https://relaxng.org/compact-20021121.html#d0e385
(struct rng-grammar-annotation rng-grammar-content ([element : RNG-Annotation-Element]) #:transparent #:type-name RNG-Grammar-Annotation)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define rnc-grammar-parse : (All (a) (-> (XML-Parser (Listof a)) (Listof XML-Token) (Values (Listof a) (Listof XML-Token))))
  (lambda [parse tokens]
    (define-values (grammar rest) (parse null tokens))

    (cond [(not grammar) (values null rest)]
          [(exn:xml? grammar) (values null rest)]
          [else (values (reverse grammar) rest)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define <:rnc-preamble:> : (-> (XML-Parser (Listof RNG-ENV)))
  (lambda []
    (RNC<*> (<:rnc-declaration:>) '*)))

(define <:rnc-body:> : (-> (XML-Parser (Listof (U RNG-Pattern RNG-Grammar-Content))))
  (lambda []
    (RNC<+> (RNC<*> (<:rnc-grammar-content:>) '*)
            (<:rnc-pattern:>))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define <:rnc-declaration:> : (-> (XML-Parser (Listof RNG-ENV)))
  (let (;[xml:uri "http://www.w3.org/XML/1998/namespace"]
        ;[xsd:uri "http://www.w3.org/2001/XMLSchema-datatypes"]
        [<default> (<rnc:keyword> '#:default)]
        [<namespace> (<rnc:keyword> '#:namespace)]
        [<datatypes> (<rnc:keyword> '#:datatypes)])

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
      (RNC<?> [<namespace> (RNC<~> (RNC<&> (RNC:<^> (<rnc:id-or-keyword>)) ((inst <:=:> (Listof (U String Symbol)))) (<:rnc-ns:literal:>))
                                   (make-xml->namespace #false) prefix-filter)]
              [<datatypes> (RNC<~> (RNC<&> (RNC:<^> (<rnc:id-or-keyword>)) ((inst <:=:> (Listof (U String Symbol)))) (<:rnc-literal:>))
                                   xml->datatypes prefix-filter)]
              [<default>   (RNC<~> (RNC<&> ((inst RNC:<_> (Listof (U String Symbol))) <namespace>)
                                           (RNC:<*> (<rnc:id-or-keyword>) '?) ((inst <:=:> (Listof (U String Symbol)))) (<:rnc-ns:literal:>))
                                   (make-xml->namespace #true) prefix-filter)]))))

(define-values (<:rnc-initial-annotation:> <:rnc-grammar-annotation:> <:rnc-follow-annotation:>)
  (let ([<:rnc-annotation-attribute:> (<:rnc-name=value:> (<rnc:name>) rng-parameter)])
    (define (rnc->annotation [data : (Listof (U RNG-Parameter String RNG-Annotation-Element))]) : RNG-Annotation
      (let dispatch ([attributes : (Listof RNG-Parameter) null]
                     [documentations : (Listof String) null]
                     [elements : (Listof RNG-Annotation-Element) null]
                     [source : (Listof (U String RNG-Parameter RNG-Annotation-Element)) data])
        (cond [(null? source) (rng-annotation (reverse attributes) (reverse documentations) (reverse elements))]
              [else (let-values ([(self rest) (values (car source) (cdr source))])
                      (cond [(rng-parameter? self) (dispatch (cons self attributes) documentations elements rest)]
                            [(rng-annotation-element? self) (dispatch attributes documentations (cons self elements) rest)]
                            [else self (dispatch attributes (cons self documentations) elements rest)]))])))
    
    (define (rnc->annotation-element [data : (Listof (U Symbol String RNG-Parameter RNG-Annotation-Element))]) : RNG-Annotation-Element
      (let dispatch ([name : Symbol '||]
                     [attributes : (Listof RNG-Parameter) null]
                     [contents : (Listof (U String RNG-Annotation-Element)) null]
                     [source : (Listof (U Symbol String RNG-Parameter RNG-Annotation-Element)) data])
        (cond [(null? source) (rng-annotation-element name (reverse attributes) (reverse contents))]
              [else (let-values ([(self rest) (values (car source) (cdr source))])
                      (cond [(symbol? self) (dispatch self attributes contents rest)]
                            [(rng-parameter? self) (dispatch name (cons self attributes) contents rest)]
                            [(rng-annotation-element? self) (dispatch name attributes (cons self contents) rest)]
                            [(string? self) (dispatch name attributes (cons self contents) rest)]
                            [else (dispatch name attributes contents rest)]))])))

    (define (rnc-annotation-filter [A : (Listof RNG-Annotation)] [a : RNG-Annotation] [tokens : (Listof XML-Token)]) : (XML-Option True)
      (and (pair? tokens) #true))
    
    (define (<:rnc-element:>) : (XML-Parser (Listof RNG-Annotation-Element))
      (RNC<~> (RNC<&> (RNC:<^> (<rnc:name>))
                      (<:rnc-bracket:> (RNC<&> (RNC<*> <:rnc-annotation-attribute:> '*)
                                               (RNC<*> (RNC<+> (RNC<λ> <:rnc-element:>) (<:rnc-literal:>)) '*))))
              rnc->annotation-element))

    (values (lambda [] : (XML-Parser (Listof RNG-Annotation))
              (RNC<~> (RNC<&> (RNC:<*> (<xml:whitespace>) '*)
                              (RNC<*> (<:rnc-bracket:> (RNC<&> (RNC<*> <:rnc-annotation-attribute:> '*)
                                                               (RNC<*> (<:rnc-element:>) '*))) '?))
                      rnc->annotation rnc-annotation-filter))

            <:rnc-element:>

            (lambda [] : (XML-Parser (Listof RNG-Annotation-Element))
              (RNC<&> ((inst <:>:> (Listof RNG-Annotation-Element))) ((inst <:>:> (Listof RNG-Annotation-Element)))
                      (<:rnc-element:>))))))

(define <:rnc-pattern:> : (-> (XML-Parser (Listof RNG-Pattern)))
  (let ([<list> (<rnc:keyword> '#:list)]
        [<mixed> (<rnc:keyword> '#:mixed)]
        [<parent> (<rnc:keyword> '#:parent)]
        [<grammar> (<rnc:keyword> '#:grammar)]
        [<element> (<rnc:keyword> '#:element)]
        [<external> (<rnc:keyword> '#:external)]
        [<attribute> (<rnc:keyword> '#:attribute)]
        [deadcode (rng-pattern)])
    (define (rnc-qname-split [cname : Any]) : (Values (Option Symbol) (U Symbol Keyword))
      (cond [(keyword? cname) (values #false cname)]
            [else (let-values ([(ns name) (xml-qname-split (assert cname symbol?))])
                    (values ns name))]))
    
    (define (make-rnc->prefab-element [type : Keyword]) : (-> (Listof RNG-Pattern) RNG-Pattern)
      (λ [[data : (Listof RNG-Pattern)]]
        (cond [(null? data) deadcode]
              [else (rng:element type (car data))])))

    (define (rnc->value [data : (Listof (U Symbol Keyword String))]) : RNG-Pattern
      (cond [(null? data) deadcode]
            [(null? (cdr data)) (rng:value #false #false (assert (car data) string?))]
            [else (let-values ([(ns name) (rnc-qname-split (car data))])
                    (rng:value ns name (assert (cadr data) string?)))]))

    (define (rnc->data [data : (Listof (U Symbol Keyword RNG-Parameter RNG-Pattern))]) : RNG-Pattern
      (define-values (params rest) (partition rng-parameter? data))
      (cond [(null? rest) deadcode]
            [else (let-values ([(ns name) (rnc-qname-split (car rest))])
                    (cond [(null? (cdr rest)) (rng:data ns name params #false)]
                          [else (rng:data ns name params (assert (cadr rest) rng-pattern?))]))]))

    (define (rnc->element [data : (Listof (U RNG-Name-Class RNG-Pattern))]) : RNG-Pattern
      (cond [(or (null? data) (null? (cdr data))) deadcode]
            [else (rng:element (assert (car data) rng-name-class?) (assert (cadr data) rng-pattern?))]))

    (define (rnc->attribute [data : (Listof (U RNG-Name-Class RNG-Pattern))]) : RNG-Pattern
      (cond [(or (null? data) (null? (cdr data))) deadcode]
            [else (rng:attribute (assert (car data) rng-name-class?) (assert (cadr data) rng-pattern?))]))

    (define (rnc->inner-particle [data : (Listof (U RNG-Pattern Char Symbol RNG-Annotation-Element))]) : RNG-Pattern
      (let dispatch ([pattern : RNG-Pattern deadcode]
                     [particle : (Option Keyword) #false]
                     [follows : (Listof RNG-Annotation-Element) null]
                     [rest : (Listof (U RNG-Pattern Char Symbol RNG-Annotation-Element)) data])
          (if (null? rest)
              (cond [(not particle) pattern]
                    [(pair? follows) (rng-annotated-pattern #false (rng:element particle pattern) (reverse follows))]
                    [else (rng:element particle pattern)])

              (let-values ([(head tail) (values (car rest) (cdr rest))])
                (cond [(rng-pattern? head) (dispatch head particle follows tail)]
                      [(char? head) (dispatch pattern (case head [(#\+) '#:oneOrMore] [(#\?) '#:optional] [else '#:zeroOrMore]) follows tail)]
                      [(rng-annotation-element? head) (dispatch pattern particle (cons head follows) tail)]
                      [else '#:deadcode (dispatch pattern particle follows tail)])))))

    (define (rnc->external [data : (Listof (U String Symbol))]) : RNG-Pattern
      (cond [(null? data) deadcode]
            [(null? (cdr data)) (rng:external (assert (car data) string?) #false)]
            [else (rng:external (assert (car data) string?) (assert (cadr data) symbol?))]))

    (define (rnc->parameter [data : (Listof (U RNG-Parameter RNG-Annotation))]) : RNG-Parameter
      (cond [(null? data) (rng-parameter 'dead "code")]
            [(null? (cdr data)) (assert (car data) rng-parameter?)]
            [else (let ([param (assert (car data) rng-parameter?)])
                    (rng-annotated-parameter (rng-parameter-name param) (rng-parameter-value param)
                                             (assert (cadr data) rng-annotation?)))]))

    (define (<datatype:name>) : (XML:Filter (U Symbol Keyword))
      (RNC:<+> (<rnc:cname>) (<rnc:keyword> '(#:string #:token))))

    (define (<:param:>) : (XML-Parser (Listof RNG-Parameter))
      (RNC<~> (RNC<&> (RNC<*> (<:rnc-initial-annotation:>) '?)
                      (<:rnc-name=value:> (<rnc:id-or-keyword>) rng-parameter))
              rnc->parameter))

    (define (<:data-except:>) : (XML-Parser (Listof RNG-Pattern))
      (RNC<~> (RNC<&> (RNC:<^> (<datatype:name>))
                      (RNC<*> (<:rnc-brace:> (<:param:>) '+) '?)
                      ((inst <:-:> (Listof RNG-Pattern)))
                      (<:rnc-annotation:> (<:rnc-initial-annotation:>) (<:primary:>) #false rng-annotated-pattern))
              rnc->data))

    (define (<:primary:>) : (XML-Parser (Listof RNG-Pattern)) ; order matters
      (RNC<+> ((inst RNC:<^> RNG-Pattern) (RNC:<~> (<rnc:keyword> '(#:empty #:text #:notAllowed)) rng:simple))
              ((inst RNC:<^> RNG-Pattern) (RNC:<~> (<rnc:id>) rng:ref))
              (RNC<~> (RNC<&> (RNC:<*> (<datatype:name>) '?) (<:rnc-literal:>)) rnc->value)
              (RNC<~> (RNC<&> (RNC:<^> (<datatype:name>)) (RNC<*> (<:rnc-brace:> (<:param:>) '+) '?)) rnc->data)
              
              (RNC<?> [<element>   (RNC<~> (RNC<&> (<:rnc-name-class:>) (<:rnc-brace:> (RNC<λ> <:rnc-pattern:>))) rnc->element)]
                      [<attribute> (RNC<~> (RNC<&> (<:rnc-name-class:>) (<:rnc-brace:> (RNC<λ> <:rnc-pattern:>))) rnc->attribute)]
                      [<list>      (RNC<~> (<:rnc-brace:> (RNC<λ> <:rnc-pattern:>)) (make-rnc->prefab-element '#:list))]
                      [<mixed>     (RNC<~> (<:rnc-brace:> (RNC<λ> <:rnc-pattern:>)) (make-rnc->prefab-element '#:mixed))]
                      [<parent>    ((inst RNC:<^> RNG-Pattern) (RNC:<~> (<rnc:id>) rng:parent))]
                      [<grammar>   ((inst RNC<~> RNG-Grammar-Content RNG-Pattern) (<:rnc-brace:> (RNC<λ> <:rnc-grammar-content:>) '+) rng:grammar)]
                      [<external>  (RNC<~> (RNC<&> (<:rnc-literal:>) (RNC<*> (<:inherit:>) '?)) rnc->external)])

              (<:rnc-parenthesis:> (RNC<λ> <:inner-pattern:>))))

    (define (<:annotated-data-except:>) : (XML-Parser (Listof RNG-Pattern))
      (<:rnc-annotation:> (<:rnc-initial-annotation:>) (<:data-except:>) (<:rnc-follow-annotation:>)
                          rng-annotated-pattern))

    (define (<:inner-particle:>) : (XML-Parser (Listof RNG-Pattern))
      (RNC<~> (RNC<&> (<:rnc-annotation:> (<:rnc-initial-annotation:>) (<:primary:>) (<:rnc-follow-annotation:>) rng-annotated-pattern)
                      (RNC<*> (RNC<&> (RNC:<^> (<xml:delim> '(#\? #\+ #\*)))
                                      (RNC<*> (<:rnc-follow-annotation:>) '*)) '?))
              rnc->inner-particle))

    (define (<:particle-list-tail:> [type : Char]) : (XML-Parser (Listof RNG-Pattern))
      (RNC<*> (RNC<?> [(<xml:delim> type) (<:inner-particle:>)]) '+))

    (define (<:pattern-list:>) : (XML-Parser (Listof RNG-Pattern))
      (λ [[data : (Listof RNG-Pattern)] [tokens : (Listof XML-Token)]]
        (define-values (data++ tokens--) ((<:inner-particle:>) null tokens))
        
        (cond [(not (pair? data++)) (values data++ tokens--)]
              [else (let-values ([(self rest) (rnc-car/cdr tokens--)])                      
                      (cond [(not (xml:delim? self)) (values (append data data++) tokens--)]
                            [else (case (xml:delim-datum self)
                                    [(#\,) (let-values ([(options rest--) ((<:particle-list-tail:> #\,) null tokens--)])
                                             (cond [(not (list? options)) (values options rest--)]
                                                   [else (values (append data (list (rng:particle '#:group (cons (car data++) (reverse options))))) rest--)]))]
                                    [(#\|) (let-values ([(options rest--) ((<:particle-list-tail:> #\|) null tokens--)])
                                             (cond [(not (list? options)) (values options rest--)]
                                                   [else (values (append data (list (rng:particle '#:choice (cons (car data++) (reverse options))))) rest--)]))]
                                    [(#\&) (let-values ([(options rest--) ((<:particle-list-tail:> #\&) null tokens--)])
                                             (cond [(not (list? options)) (values options rest--)]
                                                   [else (values (append data (list (rng:particle '#:interleave (cons (car data++) (reverse options))))) rest--)]))]
                                    [else (values (append data data++) tokens--)])]))])))

    (define (<:inner-pattern:>) : (XML-Parser (Listof RNG-Pattern))
      (RNC<+> (<:annotated-data-except:>) ; order matters, inefficient
              (<:pattern-list:>)))
    
    (lambda []
       (<:inner-pattern:>))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define <:rnc-name-class:> : (-> (XML-Parser (Listof RNG-Name-Class)))
  (let ([deadcode (rng-name-class)])
    (define (rnc->any-name [data : (Listof (U Symbol RNG-Name-Class))]) : RNG-Name-Class
      (cond [(null? data) '#:livecode (rng-any-name #false #false)]
            [(pair? (cdr data)) (rng-any-name (xml-qname-prefix (assert (car data) symbol?)) (assert (cadr data) rng-name-class?))]
            [else (let ([datum (car data)])
                    (cond [(symbol? datum) (rng-any-name (xml-qname-prefix datum) #false)]
                          [else (rng-any-name #false (assert datum rng-name-class?))]))]))

    (define (rnc->name-choice [data : (Listof RNG-Name-Class)]) : RNG-Name-Class
      (cond [(null? data) deadcode]
            [(null? (cdr data)) (car data)]
            [else (rng-alt-name data)]))

    (define (<:nsname*:>) : (XML-Parser (Listof Symbol))
      (RNC<&> (RNC:<*> (<rnc:nsname>) '?) ((inst <:*:> (Listof Symbol)))))

    (define (<:simple-class-name:>) : (XML-Parser (Listof RNG-Name-Class))
      (RNC<+> (RNC<~> (<:nsname*:>) rnc->any-name) ; order matters
              (RNC:<^> ((inst RNC:<~> Symbol RNG-Name-Class) (RNC:<+> (<rnc:id-or-keyword>) (<rnc:cname>)) rng-name))))

    (define (<:except-name:>) : (XML-Parser (Listof RNG-Name-Class))
      (RNC<~> (RNC<&> (<:nsname*:>)
                      ((inst <:-:> (Listof RNG-Name-Class)))
                      (<:rnc-annotation:> (<:rnc-initial-annotation:>) (<:simple-class-name:>) #false rng-annotated-class))
              rnc->any-name))
    
    (define (<:annotated-except-name:>) : (XML-Parser (Listof RNG-Name-Class))
      (<:rnc-annotation:> (<:rnc-initial-annotation:>) (<:except-name:>) (<:rnc-follow-annotation:>)
                          rng-annotated-class))

    (define (<:annotated-simple-class-name:>) : (XML-Parser (Listof RNG-Name-Class))
      (<:rnc-annotation:> (<:rnc-initial-annotation:>) (<:simple-class-name:>) (<:rnc-follow-annotation:>) rng-annotated-class))
    
    (lambda [] ; a.k.a `innerNameClass`
      (RNC<+> (<:annotated-except-name:>) ; order matters, inefficient
              (RNC<~> (RNC<+> (RNC<&> (<:annotated-simple-class-name:>)
                                      (RNC<*> (RNC<?> [(<xml:delim> #\|) (RNC<λ> <:annotated-simple-class-name:>)]) '*))
                              (<:rnc-parenthesis:> (RNC<λ> <:rnc-name-class:>)))
                      rnc->name-choice)))))

(define <:rnc-grammar-content:> : (-> (XML-Parser (Listof RNG-Grammar-Content)))
  (let ([<div> (<rnc:keyword> '#:div)]
        [<start> (<rnc:keyword> '#:start)]
        [<include> (<rnc:keyword> '#:include)]
        [deadcode (rng-grammar-content)])
    (define (rnc->definition [data : (Listof (U Symbol Char RNG-Pattern))]) : RNG-Grammar-Content
      (cond [(or (null? data) (null? (cdr data))) deadcode]
            [(null? (cddr data)) (rng-start (assert (car data) char?) (assert (cadr data) rng-pattern?))]
            [else (rng-define (assert (car data) symbol?) (assert (cadr data) char?) (assert (caddr data) rng-pattern?))]))

    (define (rnc->include-content [data : (Listof (U String Symbol RNG-Grammar-Content))]) : RNG-Grammar-Content
      (define-values (contents datum) (partition rng-grammar-content? data))
      (cond [(null? datum) deadcode]
            [(null? (cdr datum)) (rng-include (assert (car datum) string?) #false contents)]
            [else (rng-include (assert (car datum) string?) (assert (cadr datum) symbol?) contents)]))

    (define (rnc->grammar-annotation [data : (Listof (U RNG-Annotation-Element))]) : RNG-Grammar-Content
      (cond [(null? data) deadcode]
            [else (rng-grammar-annotation (car data))]))

    (define (rnc->annotated-component [data : (Listof (U RNG-Annotation RNG-Grammar-Content))]) : RNG-Grammar-Content
      (cond [(null? data) deadcode]
            [(null? (cdr data)) (assert (car data) rng-grammar-content?)]
            [else (rng-annotated-content (assert (car data) rng-annotation?) (assert (cadr data) rng-grammar-content?))]))
    
    (define (<:start+define:>) : (XML-Parser (Listof RNG-Grammar-Content))
      (RNC<?> [<start> (RNC<~> (RNC<&> (RNC:<^> (<rnc:assign-method>)) (<:rnc-pattern:>)) rnc->definition)]
              [else (RNC<~> (RNC<&> (RNC:<^> (<rnc:id>)) (RNC:<^> (<rnc:assign-method>)) (<:rnc-pattern:>)) rnc->definition)]))

    (define (<:include-content:>) : (XML-Parser (Listof RNG-Grammar-Content))
      (RNC<+> (<:start+define:>)
              (RNC<?> [<div> (RNC<~> (<:rnc-brace:> (RNC<λ> <:include-member:>) '+) rng-div)])))

    (define (<:grammar-component:>) : (XML-Parser (Listof RNG-Grammar-Content))
      (RNC<+> (<:start+define:>)
              (RNC<?> [<div>     ((inst RNC<~> RNG-Grammar-Content RNG-Grammar-Content) (<:rnc-brace:> (RNC<λ> <:rnc-grammar-content:>) '+) rng-div)]
                      [<include> (RNC<~> (RNC<&> (<:rnc-literal:>) (RNC<*> (<:inherit:>) '?) (RNC<*> (<:rnc-brace:> (RNC<λ> <:include-member:>) '+) '?))
                                         rnc->include-content)])))

    (define (<:annotated-include:>) : (XML-Parser (Listof RNG-Grammar-Content))
      (RNC<~> (RNC<&> (RNC<*> (<:rnc-initial-annotation:>) '?)
                      (<:include-content:>))
              rnc->annotated-component))
    
    (define (<:annotated-component:>) : (XML-Parser (Listof RNG-Grammar-Content))
      (RNC<~> (RNC<&> (RNC<*> (<:rnc-initial-annotation:>) '?)
                      (<:grammar-component:>))
              rnc->annotated-component))

    (define (<:include-member:>) : (XML-Parser (Listof RNG-Grammar-Content))
      (RNC<+> (<:annotated-include:>)
              (RNC<~> (<:rnc-grammar-annotation:>) rnc->grammar-annotation)))
    
    (lambda [] ; a.k.a `member`
      (RNC<+> (<:annotated-component:>) ; order matters, inefficient
              (RNC<~> (<:rnc-grammar-annotation:>) rnc->grammar-annotation)))))
