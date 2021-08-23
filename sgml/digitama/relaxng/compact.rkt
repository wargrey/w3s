#lang typed/racket/base

;;; https://relaxng.org/compact-20021121.html#formal

(provide (all-defined-out))

(require "recognizer.rkt")

(require "../digicore.rkt")
(require "../namespace.rkt")

(require digimon/string)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct annotation ([attributes : (Listof (Pairof Symbol String))] [documentations : (Listof String)] [elements : (Listof Annotation-Element)])
  #:transparent #:type-name Annotation)

(struct annotation-element ([name : Symbol] [attributes : (Listof (Pairof Symbol String))] [content : (Listof (U Annotation-Element String))])
  #:transparent #:type-name Annotation-Element)

(struct preamble ([prefix : Symbol]) #:transparent #:type-name Preamble)
(struct $namespace preamble ([uri : (U String Symbol)] [default? : Boolean]) #:transparent)
(struct $datatype preamble ([uri : String]) #:transparent)

(struct pattern () #:transparent #:type-name Pattern)
(struct name-class () #:transparent #:type-name Name-Class)
(struct grammar-content () #:transparent #:type-name Grammar-Content)
(struct parameter ([name : Symbol] [value : String]) #:transparent #:type-name Parameter)

(struct $simple pattern ([element : Keyword]) #:transparent)
(struct $ref pattern ([element : Symbol]) #:transparent)
(struct $parent $ref () #:transparent)
(struct $external pattern ([href : String] [inherit : (Option Symbol)]) #:transparent)
(struct $grammar pattern ([contents : (Listof Grammar-Content)]) #:transparent)
(struct $element pattern ([name : (U Keyword Name-Class)] [child : Pattern]) #:transparent)
(struct $particle pattern ([name : Keyword] [children : (Listof Pattern)]) #:transparent)
(struct $attribute pattern ([name : Name-Class] [child : Pattern]) #:transparent)
(struct $value pattern ([ns : (Option Symbol)] [name : (U Symbol Keyword False)] [literal : String]) #:transparent)
(struct $data pattern ([ns : (Option Symbol)] [name : (U Symbol Keyword)] [params : (Listof (Pairof Symbol String))] [except : (Option Pattern)]) #:transparent)

(struct $name name-class ([ns : (Option Symbol)] [id : Symbol]) #:transparent)
(struct $any-name name-class ([ns : (Option Symbol)] [except : (Option Name-Class)]) #:transparent)
(struct $alt-name name-class ([options : (Listof Name-Class)]) #:transparent)

(struct $start grammar-content ([combine : (Option Char)] [pattern : Pattern]) #:transparent)
(struct $define grammar-content ([name : Symbol] [combine : (Option Char)] [pattern : Pattern]) #:transparent)
(struct $div grammar-content ([contents : (Listof Grammar-Content)]) #:transparent)
(struct $include grammar-content ([href : String] [inherit : (Option Symbol)] [contents : (Listof Grammar-Content)]) #:transparent)

;; stupid design or bad-written specs
; https://relaxng.org/compact-20021121.html#d0e331
; https://relaxng.org/compact-20021121.html#d0e377
(struct annotated-parameter parameter ([initial : Annotation]) #:transparent)
(struct annotated-pattern pattern ([initial : (Option Annotation)] [primary : Pattern] [follows : (Listof Annotation-Element)]) #:transparent)
(struct annotated-class name-class ([initial : (Option Annotation)] [simple : Name-Class] [follows : (Listof Annotation-Element)]) #:transparent)
(struct annotated-content grammar-content ([initial : Annotation] [component : Grammar-Content]) #:transparent)

; https://relaxng.org/compact-20021121.html#d0e385
(struct grammar-annotation grammar-content ([element : Annotation-Element]) #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type RNG-Preamble-Namespaces (Immutable-HashTable Symbol (Option String)))
(define-type RNG-Preamble-Datatypes (Immutable-HashTable Symbol String))

(define prefab-namespaces : RNG-Preamble-Namespaces
  #hasheq((xml . "http://www.w3.org/XML/1998/namespace")
          (a . "http://relaxng.org/ns/compatibility/annotations/1.0")))

(define prefab-datatypes : RNG-Preamble-Datatypes
  #hasheq((xsd . "http://www.w3.org/2001/XMLSchema-datatypes")))

(define rnc-check-prefix? : (Parameterof Boolean) (make-parameter #false))
(define rnc-default-namespaces : (Parameterof RNG-Preamble-Namespaces) (make-parameter prefab-namespaces))
(define rnc-default-datatypes : (Parameterof RNG-Preamble-Datatypes) (make-parameter prefab-datatypes))

(define rnc-shadow-start? : (Parameterof Boolean) (make-parameter #false))
(define rnc-shadow-definitions : (Parameterof (Listof Symbol)) (make-parameter null))

(define rnc-grammar-parse : (All (a) (-> (XML-Parser (Listof a)) (Listof XML-Token) (Values (Listof a) (Listof XML-Token))))
  (lambda [parse tokens]
    (define-values (grammar rest) (parse null tokens))

    (cond [(not grammar) (values null rest)]
          [(exn:xml? grammar) (values null rest)]
          [else (values (reverse grammar) rest)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define <:rnc-preamble:> : (-> (XML-Parser (Listof Preamble)))
  (lambda []
    (RNC<*> (<:rnc-declaration:>) '*)))

(define <:rnc-body:> : (-> (XML-Parser (Listof (U Pattern Grammar-Content))))
  (lambda []
    (RNC<+> (RNC<*> (<:rnc-grammar-content:>) '*)
            (<:rnc-pattern:>))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define rnc-grammar-environment : (-> (Listof Preamble) (Values (Option (U String Symbol)) RNG-Preamble-Namespaces RNG-Preamble-Datatypes))
  (lambda [preamble]
    (let env ([default-ns : (Option (U String Symbol)) #false]
              ; TODO: if prefixes should be added for `include` and `external`
              [ns : RNG-Preamble-Namespaces prefab-namespaces]
              [dts : RNG-Preamble-Datatypes prefab-datatypes]
              [decls : (Listof Preamble) preamble])
      (cond [(null? decls) (values default-ns ns dts)]
            [else (let-values ([(self rest) (values (car decls) (cdr decls))])
                    (cond [($datatype? self)
                           (env default-ns ns (hash-set dts (preamble-prefix self) ($datatype-uri self)) rest)]
                          [($namespace? self)
                           (let ([prefix (preamble-prefix self)]
                                 [uri ($namespace-uri self)])
                             (cond [(not ($namespace-default? self))
                                    (env default-ns (hash-set ns prefix (and (string? uri) uri)) dts rest)]
                                   [(eq? prefix '||)
                                    (env (and (string? uri) uri) ns dts rest)]
                                   [else ; default namespace prefix = uri <==> default namespace = uri; prefix = uri
                                    (env prefix (hash-set dts prefix (and (string? uri) uri)) dts rest)]))]
                          [else '#:deadcode (env default-ns ns dts rest)]))]))))

(define rnc-uri-guard : (-> (Listof String) String (Listof XML-Token) (XML-Option True))
  (lambda [data literal tokens]
    (cond [(string-uri? literal) #true]
          [else (make+exn:rnc:uri tokens)])))

(define make-rnc:prefix-guard : (All (a b) (-> (-> Any Boolean : b) (-> b (Option Symbol)) (-> HashTableTop) HashTableTop
                                               (-> a XML-Syntax-Any (XML-Option True))))
  (lambda [subobject? object->key bases prefabs]
    (λ [datum token]
      (define key (and (subobject? datum) (object->key datum)))
      (rnc-prefix-guard key bases prefabs token))))

(define make-rnc-prefix-guard : (All (a b) (-> (-> Any Boolean : b) (-> b (Option Symbol)) (-> HashTableTop) HashTableTop
                                               (-> (Listof a) a (Listof XML-Token) (XML-Option True))))
  (lambda [subobject? object->key bases prefabs]
    (λ [data datum tokens]
      (define key (and (subobject? datum) (object->key datum)))
      (rnc-prefix-guard key bases prefabs tokens))))

(define make-rnc-prefixed-name-guard : (All (a b) (-> (-> Any Boolean : b) (-> b (Option Symbol)) (-> HashTableTop) HashTableTop
                                                      (-> (Listof a) a (Listof XML-Token) (XML-Option True))))
  (lambda [subobject? object->key bases prefabs]
    (λ [data datum tokens]
      (define key (and (subobject? datum) (object->key datum)))
      (define-values (ns name) (if (not key) (values '|| '||) (xml-qname-split key)))

      (cond [(eq? ns 'inherit) (make+exn:rnc:annotation tokens)]
            [else (rnc-prefix-guard (if (eq? ns '||) #false ns) bases prefabs tokens)]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define <:rnc-declaration:> : (-> (XML-Parser (Listof Preamble)))
  (let (;[xml:uri "http://www.w3.org/XML/1998/namespace"]
        ;[xsd:uri "http://www.w3.org/2001/XMLSchema-datatypes"]
        [<default> (<rnc:keyword> '#:default)]
        [<namespace> (<rnc:keyword> '#:namespace)]
        [<datatypes> (<rnc:keyword> '#:datatypes)])

    (define (prefix-guard [NS : (Listof Preamble)] [ns : Preamble] [tokens : (Listof XML-Token)]) : (XML-Option True)
      (define-values (prefix uri default?)
        (if ($namespace? ns)
            (values (preamble-prefix ns) ($namespace-uri ns) ($namespace-default? ns))
            (values (preamble-prefix ns) ($datatype-uri (assert ns $datatype?)) #false)))

      (let check-duplicate ([prefixes : (Listof Preamble) NS])
        (cond [(null? prefixes) #true]
              [else (let-values ([(self rest) (values (car prefixes) (cdr prefixes))])
                      (cond [(and default? ($namespace? self) ($namespace-default? self))
                             (make+exn:xml:duplicate tokens) #true]
                            [(and (eq? prefix (preamble-prefix self)) (eq? (object-name ns) (object-name self)))
                             (make+exn:xml:duplicate tokens) #true]
                            [else (check-duplicate rest)]))]))
      
      #;(cond [(eq? prefix 'xml) (if (equal? uri xml:uri) #true (make+exn:rnc:uri tokens))]
              [(eq? prefix 'xsd) (if (equal? uri xsd:uri) #true (make+exn:rnc:uri tokens))]
              [(eq? prefix 'xmlns) (make+exn:rnc:prefix tokens)]
              [(equal? uri xml:uri) (if (eq? prefix 'xml) #true (make+exn:rnc:prefix tokens))]
              [else #true]))

    (define (make-xml->namespace [default? : Boolean]) : (-> (Listof (U String Symbol)) Preamble)
      (λ [data]
        (cond [(null? data) '#:deadcode ($namespace '|| "" default?)]
              [(null? (cdr data)) ($namespace '|| (car data) default?)]
              [else ($namespace (assert (car data) symbol?) (assert (cadr data) string?) default?)])))

    (define (xml->datatypes [data : (Listof (U String Symbol))]) : Preamble
      (cond [(or (null? data) (null? (cdr data))) ($datatype '|| "deadcode")]
            [else ($datatype (assert (car data) symbol?) (assert (cadr data) string?))]))

    (lambda []
      (RNC<?> [<namespace> (RNC<~> (RNC<&> (RNC:<^> (<rnc:id-or-keyword>)) ((inst <:=:> (Listof (U String Symbol)))) (<:rnc-ns:literal:>))
                                   (make-xml->namespace #false) prefix-guard)]
              [<datatypes> (RNC<~> (RNC<&> (RNC:<^> (<rnc:id-or-keyword>)) ((inst <:=:> (Listof (U String Symbol)))) (<:rnc-literal:>))
                                   xml->datatypes prefix-guard)]
              [<default>   (RNC<~> (RNC<&> ((inst RNC:<_> (Listof (U String Symbol))) <namespace>)
                                           (RNC:<*> (<rnc:id-or-keyword>) '?) ((inst <:=:> (Listof (U String Symbol)))) (<:rnc-ns:literal:>))
                                   (make-xml->namespace #true) prefix-guard)]))))

(define-values (<:rnc-initial-annotation:> <:rnc-grammar-annotation:> <:rnc-follow-annotation:>)
  (let* ([attrspace-guard (make-rnc-prefixed-name-guard parameter? parameter-name rnc-default-namespaces prefab-namespaces)]
         [element-guard (make-rnc-prefixed-name-guard annotation-element? annotation-element-name rnc-default-namespaces prefab-namespaces)])
    (define (rnc->annotation [data : (Listof (U Parameter String Annotation-Element))]) : Annotation
      (let dispatch ([attributes : (Listof (Pairof Symbol String)) null]
                     [a:docs : (Listof String) null]
                     [elements : (Listof Annotation-Element) null]
                     [source : (Listof (U String Parameter Annotation-Element)) data])
        (cond [(null? source) (annotation (reverse attributes) (reverse a:docs) (reverse elements))]
              [else (let-values ([(self rest) (values (car source) (cdr source))])
                      (cond [(annotation-element? self) (dispatch attributes a:docs (cons self elements) rest)]
                            [(parameter? self) (dispatch (cons (cons (parameter-name self) (parameter-value self)) attributes) a:docs elements rest)]
                            [else (dispatch attributes (cons self a:docs) elements rest)]))])))
    
    (define (rnc->annotation-element [data : (Listof (U Symbol String Parameter Annotation-Element))]) : Annotation-Element
      (let dispatch ([name : Symbol '||]
                     [attributes : (Listof (Pairof Symbol String)) null]
                     [contents : (Listof (U String Annotation-Element)) null]
                     [source : (Listof (U Symbol String Parameter Annotation-Element)) data])
        (cond [(null? source) (annotation-element name (reverse attributes) (reverse contents))]
              [else (let-values ([(self rest) (values (car source) (cdr source))])
                      (cond [(symbol? self) (dispatch self attributes contents rest)]
                            [(parameter? self) (dispatch name (cons (cons (parameter-name self) (parameter-value self)) attributes) contents rest)]
                            [(annotation-element? self) (dispatch name attributes (cons self contents) rest)]
                            [(string? self) (dispatch name attributes (cons self contents) rest)]
                            [else (dispatch name attributes contents rest)]))])))

    (define (annotation-guard [A : (Listof Annotation)] [a : Annotation] [tokens : (Listof XML-Token)]) : (XML-Option True)
      (and (pair? tokens) #true))

    (define (attribute-guard [attrs : (Listof Parameter)] [attr : Parameter] [tokens : (Listof XML-Token)]) : (XML-Option True)
      (define name (parameter-name attr))
      
      (let check-duplicate ([attrs : (Listof Parameter) attrs])
        (cond [(null? attrs) #true]
              [else (let-values ([(self rest) (values (car attrs) (cdr attrs))])
                      (if (eq? name (parameter-name self))
                          (make+exn:xml:duplicate tokens)
                          (check-duplicate rest)))]))
      
      (attrspace-guard attrs attr tokens))

    (define (<:rnc-annotation-attribute:>)
      (<:rnc-name=value:> (<rnc:name>) parameter attribute-guard))
    
    (define (<:rnc-element:>) : (XML-Parser (Listof Annotation-Element))
      (RNC<~> (RNC<&> (RNC:<^> (<rnc:name>))
                      (<:rnc-bracket:> (RNC<&> (RNC<*> (<:rnc-annotation-attribute:>) '*)
                                               (RNC<*> (RNC<+> (RNC<λ> <:rnc-element:>) (<:rnc-literal:>)) '*))))
              rnc->annotation-element element-guard))

    (values (lambda [] : (XML-Parser (Listof Annotation))
              (RNC<~> (RNC<&> (RNC:<*> (<xml:whitespace>) '*)
                              (RNC<*> (<:rnc-bracket:> (RNC<&> (RNC<*> (<:rnc-annotation-attribute:>) '*)
                                                               (RNC<*> (<:rnc-element:>) '*))) '?))
                      rnc->annotation annotation-guard))

            <:rnc-element:>

            (lambda [] : (XML-Parser (Listof Annotation-Element))
              (RNC<&> ((inst <:>:> (Listof Annotation-Element))) ((inst <:>:> (Listof Annotation-Element)))
                      (<:rnc-element:>))))))

(define <:rnc-pattern:> : (-> (XML-Parser (Listof Pattern)))
  (let ([<list> (<rnc:keyword> '#:list)]
        [<mixed> (<rnc:keyword> '#:mixed)]
        [<parent> (<rnc:keyword> '#:parent)]
        [<grammar> (<rnc:keyword> '#:grammar)]
        [<element> (<rnc:keyword> '#:element)]
        [<external> (<rnc:keyword> '#:external)]
        [<attribute> (<rnc:keyword> '#:attribute)]
        [external-guard (make-rnc-prefix-guard $external? $external-inherit rnc-default-namespaces prefab-namespaces)]
        [value-guard (make-rnc-prefix-guard $value? $value-ns rnc-default-datatypes prefab-datatypes)]
        [data-guard (make-rnc-prefix-guard $data? $data-ns rnc-default-datatypes prefab-datatypes)]
        [deadcode (pattern)])
    (define (rnc-qname-split [cname : Any]) : (Values (Option Symbol) (U Symbol Keyword))
      (cond [(keyword? cname) (values #false cname)]
            [else (let-values ([(ns name) (xml-qname-split (assert cname symbol?))])
                    (values ns name))]))
    
    (define (make-rnc->prefab-element [type : Keyword]) : (-> (Listof Pattern) Pattern)
      (λ [[data : (Listof Pattern)]]
        (cond [(null? data) deadcode]
              [else ($element type (car data))])))

    (define (rnc->value [data : (Listof (U Symbol Keyword String))]) : Pattern
      (cond [(null? data) deadcode]
            [(null? (cdr data)) ($value #false #false (assert (car data) string?))]
            [else (let-values ([(ns name) (rnc-qname-split (car data))])
                    ($value ns name (assert (cadr data) string?)))]))

    (define (rnc->data [data : (Listof (U Symbol Keyword Parameter Pattern))]) : Pattern
      (define-values (params rest) (partition parameter? data))
      (cond [(null? rest) deadcode]
            [else (let-values ([(ns name) (rnc-qname-split (car rest))])
                    (cond [(null? (cdr rest)) ($data ns name (map rnc-parameter->pair params) #false)]
                          [else ($data ns name (map rnc-parameter->pair params) (assert (cadr rest) pattern?))]))]))

    (define (rnc->element [data : (Listof (U Name-Class Pattern))]) : Pattern
      (cond [(or (null? data) (null? (cdr data))) deadcode]
            [else ($element (assert (car data) name-class?) (assert (cadr data) pattern?))]))

    (define (rnc->attribute [data : (Listof (U Name-Class Pattern))]) : Pattern
      (cond [(or (null? data) (null? (cdr data))) deadcode]
            [else ($attribute (assert (car data) name-class?) (assert (cadr data) pattern?))]))

    (define (rnc->inner-particle [data : (Listof (U Pattern Char Symbol Annotation-Element))]) : Pattern
      (let dispatch ([self : Pattern deadcode]
                     [particle : (Option Keyword) #false]
                     [follows : (Listof Annotation-Element) null]
                     [rest : (Listof (U Pattern Char Symbol Annotation-Element)) data])
          (if (null? rest)
              (cond [(not particle) self]
                    [(pair? follows) (annotated-pattern #false ($element particle self) (reverse follows))]
                    [else ($element particle self)])

              (let-values ([(head tail) (values (car rest) (cdr rest))])
                (cond [(pattern? head) (dispatch head particle follows tail)]
                      [(char? head) (dispatch self (case head [(#\+) '#:oneOrMore] [(#\?) '#:optional] [else '#:zeroOrMore]) follows tail)]
                      [(annotation-element? head) (dispatch self particle (cons head follows) tail)]
                      [else '#:deadcode (dispatch self particle follows tail)])))))

    (define (rnc->external [data : (Listof (U String Symbol))]) : Pattern
      (cond [(null? data) deadcode]
            [(null? (cdr data)) ($external (assert (car data) string?) #false)]
            [else ($external (assert (car data) string?) (assert (cadr data) symbol?))]))

    (define (rnc->parameter [data : (Listof (U Parameter Annotation))]) : Parameter
      (cond [(null? data) (parameter 'dead "code")]
            [(null? (cdr data)) (assert (car data) parameter?)]
            [else (let ([param (assert (car data) parameter?)])
                    (annotated-parameter (parameter-name param) (parameter-value param)
                                             (assert (cadr data) annotation?)))]))

    (define (<datatype:name>) : (XML:Filter (U Symbol Keyword))
      (RNC:<+> (<rnc:cname>) (<rnc:keyword> '(#:string #:token))))

    (define (<:param:>) : (XML-Parser (Listof Parameter))
      (RNC<~> (RNC<&> (RNC<*> (<:rnc-initial-annotation:>) '?)
                      (<:rnc-name=value:> (<rnc:id-or-keyword>) parameter))
              rnc->parameter))

    (define (<:data-except:>) : (XML-Parser (Listof Pattern))
      (RNC<~> (RNC<&> (RNC:<^> (<datatype:name>))
                      (RNC<*> (<:rnc-brace:> (<:param:>) '+) '?)
                      ((inst <:-:> (Listof Pattern)))
                      (<:rnc-annotation:> (<:rnc-initial-annotation:>) (<:primary:>) #false annotated-pattern))
              rnc->data))

    (define (<:primary:>) : (XML-Parser (Listof Pattern)) ; order matters
      (RNC<+> ((inst RNC:<^> Pattern) (RNC:<~> (<rnc:keyword> '(#:empty #:text #:notAllowed)) $simple))
              ((inst RNC:<^> Pattern) (RNC:<~> (<rnc:id>) $ref))
              (RNC<~> (RNC<&> (RNC:<*> (<datatype:name>) '?) (<:rnc-literal:>)) rnc->value value-guard)
              (RNC<~> (RNC<&> (RNC:<^> (<datatype:name>)) (RNC<*> (<:rnc-brace:> (<:param:>) '+) '?)) rnc->data data-guard)
              
              (RNC<?> [<element>   (RNC<~> (RNC<&> (<:rnc-name-class:>) (<:rnc-brace:> (RNC<λ> <:rnc-pattern:>))) rnc->element)]
                      [<attribute> (RNC<~> (RNC<&> (<:rnc-name-class:>) (<:rnc-brace:> (RNC<λ> <:rnc-pattern:>))) rnc->attribute)]
                      [<list>      (RNC<~> (<:rnc-brace:> (RNC<λ> <:rnc-pattern:>)) (make-rnc->prefab-element '#:list))]
                      [<mixed>     (RNC<~> (<:rnc-brace:> (RNC<λ> <:rnc-pattern:>)) (make-rnc->prefab-element '#:mixed))]
                      [<parent>    ((inst RNC:<^> Pattern) (RNC:<~> (<rnc:id>) $parent))]
                      [<grammar>   ((inst RNC<~> Grammar-Content Pattern) (<:rnc-brace:> (RNC<λ> <:rnc-grammar-content:>) '+) $grammar)]
                      [<external>  (RNC<~> (RNC<&> (<:rnc-literal:> rnc-uri-guard) (RNC<*> (<:inherit:>) '?)) rnc->external external-guard)])

              (<:rnc-parenthesis:> (RNC<λ> <:inner-pattern:>))))

    (define (<:annotated-data-except:>) : (XML-Parser (Listof Pattern))
      (<:rnc-annotation:> (<:rnc-initial-annotation:>) (<:data-except:>) (<:rnc-follow-annotation:>)
                          annotated-pattern))

    (define (<:inner-particle:>) : (XML-Parser (Listof Pattern))
      (RNC<~> (RNC<&> (<:rnc-annotation:> (<:rnc-initial-annotation:>) (<:primary:>) (<:rnc-follow-annotation:>) annotated-pattern)
                      (RNC<*> (RNC<&> (RNC:<^> (<xml:delim> '(#\? #\+ #\*)))
                                      (RNC<*> (<:rnc-follow-annotation:>) '*)) '?))
              rnc->inner-particle))

    (define (<:particle-list-tail:> [type : Char]) : (XML-Parser (Listof Pattern))
      (RNC<*> (RNC<?> [(<xml:delim> type) (<:inner-particle:>)]) '+))

    (define (<:pattern-list:>) : (XML-Parser (Listof Pattern))
      (λ [[data : (Listof Pattern)] [tokens : (Listof XML-Token)]]
        (define-values (data++ tokens--) ((<:inner-particle:>) null tokens))
        
        (cond [(not (pair? data++)) (values data++ tokens--)]
              [else (let-values ([(self rest) (rnc-car/cdr tokens--)])                      
                      (cond [(not (xml:delim? self)) (values (append data data++) tokens--)]
                            [else (case (xml:delim-datum self)
                                    [(#\,) (let-values ([(options rest--) ((<:particle-list-tail:> #\,) null tokens--)])
                                             (cond [(not (list? options)) (values options rest--)]
                                                   [else (values (append data (list ($particle '#:group (cons (car data++) (reverse options))))) rest--)]))]
                                    [(#\|) (let-values ([(options rest--) ((<:particle-list-tail:> #\|) null tokens--)])
                                             (cond [(not (list? options)) (values options rest--)]
                                                   [else (values (append data (list ($particle '#:choice (cons (car data++) (reverse options))))) rest--)]))]
                                    [(#\&) (let-values ([(options rest--) ((<:particle-list-tail:> #\&) null tokens--)])
                                             (cond [(not (list? options)) (values options rest--)]
                                                   [else (values (append data (list ($particle '#:interleave (cons (car data++) (reverse options))))) rest--)]))]
                                    [else (values (append data data++) tokens--)])]))])))

    (define (<:inner-pattern:>) : (XML-Parser (Listof Pattern))
      (RNC<+> (<:annotated-data-except:>) ; order matters, inefficient
              (<:pattern-list:>)))
    
    (lambda []
       (<:inner-pattern:>))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define <:rnc-name-class:> : (-> (XML-Parser (Listof Name-Class)))
  (let ([prefix-guard (make-rnc-prefix-guard $any-name? $any-name-ns rnc-default-namespaces prefab-namespaces)]
        [cname:guard (make-rnc:prefix-guard $name? $name-ns rnc-default-namespaces prefab-namespaces)]
        [deadcode (name-class)])
    (define (rnc->name [datum : Symbol]) : Name-Class
      (define-values (prefix name) (xml-qname-split datum))
      ($name (if (eq? prefix '||) #false prefix) name))
    
    (define (rnc->any-name [data : (Listof (U Symbol Name-Class))]) : Name-Class
      (cond [(null? data) '#:livecode ($any-name #false #false)]
            [(pair? (cdr data)) ($any-name (xml-qname-prefix (assert (car data) symbol?)) (assert (cadr data) name-class?))]
            [else (let ([datum (car data)])
                    (cond [(symbol? datum) ($any-name (xml-qname-prefix datum) #false)]
                          [else ($any-name #false (assert datum name-class?))]))]))

    (define (rnc->name-choice [data : (Listof Name-Class)]) : Name-Class
      (cond [(null? data) deadcode]
            [(null? (cdr data)) (car data)]
            [else ($alt-name data)]))

    (define (<:nsname*:>) : (XML-Parser (Listof Symbol))
      (RNC<&> (RNC:<*> (<rnc:nsname>) '?)
              ((inst <:*:> (Listof Symbol)))))

    (define (<:simple-class-name:>) : (XML-Parser (Listof Name-Class))
      (RNC<+> (RNC<~> (<:nsname*:>) rnc->any-name prefix-guard) ; order matters
              (RNC:<^> (RNC:<+> ((inst RNC:<~> Symbol Name-Class) (<rnc:id-or-keyword>) rnc->name)
                                ((inst RNC:<~> Symbol Name-Class) (<rnc:cname>) rnc->name cname:guard)))))

    (define (<:except-name:>) : (XML-Parser (Listof Name-Class))
      (RNC<~> (RNC<&> (<:nsname*:>)
                      ((inst <:-:> (Listof Name-Class)))
                      (<:rnc-annotation:> (<:rnc-initial-annotation:>) (<:simple-class-name:>) #false annotated-class))
              rnc->any-name prefix-guard))
    
    (define (<:annotated-except-name:>) : (XML-Parser (Listof Name-Class))
      (<:rnc-annotation:> (<:rnc-initial-annotation:>) (<:except-name:>) (<:rnc-follow-annotation:>)
                          annotated-class))

    (define (<:annotated-simple-class-name:>) : (XML-Parser (Listof Name-Class))
      (<:rnc-annotation:> (<:rnc-initial-annotation:>) (<:simple-class-name:>) (<:rnc-follow-annotation:>) annotated-class))
    
    (lambda [] ; a.k.a `innerNameClass`
      (RNC<+> (<:annotated-except-name:>) ; order matters, inefficient
              (RNC<~> (RNC<+> (RNC<&> (<:annotated-simple-class-name:>)
                                      (RNC<*> (RNC<?> [(<xml:delim> #\|) (RNC<λ> <:annotated-simple-class-name:>)]) '*))
                              (<:rnc-parenthesis:> (RNC<λ> <:rnc-name-class:>)))
                      rnc->name-choice)))))

(define <:rnc-grammar-content:> : (-> (XML-Parser (Listof Grammar-Content)))
  (let ([<div> (<rnc:keyword> '#:div)]
        [<start> (<rnc:keyword> '#:start)]
        [<include> (<rnc:keyword> '#:include)]
        [include-guard (make-rnc-prefix-guard $include? $include-inherit rnc-default-namespaces prefab-namespaces)]
        [shadow-grammar (grammar-content)])
    (define (rnc->definition [data : (Listof (U Symbol Char Pattern))]) : Grammar-Content
      (cond [(or (null? data) (null? (cdr data))) shadow-grammar]
            [(null? (cddr data)) (if (rnc-shadow-start?) shadow-grammar ($start (assert (car data) char?) (assert (cadr data) pattern?)))]
            [(memq (car data) (rnc-shadow-definitions)) shadow-grammar]
            [else ($define (assert (car data) symbol?) (assert (cadr data) char?) (assert (caddr data) pattern?))]))

    (define (rnc->include-content [data : (Listof (U String Symbol Grammar-Content))]) : Grammar-Content
      (define-values (contents datum) (partition grammar-content? data))
      (cond [(null? datum) shadow-grammar]
            [(null? (cdr datum)) ($include (assert (car datum) string?) #false contents)]
            [else ($include (assert (car datum) string?) (assert (cadr datum) symbol?) contents)]))

    (define (rnc->grammar-annotation [data : (Listof (U Annotation-Element))]) : Grammar-Content
      (cond [(null? data) shadow-grammar]
            [else (grammar-annotation (car data))]))

    (define (rnc->annotated-component [data : (Listof (U Annotation Grammar-Content))]) : Grammar-Content
      (cond [(null? data) shadow-grammar]
            [(null? (cdr data)) (assert (car data) grammar-content?)]
            [else (annotated-content (assert (car data) annotation?) (assert (cadr data) grammar-content?))]))
    
    (define (<:start+define:>) : (XML-Parser (Listof Grammar-Content))
      (RNC<?> [<start> (RNC<~> (RNC<&> (RNC:<^> (<rnc:assign-method>)) (<:rnc-pattern:>)) rnc->definition)]
              [else (RNC<~> (RNC<&> (RNC:<^> (<rnc:id>)) (RNC:<^> (<rnc:assign-method>)) (<:rnc-pattern:>)) rnc->definition)]))

    (define (<:include-content:>) : (XML-Parser (Listof Grammar-Content))
      (RNC<+> (<:start+define:>)
              (RNC<?> [<div> (RNC<~> (<:rnc-brace:> (RNC<λ> <:include-member:>) '+) $div)])))

    (define (<:grammar-component:>) : (XML-Parser (Listof Grammar-Content))
      (RNC<+> (<:start+define:>)
              (RNC<?> [<div>     ((inst RNC<~> Grammar-Content Grammar-Content) (<:rnc-brace:> (RNC<λ> <:rnc-grammar-content:>) '+) $div)]
                      [<include> (RNC<~> (RNC<&> (<:rnc-literal:> rnc-uri-guard) (RNC<*> (<:inherit:>) '?) (RNC<*> (<:rnc-brace:> (RNC<λ> <:include-member:>) '+) '?))
                                         rnc->include-content include-guard)])))

    (define (<:annotated-include:>) : (XML-Parser (Listof Grammar-Content))
      (RNC<~> (RNC<&> (RNC<*> (<:rnc-initial-annotation:>) '?)
                      (<:include-content:>))
              rnc->annotated-component))
    
    (define (<:annotated-component:>) : (XML-Parser (Listof Grammar-Content))
      (RNC<~> (RNC<&> (RNC<*> (<:rnc-initial-annotation:>) '?)
                      (<:grammar-component:>))
              rnc->annotated-component))

    (define (<:include-member:>) : (XML-Parser (Listof Grammar-Content))
      (RNC<+> (<:annotated-include:>)
              (RNC<~> (<:rnc-grammar-annotation:>) rnc->grammar-annotation)))
    
    (lambda [] ; a.k.a `member`
      (RNC<+> (<:annotated-component:>) ; order matters, inefficient
              (RNC<~> (<:rnc-grammar-annotation:>) rnc->grammar-annotation)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define rnc-parameter->pair : (-> Parameter (Pairof Symbol String))
  (lambda [param]
    (cons (parameter-name param) (parameter-value param))))

(define rnc-prefix-guard : (-> (Option Symbol) (-> HashTableTop) HashTableTop (U XML-Syntax-Any (Listof XML-Token)) (XML-Option True))
  (lambda [key bases prefabs tokens]
    (cond [(not key) #true]
          [(not (rnc-check-prefix?)) #true]
          [(hash-has-key? (bases) key) #true]
          [(hash-has-key? prefabs key) #true]
          [else (make+exn:rnc:prefix tokens)])))
