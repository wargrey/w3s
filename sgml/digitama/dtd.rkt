#lang typed/racket/base

(provide (all-defined-out))

(require "digicore.rkt")
(require "stdin.rkt")
(require "grammar.rkt")
(require "tokenizer.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type XML-Type-Element-Mixed (Boxof (Listof XML:Name)))
(define-type XML-Type-Element-Sequence (Vectorof (U (Pairof Char XML:Name) XML-Type-Element-Children)))
(define-type XML-Type-Element-Choice (Listof (U (Pairof Char XML:Name) XML-Type-Element-Children)))
(define-type XML-Type-Element-Children (U (Pairof Char XML-Type-Element-Sequence) (Pairof Char XML-Type-Element-Choice)))
(define-type XML-Type-Element-Content (U XML-Type-Element-Mixed XML-Type-Element-Children))

(define-type XML-Type-Attribute-Type (U XML:Name (Listof XML:Name)))
(define-type XML-Type-Attribute-Default (U XML:Name XML:String))

(define-type XML-Type-Definition*
  (U XML-Processing-Instruction* XML-Entity XML-Notation
     XML-Element-Content XML-Attribute-List XML-Declaration*))

(define-type XML-Type-Declaration*
  (Rec defs (U XML-Type-Definition* XML:PEReference
               (Pairof (U XML:Name XML:PEReference) (Listof defs)))))

(struct xml-entity
  ([name : (U XML:Reference XML:PEReference)]
   [value : (Option XML:String)]
   [public : (Option XML:String)]
   [system : (Option XML:String)]
   [ndata : (Option XML:Name)])
  #:transparent
  #:type-name XML-Entity)

(struct xml-notation
  ([name : XML:Name]
   [public : (Option XML:String)]
   [system : (Option XML:String)])
  #:transparent
  #:type-name XML-Notation)

(struct xml-attribute
  ([element : XML:Name]
   [name : XML:Name]
   [type : XML-Type-Attribute-Type]
   [default : XML-Type-Attribute-Default]
   [notation? : Boolean]
   [fixed? : Boolean])
  #:transparent
  #:type-name XML-Attribute)

(struct xml-attribute-list
  ([element : XML:Name]
   [body : (Listof XML-Attribute)])
  #:transparent
  #:type-name XML-Attribute-List)

(struct xml-element-content
  ([name : XML:Name]
   [body : (U XML:Name XML-Type-Element-Content)])
  #:transparent
  #:type-name XML-Element-Content)

(struct xml-dtd
  ([location : (U String Symbol)]
   [declarations : (Listof XML-Type-Declaration*)]
   [type : (Option XML-Type)])
  #:transparent
  #:type-name XML-DTD)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type XML-Type-Entities (Immutable-HashTable (U Symbol Keyword) XML-Entity))
(define-type XML-Type-Notations (Immutable-HashTable Symbol XML-Notation))
(define-type XML-Type-Elements (Immutable-HashTable Symbol XML-Element-Content))
(define-type XML-Type-Attributes (Immutable-HashTable Symbol (Immutable-HashTable Symbol XML-Attribute)))

(struct xml-type
  ([entities : XML-Type-Entities]
   [notations : XML-Type-Notations]
   [elements : XML-Type-Elements]
   [attributes : XML-Type-Attributes])
  #:transparent
  #:type-name XML-Type)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define read-xml-type-definition : (->* (SGML-StdIn) ((U False String Symbol)) XML-DTD)
  (lambda [/dev/rawin [port-name #false]]
    (define-values (source definitions) (xml-dtd-read-definition /dev/rawin port-name))
    
    (xml-make-type-definition source definitions #true)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-make-type-definition : (->* ((U String Symbol) (Listof XML-Definition*)) (Boolean) XML-DTD)
  (lambda [source subset [extsubset? #false]]
    (xml-dtd source (xml-dtd-definitions->declarations subset extsubset?) #false)))

(define xml-dtd-definitions->declarations : (-> (Listof XML-Definition*) Boolean (Listof XML-Type-Declaration*))
  (lambda [subset extsubset?]
    (let filter-definition : (Listof XML-Type-Declaration*) ([rest : (Listof XML-Definition*) subset]
                                                             [snoitaralced : (Listof XML-Type-Declaration*) null])
        (cond [(null? rest) (reverse snoitaralced)]
              [else (let-values ([(self rest++) (values (car rest) (cdr rest))])
                      (cond [(vector? self)
                             (let ([DECL (vector-ref self 0)])
                               (case (xml:name-datum DECL)
                                 [(ENTITY)
                                  (let ([e (xml-dtd-extract-entity* DECL (vector-ref self 1))])
                                    (filter-definition rest++ (if (xml-entity? e) (cons e snoitaralced) snoitaralced)))]
                                 [(ATTLIST)
                                  (cond [(and extsubset?) (filter-definition rest++ (cons self snoitaralced))]
                                        [else (let ([a (xml-dtd-extract-attributes* DECL (vector-ref self 1))])
                                                (filter-definition rest++ (if (xml-attribute-list? a) (cons a snoitaralced) snoitaralced)))])]
                                 [(ELEMENT)
                                  (cond [(and extsubset?) (filter-definition rest++ (cons self snoitaralced))]
                                        [else (let ([e (xml-dtd-extract-element* DECL (vector-ref self 1))])
                                                (filter-definition rest++ (if (xml-element-content? e) (cons e snoitaralced) snoitaralced)))])]
                                 [(NOTATION)
                                  (let ([e (xml-dtd-extract-notation* DECL (vector-ref self 1))])
                                    (filter-definition rest++ (if (xml-notation? e) (cons e snoitaralced) snoitaralced)))]
                                 [else (make+exn:xml:unimplemented DECL) (filter-definition rest++ snoitaralced)]))]
                            [(xml-section? self)
                             (filter-definition rest++ (cons ((inst cons (U XML:Name XML:PEReference) (Listof XML-Type-Declaration*))
                                                              (xml-section-condition self) (filter-definition (xml-section-body self) null))
                                                             snoitaralced))]
                            [(xml:pereference? self) (filter-definition rest++ (cons self snoitaralced))]
                            [(mpair? self) (filter-definition rest++ (cons self snoitaralced))]
                            [else (xml-grammar-throw #false self) (filter-definition rest++ snoitaralced)]))]))))

(define xml-dtd-extract-entity* : (-> XML:Name (Listof XML-Doctype-Body*) (U XML-Entity XML-Syntax-Error Void))
  (lambda [ENTITY body]
    (define tokens : (U (Listof XML-Token) XML-Syntax-Error Void) (xml-dtd-filter-tokens ENTITY body))
    
    (when (list? tokens)
      (cond [(or (null? tokens) (null? (cdr tokens))) (make+exn:xml:malformed tokens ENTITY)]
            [else (let-values ([(?name ?value rest) (values (car tokens) (cadr tokens) (cddr tokens))])
                    (cond [(not (or (xml:reference? ?name) (xml:pereference? ?name))) (make+exn:xml:malformed tokens ENTITY)]
                          [(xml:string? ?value)
                           (cond [(null? rest) (xml-entity ?name ?value #false #false #false)]
                                 [else (make+exn:xml:malformed rest ENTITY)])]
                          [else (let*-values ([(ext) (xml-grammar-extract-external* (cdr tokens))]
                                              [(?public ?system terms) (values (car ext) (cadr ext) (cddr ext))])
                                  (cond [(null? terms) (xml-entity ?name #false ?public ?system #false)]
                                        [(xml:pereference? ?name) (make+exn:xml:malformed terms ENTITY)]
                                        [(or (null? (cdr terms))) (make+exn:xml:malformed terms ENTITY)] 
                                        [else (let-values ([(?ndata ?nname term-rest) (values (car terms) (cadr terms) (cddr terms))])
                                                (cond [(and (xml:name=:=? ?ndata 'NDATA) (xml:name? ?nname) (null? term-rest))
                                                       (xml-entity ?name #false ?public ?system ?nname)]
                                                      [else (make+exn:xml:malformed terms ENTITY)]))]))]))]))))

(define xml-dtd-extract-notation* : (-> XML:Name (Listof XML-Doctype-Body*) (U XML-Notation XML-Syntax-Error Void))
  (lambda [NOTATION body]
    (define tokens : (U (Listof XML-Token) XML-Syntax-Error Void) (xml-dtd-filter-tokens NOTATION body))
    
    (when (list? tokens)
      (cond [(or (null? tokens) (null? (cdr tokens))) (make+exn:xml:malformed tokens NOTATION)]
            [else (let-values ([(?name ?keyword rest) (values (car tokens) (cadr tokens) (cddr tokens))])
                    (cond [(not (xml:name? ?name)) (make+exn:xml:malformed tokens NOTATION)]
                          [else (or (and (pair? rest) (null? (cdr rest))
                                         (xml:name=:=? ?keyword 'PUBLIC)
                                         (let ([?public (car rest)])
                                           (and (xml:string? ?public)
                                                (xml-notation ?name ?public #false))))
                                    (let*-values ([(ext) (xml-grammar-extract-external* (cdr tokens))]
                                                  [(?public ?system) (values (car ext) (cadr ext))])
                                      (cond [(pair? (cddr ext)) (make+exn:xml:malformed (cddr ext) NOTATION)]
                                            [else (xml-notation ?name ?public ?system)])))]))]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-dtd-extract-element* : (-> XML:Name (Listof XML-Doctype-Body*) (U XML-Element-Content XML-Syntax-Error Void))
  (lambda [ELEMENT body]
    (define tokens : (U (Listof XML-Token) XML-Syntax-Error Void) (xml-dtd-filter-tokens ELEMENT body))
    
    (when (list? tokens)
      (cond [(or (null? tokens) (null? (cdr tokens))) (make+exn:xml:malformed tokens ELEMENT)]
            [else (let-values ([(?name rest) (values (car tokens) (cdr tokens))])
                    (cond [(not (xml:name? ?name)) (make+exn:xml:malformed tokens ELEMENT)]
                          [(null? rest) (make+exn:xml:malformed tokens ELEMENT)]
                          [else (let-values ([(?content cbody) (values (car rest) (cdr rest))])
                                  (cond [(xml:delim=:=? ?content #\()
                                         (let-values ([(?c rest++) (xml-dtd-extract-element-content* ?name cbody)])
                                           (cond [(pair? rest++) (make+exn:xml:malformed rest++ ELEMENT)]
                                                 [else (unless (exn:xml? ?c) (xml-element-content ?name ?c))]))]
                                        [(not (xml:name? ?content)) (make+exn:xml:malformed ?content ELEMENT)]
                                        [(pair? cbody) (make+exn:xml:malformed rest ELEMENT)]
                                        [else (xml-element-content ?name ?content)]))]))]))))

(define xml-dtd-extract-element-content* : (-> XML:Name (Listof XML-Token) (Values (U XML-Type-Element-Content XML-Syntax-Error) (Listof XML-Token)))
  (lambda [elem body]
    (cond [(null? body) (values (make+exn:xml:malformed body elem) null)]
          [else (let-values ([(?data rest) (values (car body) (cdr body))])
                  (cond [(xml:name=:=? ?data '|#PCDATA|)
                         (let-values ([(pcdata rest++) (xml-dtd-extract-enumeration* elem rest #false)])
                           (cond [(exn:xml? pcdata) (values pcdata rest++)]
                                 [else (let-values ([(?* rest*) (xml-dtd-extract-element-children-particle* rest++)])
                                         (cond [(eq? ?* #\*) '|it's okay if no names are specified via PEs| (values (box pcdata) rest*)]
                                               [(and (eq? ?* #\1) (null? pcdata)) (values (box null) rest*)]
                                               [else (values (box null) rest++ #| let the caller deal with the malformation |#)]))]))]
                        [else (xml-dtd-extract-element-children* elem body)]))])))

(define xml-dtd-extract-element-children* : (-> XML:Name (Listof XML-Token) (Values (U XML-Type-Element-Children XML-Syntax-Error) (Listof XML-Token)))
  (lambda [elem body]
    (let extract-children ([rest : (Listof XML-Token) body]
                           [nerdlihc : (Listof (U (Pairof Char XML:Name) XML-Type-Element-Children)) null]
                           [sep? : Boolean #true]
                           [sep : (Option Char) #false])
      (cond [(null? body) (values (make+exn:xml:malformed body elem) null)]
            [else (let-values ([(?e rest++) (values (car rest) (cdr rest))])
                    (cond [(xml:name? ?e)
                           (let-values ([(p rest*) (xml-dtd-extract-element-children-particle* rest++)])
                             (when (not sep?) (make+exn:xml:malformed ?e elem))
                             (extract-children rest* (if (not sep?) nerdlihc (cons (cons p ?e) nerdlihc)) #false sep))]
                          [(xml:delim? ?e)
                           (let ([delim (xml:delim-datum ?e)])
                             (cond [(eq? delim #\|)
                                    (when (or sep? (and sep (not (eq? sep #\|)))) (make+exn:xml:malformed ?e elem))
                                    (extract-children rest++ nerdlihc #true (or sep #\|))]
                                   [(eq? delim #\,)
                                    (when (or sep? (and sep (not (eq? sep #\,)))) (make+exn:xml:malformed ?e elem))
                                    (extract-children rest++ nerdlihc #true (or sep #\,))]
                                   [(eq? delim #\))
                                    (let-values ([(p rest*) (xml-dtd-extract-element-children-particle* rest++)]
                                                 [(content) (reverse nerdlihc)])
                                      (values (cond [(eq? sep #\|) (cons p content)]
                                                    [else (cons p (list->vector content))])
                                              rest*))]
                                   [(eq? delim #\()
                                    (let-values ([(?content rest*) (extract-children rest++ null #true #false)])
                                      (extract-children rest* (if (exn:xml? ?content) nerdlihc (cons ?content nerdlihc)) #false sep))]
                                   [else (make+exn:xml:malformed ?e elem) (extract-children rest++ nerdlihc sep? sep)]))]
                          [else (make+exn:xml:malformed ?e elem) (extract-children rest++ nerdlihc sep? sep)]))]))))

(define xml-dtd-extract-element-children-particle* : (-> (Listof XML-Token) (Values Char (Listof XML-Token)))
  (lambda [body]
    (define defchar : Char #\1)

    (cond [(null? body) (values defchar body)]
          [else (let-values ([(?p rest) (values (car body) (cdr body))])
                  (cond [(xml:delim? ?p)
                         (let ([p (xml:delim-datum ?p)])
                           (if (memq p '(#\+ #\* #\?))
                               (values (assert p char?) rest)
                               (values defchar body)))]
                        [else (values defchar body)]))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-dtd-extract-attributes* : (-> XML:Name (Listof XML-Doctype-Body*) (U XML-Attribute-List XML-Syntax-Error Void))
  (lambda [ATTLIST body]
    (define tokens : (U (Listof XML-Token) XML-Syntax-Error Void) (xml-dtd-filter-tokens ATTLIST body))
    
    (when (list? tokens)
      (cond [(or (null? tokens) (null? (cdr tokens))) (make+exn:xml:malformed tokens ATTLIST)]
            [else (let-values ([(?element rest) (values (car tokens) (cdr tokens))])
                    (cond [(not (xml:name? ?element)) (make+exn:xml:malformed tokens ATTLIST)]
                          [else (let extract-attribute ([altokens : (Listof XML-Token) rest]
                                                        [setubirtta : (Listof XML-Attribute) null])
                                  (cond [(null? altokens) (xml-attribute-list ?element (reverse setubirtta))]
                                        [else (let-values ([(?attr albody) (values (car altokens) (cdr altokens))])
                                                (cond [(not (xml:name? ?attr)) (make+exn:xml:malformed altokens ATTLIST)]
                                                      [else (let*-values ([(?t notation? albody++) (xml-dtd-extract-attribute-type* ?attr albody)]
                                                                          [(?v fixed? albody++++) (xml-dtd-extract-attribute-default* ?attr albody++)])
                                                              (extract-attribute albody++++
                                                                                 (cond [(and ?t ?v)
                                                                                        (cons (xml-attribute ?element ?attr ?t ?v notation? fixed?)
                                                                                              setubirtta)]
                                                                                       [else setubirtta])))]))]))]))]))))

(define xml-dtd-extract-attribute-type* : (-> XML:Name (Listof XML-Token) (Values (Option XML-Type-Attribute-Type) Boolean (Listof XML-Token)))
  (lambda [attr body]
    (let extract-type ([rest : (Listof XML-Token) body]
                       [notation? : (Option XML:Name) #false])
      (cond [(null? body) (if (not notation?) (make+exn:xml:malformed attr) (make+exn:xml:malformed notation? attr)) (values #false #false null)]
            [else (let-values ([(?type rest++) (values (car rest) (cdr rest))])
                    (cond [(xml:delim=:=? ?type #\()
                           (let-values ([(enum rest++++) (xml-dtd-extract-enumeration* attr rest++ #true)])
                             (values (and (pair? enum) enum) (and notation? #true) rest++++))]
                          [(and notation?) (make+exn:xml:malformed notation? attr) (values #false #true rest++)]
                          [(xml:name? ?type)
                           (cond [(eq? (xml:name-datum ?type) 'NOTATION) (extract-type rest++ ?type)]
                                 [else (values ?type notation? rest++)])]
                          [else (make+exn:xml:malformed ?type attr) (values #false notation? rest++)]))]))))

(define xml-dtd-extract-attribute-default* : (-> XML:Name (Listof XML-Token) (Values (Option XML-Type-Attribute-Default) Boolean (Listof XML-Token)))
  (lambda [attr body]
    (let extract-default ([rest : (Listof XML-Token) body]
                          [fixed? : (Option XML:Name) #false])
      (cond [(null? body) (if (not fixed?) (make+exn:xml:malformed attr) (make+exn:xml:malformed fixed? attr)) (values #false #false null)]
            [else (let-values ([(?v rest++) (values (car rest) (cdr rest))])
                    (cond [(xml:string? ?v) (values ?v (and fixed? #true) rest++)]
                          [(and fixed?) (make+exn:xml:malformed (list fixed? ?v) attr) (values #false #true rest++)]
                          [(xml:name? ?v)
                           (if (eq? (xml:name-datum ?v) '|#FIXED|)
                               (extract-default rest++ ?v)
                               (values ?v fixed? rest++))]
                          [else (make+exn:xml:malformed ?v attr) (values #false fixed? rest++)]))]))))

(define xml-dtd-extract-enumeration* : (-> XML:Name (Listof XML-Token) Boolean (Values (U (Listof XML:Name) XML-Syntax-Error) (Listof XML-Token)))
  (lambda [attr body bar?]
    (let extract-enum ([rest : (Listof XML-Token) body]
                       [smune : (Listof XML:Name) null]
                       [enums : (Listof Symbol) null]
                       [bar? : Boolean bar?])
      (cond [(null? body) (values (make+exn:xml:malformed attr) null)]
            [else (let-values ([(?e rest++) (values (car rest) (cdr rest))])
                    (cond [(xml:name? ?e)
                           (let ([name (xml:name-datum ?e)])
                             (cond [(not bar?)
                                    (make+exn:xml:malformed ?e attr)
                                    (extract-enum rest++ smune enums #false)]
                                   [(memq name enums)
                                    (make+exn:xml:duplicate ?e attr)
                                    (extract-enum rest++ smune enums #false)]
                                   [else (extract-enum rest++ (cons ?e smune) (cons name enums) #false)]))]
                          [(xml:delim? ?e)
                           (let ([delim (xml:delim-datum ?e)])
                             (unless (not bar?) (make+exn:xml:malformed ?e attr))
                             (cond [(eq? delim #\|) (extract-enum rest++ smune enums #true)]
                                   [(eq? delim #\)) (values (reverse smune) rest++)]
                                   [else (make+exn:xml:malformed ?e attr) (extract-enum rest++ smune enums bar?)]))]
                          [else (make+exn:xml:malformed ?e attr) (extract-enum rest++ smune enums bar?)]))]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-dtd-read-definition : (->* (SGML-StdIn) ((U False String Symbol)) (Values (U Symbol String) (Listof XML-Definition*)))
  (lambda [/dev/rawin [port-name #false]]
    (define /dev/dtdin : Input-Port (dtd-open-input-port /dev/rawin #true port-name))
    (define source : (U Symbol String) (sgml-port-name /dev/dtdin))
    (define tokens : (Listof XML-Token) (read-xml-tokens* /dev/dtdin source))

    (values source (xml-syntax->definition* tokens))))

(define xml-dtd-filter-tokens : (-> XML-Token (Listof XML-Doctype-Body*) (U (Listof XML-Token) XML-Syntax-Error Void))
  (lambda [DECLNAME body]
    (define-values (%tokens others) (partition xml-token? body))
    (define tokens : (Listof XML-Token) (if (and (pair? %tokens) (xml:pe? (car %tokens))) (cdr %tokens) %tokens))

    (cond [(pair? others) (xml-grammar-throw DECLNAME others)]
          [else tokens])))

(define xml-entity-value->datum : (-> XML:String (U String (Boxof String)))
  (lambda [v]
    (cond [(xml:&string? v) (box (xml:string-datum v))]
          [else (xml:string-datum v)])))
