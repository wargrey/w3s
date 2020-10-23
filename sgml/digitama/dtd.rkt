#lang typed/racket/base

(provide (all-defined-out))

(require "digicore.rkt")
(require "stdin.rkt")
(require "grammar.rkt")
(require "tokenizer.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type DTD-Attribute-Type (U DTD-Attribute-String-Type DTD-Attribute-Token-Type DTD-Attribute-Enum-Type))

(define-type DTD-Element-Sequence (Immutable-Vectorof (Pairof (U XML:Name DTD-Element-Children) Char)))
(define-type DTD-Element-Choice (Listof (Pairof (U XML:Name DTD-Element-Children) Char)))
(define-type DTD-Element-Children (U DTD-Element-Sequence DTD-Element-Choice))

(define-type DTD-Raw-Declaration* (Immutable-Vector XML:Name (Listof XML-Token)))
(define-type DTD-Definition* (U XML-Processing-Instruction* DTD-Entity DTD-Notation DTD-Element DTD-Attribute DTD-Raw-Declaration*))
(define-type DTD-Declaration* (U DTD-Definition* XML:PEReference DTD-Section))

(struct dtd-section
  ([condition : (U XML:Name XML:PEReference)]
   [body : (Listof DTD-Declaration*)])
  #:transparent
  #:type-name DTD-Section)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct dtd-entity
  ([name : (U XML:Reference XML:PEReference)])
  #:transparent
  #:type-name DTD-Entity)

(struct dtd-internal-entity dtd-entity
  ([value : XML:String])
  #:transparent
  #:type-name DTD-Internal-Entity)

(struct dtd-token-entity dtd-internal-entity
  ([body : (Option (Listof XML-Token))])
  #:mutable
  #:type-name DTD-Token-Entity)

(struct dtd-external-entity dtd-entity
  ([public : (Option XML:String)]
   [system : (Option XML:String)])
  #:transparent
  #:type-name DTD-External-Entity)

(struct dtd-unparsed-entity dtd-external-entity
  ([ndata : XML:Name])
  #:transparent
  #:type-name DTD-Unparsed-Entity)

(struct dtd-notation
  ([name : XML:Name]
   [public : (Option XML:String)]
   [system : (Option XML:String)])
  #:transparent
  #:type-name DTD-Notation)

(struct dtd-attribute-string-type
  ()
  #:transparent
  #:type-name DTD-Attribute-String-Type)

(struct dtd-attribute-token-type
  ([name : XML:Name]
   [names? : Boolean])
  #:transparent
  #:type-name DTD-Attribute-Token-Type)

(struct dtd-attribute-enum-type
  ([options : (Pairof XML:Name (Listof XML:Name))]
   [notation? : Boolean])
  #:transparent
  #:type-name DTD-Attribute-Enum-Type)

(struct dtd-attribute
  ([element : XML:Name]
   [name : XML:Name]
   [type : DTD-Attribute-Type])
  #:transparent
  #:type-name DTD-Attribute)

(struct dtd-attribute/required dtd-attribute
  ()
  #:transparent
  #:type-name DTD-Attribute/Required)

(struct dtd-attribute+default dtd-attribute
  ([value : XML:String]
   [fixed? : Boolean])
  #:transparent
  #:type-name DTD-Attribute+Default)

(struct dtd-element
  ([name : XML:Name])
  #:transparent
  #:type-name DTD-Element)

(struct dtd-empty-element dtd-element
  ()
  #:transparent
  #:type-name DTD-Empty-Element)

(struct dtd-mixed-element dtd-element
  ([children : (Listof XML:Name)])
  #:transparent
  #:type-name DTD-Mixed-Element)

(struct dtd-element+children dtd-element
  ([content : (Pairof DTD-Element-Children Char)])
  #:transparent
  #:type-name DTD-Element+Children)

(struct xml-dtd
  ([location : (U String Symbol)]
   [declarations : (Listof DTD-Declaration*)])
  #:transparent
  #:type-name XML-DTD)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type DTD-Entities (Immutable-HashTable (U Symbol Keyword) DTD-Entity))
(define-type DTD-Notations (Immutable-HashTable Symbol DTD-Notation))
(define-type DTD-Elements (Immutable-HashTable Symbol DTD-Element))
(define-type DTD-Attributes (Immutable-HashTable Symbol (Immutable-HashTable Symbol DTD-Attribute)))

(struct xml-type
  ([entities : DTD-Entities]
   [notations : DTD-Notations]
   [elements : DTD-Elements]
   [attributes : DTD-Attributes])
  #:transparent
  #:type-name XML-Type)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define read-xml-type-definition : (->* (SGML-StdIn) ((U False String Symbol)) XML-DTD)
  (lambda [/dev/rawin [port-name #false]]
    (define /dev/dtdin : Input-Port (dtd-open-input-port /dev/rawin #true port-name))
    (define source : (U Symbol String) (or port-name (sgml-port-name /dev/dtdin)))
    (define tokens : (Listof XML-Token) (read-xml-tokens* /dev/dtdin source))
    
    (xml-make-type-definition source (xml-syntax->definition* tokens))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-make-type-definition : (-> (U String Symbol) (Listof XML-Definition*) XML-DTD)
  (lambda [source subset]
    (xml-dtd source (xml-dtd-definitions->declarations subset))))

(define xml-dtd-definitions->declarations : (-> (Listof XML-Definition*) (Listof DTD-Declaration*))
  (lambda [subset]
    (let filter-definition : (Listof DTD-Declaration*) ([rest : (Listof XML-Definition*) subset]
                                                        [snoitaralced : (Listof DTD-Declaration*) null])
        (cond [(null? rest) (reverse snoitaralced)]
              [else (let-values ([(self rest++) (values (car rest) (cdr rest))])
                      (cond [(vector? self)
                             (let ([DECL (vector-ref self 0)])
                               (case (xml:name-datum DECL)
                                 [(ENTITY)
                                  (let ([e (xml-dtd-extract-entity* DECL (vector-ref self 1))])
                                    (filter-definition rest++ (if (dtd-entity? e) (cons e snoitaralced) snoitaralced)))]
                                 [(ATTLIST)
                                  (let ([a (xml-dtd-extract-attributes* DECL (vector-ref self 1))])
                                    (cond [(list? a) (filter-definition rest++ (append a snoitaralced))]
                                          [(vector? a) (filter-definition rest++ (cons a snoitaralced))]
                                          [else (filter-definition rest++ snoitaralced)]))]
                                 [(ELEMENT)
                                  (let ([e (xml-dtd-extract-element* DECL (vector-ref self 1))])
                                    (cond [(dtd-element? e) (filter-definition rest++ (cons e snoitaralced))]
                                          [(vector? e) (filter-definition rest++ (cons e snoitaralced))]
                                          [else (filter-definition rest++ snoitaralced)]))]
                                 [(NOTATION)
                                  (let ([e (xml-dtd-extract-notation* DECL (vector-ref self 1))])
                                    (filter-definition rest++ (if (dtd-notation? e) (cons e snoitaralced) snoitaralced)))]
                                 [else (make+exn:xml:unrecognized DECL) (filter-definition rest++ snoitaralced)]))]
                            [(xml-section? self)
                             (filter-definition rest++ (cons (dtd-section (xml-section-condition self)
                                                                          (filter-definition (xml-section-body self) null))
                                                             snoitaralced))]
                            [(xml:pereference? self) (filter-definition rest++ (cons self snoitaralced))]
                            [(mpair? self) (filter-definition rest++ (cons self snoitaralced))]
                            [else (xml-grammar-throw #false self) (filter-definition rest++ snoitaralced)]))]))))

(define xml-dtd-extract-entity* : (-> XML:Name (Listof XML-Doctype-Body*) (U DTD-Entity XML-Syntax-Error Void))
  (lambda [ENTITY body]
    (define tokens : (U (Listof XML-Token) XML-Syntax-Error Void) (xml-dtd-filter-tokens ENTITY body))
    
    (when (list? tokens)
      (cond [(or (null? tokens) (null? (cdr tokens))) (make+exn:xml:malformed tokens ENTITY)]
            [else (let-values ([(?name ?value rest) (values (car tokens) (cadr tokens) (cddr tokens))])
                    (cond [(not (or (xml:reference? ?name) (xml:pereference? ?name))) (make+exn:xml:malformed tokens ENTITY)]
                          [(xml:string? ?value)
                           (cond [(null? rest) (dtd-token-entity ?name ?value #false)]
                                 [else (make+exn:xml:malformed rest ENTITY)])]
                          [else (let*-values ([(ext) (xml-grammar-extract-external* (cdr tokens))]
                                              [(?public ?system terms) (values (car ext) (cadr ext) (cddr ext))])
                                  (cond [(null? terms) (dtd-external-entity ?name ?public ?system)]
                                        [(xml:pereference? ?name) (make+exn:xml:malformed terms ENTITY)]
                                        [(or (null? (cdr terms))) (make+exn:xml:malformed terms ENTITY)] 
                                        [else (let-values ([(?ndata ?nname term-rest) (values (car terms) (cadr terms) (cddr terms))])
                                                (if (and (xml:name=:=? ?ndata 'NDATA) (xml:name? ?nname) (null? term-rest))
                                                    (dtd-unparsed-entity ?name ?public ?system ?nname)
                                                    (make+exn:xml:malformed terms ENTITY)))]))]))]))))

(define xml-dtd-extract-notation* : (-> XML:Name (Listof XML-Doctype-Body*) (U DTD-Notation XML-Syntax-Error Void))
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
                                                (dtd-notation ?name ?public #false))))
                                    (let*-values ([(ext) (xml-grammar-extract-external* (cdr tokens))]
                                                  [(?public ?system) (values (car ext) (cadr ext))])
                                      (cond [(pair? (cddr ext)) (make+exn:xml:malformed (cddr ext) NOTATION)]
                                            [else (dtd-notation ?name ?public ?system)])))]))]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-dtd-extract-element* : (-> XML:Name (Listof XML-Doctype-Body*) (U DTD-Element XML-Syntax-Error DTD-Raw-Declaration* Void))
  (lambda [ELEMENT body]
    (define tokens : (U (Listof XML-Token) XML-Syntax-Error Void) (xml-dtd-filter-tokens ELEMENT body))
    
    (when (list? tokens)
      (cond [(or (null? tokens) (null? (cdr tokens))) (make+exn:xml:malformed tokens ELEMENT)]
            [(ormap xml:pereference? tokens) (vector-immutable ELEMENT tokens)]
            [else (let-values ([(?name rest) (values (car tokens) (cdr tokens))])
                    (cond [(not (xml:name? ?name)) (make+exn:xml:malformed tokens ELEMENT)]
                          [(null? rest) (make+exn:xml:malformed tokens ELEMENT)]
                          [else (let-values ([(?content cbody) (values (car rest) (cdr rest))])
                                  (cond [(xml:delim=:=? ?content #\()
                                         (let-values ([(?c rest++) (xml-dtd-extract-element-content* ?name cbody)])
                                           (cond [(pair? rest++) (make+exn:xml:malformed rest++ ELEMENT)]
                                                 [(box? ?c) (dtd-mixed-element ?name (unbox ?c))]
                                                 [(pair? ?c) (dtd-element+children ?name ?c)]
                                                 [else ?c]))]
                                        [(not (xml:name? ?content)) (make+exn:xml:unrecognized ?content ELEMENT)]
                                        [(pair? cbody) (make+exn:xml:malformed rest ELEMENT)]
                                        [else (case (xml:name-datum ?content)
                                                [(EMPTY) (dtd-empty-element ?name)]
                                                [(ANY) (dtd-element ?name)]
                                                [else (make+exn:xml:enum ?content ELEMENT)])]))]))]))))

(define xml-dtd-extract-element-content* : (-> XML:Name (Listof XML-Token)
                                               (Values (U (Boxof (Listof XML:Name)) (Pairof DTD-Element-Children Char) XML-Syntax-Error)
                                                       (Listof XML-Token)))
  (lambda [elem body]
    (cond [(null? body) (values (make+exn:xml:malformed body elem) null)]
          [else (let-values ([(?data rest) (values (car body) (cdr body))])
                  (if (xml:name=:=? ?data '|#PCDATA|)
                      (let-values ([(pcdata rest++) (xml-dtd-extract-enumeration* elem rest #false)])
                        (cond [(exn:xml? pcdata) (values pcdata rest++)]
                              [else (let-values ([(?* rest*) (xml-dtd-extract-element-children-particle* rest++)])
                                      (cond [(eq? ?* #\*) '|it's okay if no names are specified via PEs| (values (box pcdata) rest*)]
                                            [(and (eq? ?* #\1) (null? pcdata)) (values (box null) rest*)]
                                            [else (values (box null) rest++ #| let the caller deal with the malformation |#)]))]))
                      (xml-dtd-extract-element-children* elem body)))])))

(define xml-dtd-extract-element-children* : (-> XML:Name (Listof XML-Token) (Values (U (Pairof DTD-Element-Children Char) XML-Syntax-Error) (Listof XML-Token)))
  (lambda [elem body]
    (let extract-children ([rest : (Listof XML-Token) body]
                           [nerdlihc : (Listof (Pairof (U XML:Name DTD-Element-Children) Char)) null]
                           [sep? : Boolean #true]
                           [sep : (Option Char) #false])
      (cond [(null? body) (values (make+exn:xml:malformed body elem) null)]
            [else (let-values ([(?e rest++) (values (car rest) (cdr rest))])
                    (cond [(xml:name? ?e)
                           (let-values ([(p rest*) (xml-dtd-extract-element-children-particle* rest++)])
                             (when (not sep?) (make+exn:xml:malformed ?e elem))
                             (extract-children rest* (if (not sep?) nerdlihc (cons (cons ?e p) nerdlihc)) #false sep))]
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
                                      (values (cond [(eq? sep #\|) (cons content p)]
                                                    [else (cons (apply vector-immutable content) p)])
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
(define xml-dtd-extract-attributes* : (-> XML:Name (Listof XML-Doctype-Body*) (U (Listof DTD-Attribute) XML-Syntax-Error DTD-Raw-Declaration* Void))
  (lambda [ATTLIST body]
    (define tokens : (U (Listof XML-Token) XML-Syntax-Error Void) (xml-dtd-filter-tokens ATTLIST body))
    
    (when (list? tokens)
      (cond [(or (null? tokens) (null? (cdr tokens))) (make+exn:xml:malformed tokens ATTLIST)]
            [(ormap xml:pereference? tokens) (vector-immutable ATTLIST tokens)]
            [else (let-values ([(?element rest) (values (car tokens) (cdr tokens))])
                    (if (xml:name? ?element)
                        (let extract-attribute ([altokens : (Listof XML-Token) rest]
                                                [setubirtta : (Listof DTD-Attribute) null])
                          (cond [(null? altokens) setubirtta]
                                [else (let-values ([(?attr albody) (values (car altokens) (cdr altokens))])
                                        (if (xml:name? ?attr)
                                            (let*-values ([(?t albody++) (xml-dtd-extract-attribute-type* ?attr albody)]
                                                          [(?v fixed? albody++++) (xml-dtd-extract-attribute-default* ?attr albody++)])
                                              (define ?attrobj : (U DTD-Attribute XML-Syntax-Error False)
                                                (and ?t ?v
                                                     (or (and (xml:string? ?v) (dtd-attribute+default ?element ?attr ?t ?v fixed?))
                                                         (case (xml:name-datum ?v)
                                                           [(|#REQUIRED|) (dtd-attribute/required ?element ?attr ?t)]
                                                           [(|#IMPLIED|) (dtd-attribute ?element ?attr ?t)]
                                                           [else (make+exn:xml:enum ?v ATTLIST)]))))
                                              (extract-attribute albody++++ (if (dtd-attribute? ?attrobj) (cons ?attrobj setubirtta) setubirtta)))
                                            (make+exn:xml:malformed altokens ATTLIST)))]))
                        (make+exn:xml:malformed tokens ATTLIST)))]))))

(define xml-dtd-extract-attribute-type* : (-> XML:Name (Listof XML-Token) (Values (Option DTD-Attribute-Type) (Listof XML-Token)))
  (lambda [attr body]
    (let extract-type ([rest : (Listof XML-Token) body]
                       [notation? : (Option XML:Name) #false])
      (cond [(null? body) (if (not notation?) (make+exn:xml:malformed attr) (make+exn:xml:malformed notation? attr)) (values #false null)]
            [else (let-values ([(?type rest++) (values (car rest) (cdr rest))])
                    (cond [(xml:delim=:=? ?type #\()
                           (let-values ([(enum rest++++) (xml-dtd-extract-enumeration* attr rest++ #true)])
                             (values (and (pair? enum)
                                          (dtd-attribute-enum-type enum (and notation? #true)))
                                     rest++++))]
                          [(and notation?) (make+exn:xml:malformed notation? attr) (values #false rest++)]
                          [(xml:name? ?type)
                           (case (xml:name-datum ?type)
                             [(NOTATION) (extract-type rest++ ?type)]
                             [(CDATA) (values dtd:attribute:cdata rest++)]
                             [(ID IDREF ENTITY NMTOKEN) (values (dtd-attribute-token-type ?type #false) rest++)]
                             [(IDREFS ENTITIES NMTOKENS) (values (dtd-attribute-token-type ?type #true) rest++)]
                             [else (make+exn:xml:enum ?type attr) (values #false rest++)])]
                          [else (make+exn:xml:malformed ?type attr) (values #false rest++)]))]))))

(define xml-dtd-extract-attribute-default* : (-> XML:Name (Listof XML-Token) (Values (U False XML:Name XML:String) Boolean (Listof XML-Token)))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dtd:attribute:cdata : DTD-Attribute-String-Type (dtd-attribute-string-type))
