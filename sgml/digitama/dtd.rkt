#lang typed/racket/base

(provide (all-defined-out))

(require "digicore.rkt")
(require "stdin.rkt")
(require "grammar.rkt")
(require "tokenizer.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type XML-Type-Definition*
  (U XML-Processing-Instruction* XML-Entity XML-Notation
     (MPairof XML:Name (Listof XML-Attribute))))

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
  ()
  #:transparent
  #:type-name XML-Attribute)

(struct xml-dtd
  ([location : (U String Symbol)]
   [declarations : (Listof XML-Type-Declaration*)]
   [type : (Option XML-Type)])
  #:transparent
  #:type-name XML-DTD)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type XML-Type-Entities (Immutable-HashTable (U Symbol Keyword) XML-Entity))
(define-type XML-Type-Notations (Immutable-HashTable Symbol XML-Notation))
(define-type XML-Type-Attributes (Immutable-HashTable Symbol (Listof XML-Attribute)))

(struct xml-type
  ([entities : XML-Type-Entities]
   [notations : XML-Type-Notations]
   [attributes : XML-Type-Attributes])
  #:transparent
  #:type-name XML-Type)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define read-xml-type-definition : (->* (SGML-StdIn) ((U False String Symbol)) XML-DTD)
  (lambda [/dev/rawin [port-name #false]]
    (define /dev/dtdin : Input-Port (dtd-open-input-port /dev/rawin #true port-name))
    (define source : (U Symbol String) (sgml-port-name /dev/dtdin))
    (define tokens : (Listof XML-Token) (read-xml-tokens* /dev/dtdin source))
    (define definitions : (Listof XML-Definition*) (xml-syntax->definition* tokens))
    
    (xml-make-definition source #false definitions)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-make-definition : (-> (U String Symbol) (Option XML-Token) (Listof XML-Definition*) XML-DTD)
  (lambda [source declname subset]
    (define definitions
      (let filter-definition : (Listof XML-Type-Declaration*) ([rest : (Listof XML-Definition*) subset]
                                                               [snoitaralced : (Listof XML-Type-Declaration*) null])
        (cond [(null? rest) (reverse snoitaralced)]
              [else (let-values ([(self rest++) (values (car rest) (cdr rest))])
                      (cond [(vector? self)
                             (let ([DECL (vector-ref self 0)])
                               (case (xml:name-datum DECL)
                                 [(ENTITY)
                                  (let ([e (xml-dtd-extract-entity* declname DECL (vector-ref self 1))])
                                    (filter-definition rest++ (if (xml-entity? e) (cons e snoitaralced) snoitaralced)))]
                                 [(NOTATION)
                                  (let ([e (xml-dtd-extract-notation* declname DECL (vector-ref self 1))])
                                    (filter-definition rest++ (if (xml-notation? e) (cons e snoitaralced) snoitaralced)))]
                                 [else (make+exn:xml:unimplemented DECL declname) (filter-definition rest++ snoitaralced)]))]
                            [(xml-section? self)
                             (filter-definition rest++ (cons ((inst cons (U XML:Name XML:PEReference) (Listof XML-Type-Declaration*))
                                                              (xml-section-condition self) (filter-definition (xml-section-body self) null))
                                                             snoitaralced))]
                            [(xml:pereference? self) (filter-definition rest++ (cons self snoitaralced))]
                            [(mpair? self) (filter-definition rest++ (cons self snoitaralced))]
                            [else (xml-grammar-throw declname self) (filter-definition rest++ snoitaralced)]))])))

    (xml-dtd source definitions #false)))

(define xml-dtd-extract-entity* : (-> (Option XML-Token) XML:Name (Listof XML-Doctype-Body*) (U XML-Entity XML-Syntax-Error Void))
  (lambda [declname ENTITY body]
    (define tokens : (U (Listof XML-Token) XML-Syntax-Error Void) (xml-dtd-filter-tokens ENTITY body))
    
    (when (list? tokens)
      (cond [(or (null? tokens) (null? (cdr tokens))) (make+exn:xml:malformed (cons ENTITY tokens) declname)]
            [else (let-values ([(?name ?value rest) (values (car tokens) (cadr tokens) (cddr tokens))])
                    (cond [(not (or (xml:reference? ?name) (xml:pereference? ?name))) (make+exn:xml:malformed (cons ENTITY tokens) declname)]
                          [(xml:string? ?value)
                           (cond [(null? rest) (xml-entity ?name ?value #false #false #false)]
                                 [else (make+exn:xml:malformed (cons ENTITY rest) declname)])]
                          [else (let*-values ([(ext) (xml-grammar-extract-external* (cdr tokens))]
                                              [(?public ?system terms) (values (car ext) (cadr ext) (cddr ext))])
                                  (cond [(null? terms) (xml-entity ?name #false ?public ?system #false)]
                                        [(xml:pereference? ?name) (make+exn:xml:malformed (cons ENTITY terms) declname)]
                                        [(or (null? (cdr terms))) (make+exn:xml:malformed (cons ENTITY terms) declname)] 
                                        [else (let-values ([(?ndata ?nname term-rest) (values (car terms) (cadr terms) (cddr terms))])
                                                (cond [(and (xml:name=:=? ?ndata 'NDATA) (xml:name? ?nname) (null? term-rest))
                                                       (xml-entity ?name #false ?public ?system ?nname)]
                                                      [else (make+exn:xml:malformed (cons ENTITY terms) declname)]))]))]))]))))

(define xml-dtd-extract-notation* : (-> (Option XML-Token) XML:Name (Listof XML-Doctype-Body*) (U XML-Notation XML-Syntax-Error Void))
  (lambda [declname NOTATION body]
    (define tokens : (U (Listof XML-Token) XML-Syntax-Error Void) (xml-dtd-filter-tokens NOTATION body))
    
    (when (list? tokens)
      (cond [(or (null? tokens) (null? (cdr tokens))) (make+exn:xml:malformed (cons NOTATION tokens) declname)]
            [else (let-values ([(?name ?keyword rest) (values (car tokens) (cadr tokens) (cddr tokens))])
                    (cond [(not (xml:name? ?name)) (make+exn:xml:malformed (cons NOTATION tokens) declname)]
                          [else (or (and (pair? rest) (null? (cdr rest))
                                         (xml:name=:=? ?keyword 'PUBLIC)
                                         (let ([?public (car rest)])
                                           (and (xml:string? ?public)
                                                (xml-notation ?name ?public #false))))
                                    (let*-values ([(ext) (xml-grammar-extract-external* (cdr tokens))]
                                                  [(?public ?system) (values (car ext) (cadr ext))])
                                      (cond [(pair? (cddr ext)) (make+exn:xml:malformed (cons NOTATION (cddr ext)) declname)]
                                            [else (xml-notation ?name ?public ?system)])))]))]))))

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
