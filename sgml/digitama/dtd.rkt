#lang typed/racket/base

(provide (all-defined-out))

(require "digicore.rkt")
(require "stdin.rkt")
(require "grammar.rkt")
(require "tokenizer.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type XML-Type-Definition* (U XML-Processing-Instruction* XML-Entity))

(define-type XML-Type-Declaration*
  (Rec defs (U XML-Type-Definition* XML:PEReference
               (Pairof (U XML:Name XML:PEReference) (Listof defs)))))

(struct xml-entity
  ([name : (U XML:Reference XML:PEReference)]
   [value : (Option XML:String)]
   [external : (U False XML:String (Pairof XML:String XML:String))]
   [ndata : (Option XML:Name)])
  #:transparent
  #:type-name XML-Entity)

(struct xml-dtd
  ([location : (U String Symbol)]
   [declarations : (Listof XML-Type-Declaration*)]
   [type : (Option XML-Type)])
  #:transparent
  #:type-name XML-DTD)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type XML-Type-Entities (Immutable-HashTable (U Symbol Keyword) XML-Entity))

(struct xml-type
  ([entities : XML-Type-Entities])
  #:transparent
  #:type-name XML-Type)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define read-xml-type-definition : (-> SGML-StdIn XML-DTD)
  (lambda [/dev/rawin]
    (define /dev/dtdin : Input-Port (dtd-open-input-port /dev/rawin #true))
    (define source : (U Symbol String) (sgml-port-name /dev/dtdin))
    (define tokens : (Listof XML-Token) (read-xml-tokens* /dev/dtdin source))
    (define definitions : (Listof XML-Definition*) (xml-syntax->definition* tokens))
    
    (xml-make-definition source #false definitions)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-make-definition : (-> (U String Symbol) (Option (U XML:Name XML:PEReference)) (Listof XML-Definition*) XML-DTD)
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
                                 [else (make+exn:xml:unimplemented DECL declname) (filter-definition rest++ snoitaralced)]))]
                            [(xml-section? self)
                             (filter-definition rest++ (cons ((inst cons (U XML:Name XML:PEReference) (Listof XML-Type-Declaration*))
                                                              (xml-section-condition self) (filter-definition (xml-section-body self) null))
                                                             snoitaralced))]
                            [(xml:pereference? self) (filter-definition rest++ (cons self snoitaralced))]
                            [(mpair? self) (filter-definition rest++ (cons self snoitaralced))]
                            [else (xml-grammar-throw declname self) (filter-definition rest++ snoitaralced)]))])))

    (xml-dtd source definitions #false)))

(define xml-dtd-extract-entity* : (-> (Option (U XML:Name XML:PEReference)) XML:Name (Listof XML-Doctype-Body*) (U XML-Entity XML-Syntax-Error Void))
  (lambda [declname ENTITY body]
    (define-values (%tokens others) (partition xml-token? body))
    (define tokens : (Listof XML-Token) (if (and (pair? %tokens) (xml:pe? (car %tokens))) (cdr %tokens) %tokens))
    (cond [(pair? others) (xml-grammar-throw ENTITY others)]
          [(or (null? tokens) (null? (cdr tokens))) (make+exn:xml:malformed (cons ENTITY tokens) declname)]
          [else (let-values ([(?name ?value rest) (values (car tokens) (cadr tokens) (cddr tokens))])
                  (cond [(not (and (or (xml:reference? ?name) (xml:pereference? ?name)) (xml:string? ?value)))
                         (make+exn:xml:unimplemented (cons ENTITY tokens) declname)]
                        [(null? rest) (xml-entity ?name ?value #false #false)]
                        [else (xml-entity ?name ?value #false #false)]))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-entity-value->datum : (-> XML:String (U String (Boxof String)))
  (lambda [v]
    (cond [(xml:&string? v) (box (xml:string-datum v))]
          [else (xml:string-datum v)])))
