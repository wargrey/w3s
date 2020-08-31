#lang typed/racket/base

(provide (all-defined-out))

(require "doctype.rkt")
(require "digicore.rkt")
(require "stdin.rkt")
(require "grammar.rkt")
(require "tokenizer.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct xml-dtd
  ([location : (U String Symbol)]
   [definitions : (Listof XML-Definition*)])
  #:transparent
  #:type-name XML-DTD)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define read-xml-type-definition : (-> SGML-StdIn XML-DTD)
  (lambda [/dev/rawin]
    (define /dev/dtdin : Input-Port (dtd-open-input-port /dev/rawin #true))
    (define source : (U Symbol String) (sgml-port-name /dev/dtdin))
    (define tokens : (Listof XML-Token) (read-xml-tokens* /dev/dtdin source))
    (define definitions : (Listof XML-Definition*) (xml-syntax->definition* tokens))
    
    (xml-make-definition source #false definitions)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-make-definition : (-> (U String Symbol) (Option XML:Name) (Listof XML-Definition*) XML-DTD)
  (lambda [source declname subset]
    #;(let extract-definition ([rest : (Listof XML-Definition*) subset])
      (cond [(null? rest) (make+exn:xml:malformed declname) sIP]
            [else (let-values ([(self rest++) (values (car rest) (cdr rest))])
                    (cond [(xml:delim=:=? self #\]) (when (pair? rest++) (xml-grammar-throw declname rest++)) sIP]
                          [(vector? self)
                           (let ([DECL (vector-ref self 0)])
                             (case (xml:name-datum DECL)
                               [(ENTITY) (xml-grammar-extract-entity* declname DECL (vector-ref self 1) dtd)]
                               [else (make+exn:xml:unimplemented DECL declname)])
                             (extract-entity rest++ sIP))]
                          [(box? self) (extract-entity rest++ (cons self sIP))]
                          [(xml:entity? self) (make+exn:xml:unimplemented self declname) (extract-entity rest++ sIP)]
                          [(xml-section? self) (make+exn:xml:unimplemented self declname) (extract-entity rest++ sIP)]
                          [else (xml-grammar-throw declname self) (extract-entity rest++ sIP)]))]))
    (xml-dtd source subset)))

(define xml-dtd-extract-entity* : (-> XML:Name XML:Name (Listof XML-Doctype-Body*) XML-DTD Void)
  (lambda [declname ENTITY body dtd]
    (define-values (tokens others) (partition xml-token? body))
    (cond [(pair? others) (xml-grammar-throw ENTITY others)]
          [(or (null? tokens) (null? (cdr tokens))) (make+exn:xml:malformed (cons ENTITY tokens) declname)]
          [else (let-values ([(?name ?value rest) (values (car tokens) (cadr tokens) (cddr tokens))])
                  (if (and (xml:name? ?name) (xml:string? ?value) (null? rest))
                      #;(hash-set! (xml-dtd-entities dtd) (xml:name-datum ?name) (xml-entity-value->datum ?value))
                      (void)
                      (make+exn:xml:unimplemented (cons ENTITY tokens) declname)))])
    (void)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-entity-value->datum : (-> XML:String (U String (Boxof String)))
  (lambda [v]
    (cond [(xml:&string? v) (box (xml:string-datum v))]
          [else (xml:string-datum v)])))
