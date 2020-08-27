#lang typed/racket/base

;;; https://www.w3.org/TR/xml11/#sec-documents

(provide (all-defined-out))

(require racket/string)
(require racket/path)
(require racket/vector)

(require "doctype.rkt")
(require "grammar.rkt")
(require "digicore.rkt")
(require "stdin.rkt")

(require "tokenizer/port.rkt")
(require "tokenizer/grammar.rkt")
(require "tokenizer.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct xml-document
  ([doctype : XML-DocType]
   [entities : XML-Internal-Entities]
   [elements : (Listof XML-Grammar)])
  #:transparent
  #:type-name XML-Document)

(struct xml-document*
  ([doctype : XML-DocType]
   [entities : XML-Internal-Entities*]
   [elements : (Listof XML-Grammar*)])
  #:transparent
  #:type-name XML-Document*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define read-xml-document : (-> XML-StdIn XML-Document)
  (lambda [/dev/rawin]
    (define-values (/dev/xmlin version encoding standalone?) (xml-open-input-port /dev/rawin #false))
    (define tokens : (Listof XML-Datum) (read-xml-tokens /dev/xmlin))
    (define-values (doctype entities grammars) (xml-syntax->grammar tokens))
    (define-values (maybe-name external) (xml-doctype-values doctype))
    (define name : Symbol
      (cond [(or maybe-name) maybe-name]
            [else (let ([maybe-first-element (findf list? grammars)])
                    (cond [(pair? maybe-first-element) (car maybe-first-element)]
                          [else '||]))]))

    (xml-document (xml-doctype (xml-port-name /dev/xmlin)
                               version encoding standalone?
                               name external)
                  entities grammars)))

(define read-xml-document* : (-> XML-StdIn XML-Document*)
  (lambda [/dev/rawin]
    (define-values (/dev/xmlin version encoding standalone?) (xml-open-input-port /dev/rawin #true))
    (define tokens : (Listof XML-Token) (read-xml-tokens* /dev/xmlin))
    (define-values (doctype entities grammars) (xml-syntax->grammar* tokens))
    (define-values (maybe-name external) (xml-doctype-values doctype))
    (define name : Symbol
      (cond [(or maybe-name) maybe-name]
            [else (let ([maybe-first-element (findf list? grammars)])
                    (cond [(pair? maybe-first-element) (xml:name-datum (car maybe-first-element))]
                          [else '||]))]))

    (xml-document* (xml-doctype (xml-port-name /dev/xmlin)
                                version encoding standalone?
                                name external)
                   entities grammars)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define read-xml-document*->document : (-> XML-Document* XML-Document)
  (lambda [doc.xml]
    (xml-document (xml-document*-doctype doc.xml)
                  (for/hasheq : XML-Internal-Entities ([(key value) (in-hash (xml-document*-entities doc.xml))])
                    (values key (xml-entity-value->datum value)))
                  (map xml-grammar->datum (xml-document*-elements doc.xml)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-grammar->datum : (-> XML-Grammar* XML-Grammar)
  (lambda [g]
    (cond [(list? g) (xml-element->datum g)]
          [else (xml-pi->datum g)])))

(define xml-entity-value->datum : (-> XML:String (U String (Boxof String)))
  (lambda [v]
    (cond [(xml:&string? v) (box (xml:string-datum v))]
          [else (xml:string-datum v)])))

(define xml-pi->datum : (-> XML-Processing-Instruction* XML-Processing-Instruction)
  (lambda [p]
    (box (xml-pair->datum (unbox p)))))

(define xml-element->datum : (-> XML-Element* XML-Element)
  (lambda [e]
    (list (xml:name-datum (car e))
          (map xml-pair->datum (cadr e))
          (map (λ [[child : (U XML-Element-Plain-Children* XML-Element*)]]
                 (cond [(list? child) (xml-element->datum child)]
                       [(xml:string? child) (xml:string-datum child)]
                       [(xml:whitespace? child) (xml-white-space (xml:whitespace-datum child))]
                       [(xml:entity? child) (xml:entity-datum child)]
                       [else (xml-pi->datum child)]))
               (caddr e)))))

(define xml-pair->datum : (-> (Pairof XML:Name XML:String) (Pairof Symbol String))
  (lambda [p]
    (cons (xml:name-datum (car p)) (xml:string-datum (cdr p)))))
