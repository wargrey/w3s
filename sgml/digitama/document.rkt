#lang typed/racket/base

;;; https://www.w3.org/TR/xml11/#sec-documents

(provide (all-defined-out))

(require racket/string)
(require racket/path)
(require racket/vector)

(require "grammar.rkt")
(require "digicore.rkt")
(require "stdin.rkt")

(require "tokenizer/port.rkt")
(require "tokenizer/grammar.rkt")
(require "tokenizer.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct xml-doctype
  ([location : (U String Symbol)]
   [version : (Option Nonnegative-Flonum)]
   [encoding : (Option String)]
   [standalone? : Boolean]
   [name : Symbol]
   [public : (Option String)]
   [system : (Option String)])
  #:transparent
  #:type-name XML-DocType)

(struct xml-document
  ([doctype : XML-DocType]
   [subset : (Listof XML-Datum)]
   [nodes : (Listof XML-Grammar)])
  #:transparent
  #:type-name XML-Document)

(struct xml-document*
  ([doctype : XML-DocType]
   [subset : (Listof XML-Token)]
   [nodes : (Listof XML-Grammar*)])
  #:transparent
  #:type-name XML-Document*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define read-xml-document : (-> XML-StdIn XML-Document)
  (lambda [/dev/rawin]
    (define-values (/dev/xmlin version encoding standalone?) (xml-open-input-port /dev/rawin #false))
    (define tokens : (Listof XML-Datum) (read-xml-tokens /dev/xmlin))

    (xml-document (xml-doctype (xml-port-name /dev/xmlin) version encoding standalone? '|| #false #false)
                  null (xml-syntax->grammar tokens))))

(define read-xml-document* : (-> XML-StdIn XML-Document*)
  (lambda [/dev/rawin]
    (define-values (/dev/xmlin version encoding standalone?) (xml-open-input-port /dev/rawin #true))
    (define tokens : (Listof XML-Token) (read-xml-tokens* /dev/xmlin))

    (xml-document* (xml-doctype (xml-port-name /dev/xmlin) version encoding standalone? '|| #false #false)
                   null (xml-syntax->grammar* tokens))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#;(define read-xml-document*->document : (-> XML-Document* XML-Document)
  (lambda [doc.xml]
    (xml-document (xml-document*-doctype doc.xml)
                  null ;(map xml-token->datum (xml-document*-subset doc.xml))
                  (map xml-grammar->datum (xml-document*-nodes doc.xml)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#;(define xml-declaration->datum : (-> XML-Declaration* XML-Declaration)
  (lambda [d]
    (vector (xml:name-datum (vector-ref d 0))
            (map (λ [[body : (U XML-Token XML-Declaration* XML-Processing-Instruction*)]]
                   (cond [(vector? body) (xml-declaration->datum body)]
                         [(box? body) (xml-pi->datum body)]
                         [else (xml-token->datum body)]))
                 (vector-ref d 1)))))

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

#;(define xml-grammar->datum : (-> XML-Grammar* XML-Grammar)
  (lambda [g]
    (cond [(list? g) (xml-element->datum g)]
          [(box? g) (xml-pi->datum g)]
          [else (xml-declaration->datum g)])))

(define xml-pair->datum : (-> (Pairof XML:Name XML:String) (Pairof Symbol String))
  (lambda [p]
    (cons (xml:name-datum (car p)) (xml:string-datum (cdr p)))))
