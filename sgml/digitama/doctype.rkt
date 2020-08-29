#lang typed/racket/base

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type XML-External-ID (U False String (Pairof String String)))
(define-type XML-DocType-Metadata (Pairof Symbol (U False String (Pairof String String))))
(define-type XML-Entities (HashTable Symbol (U String (Boxof String))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct xml-dtd
  ([entities : XML-Entities])
  #:transparent
  #:type-name XML-DTD)

(define make-xml-dtd : (-> XML-DTD)
  (lambda []
    (xml-dtd (make-hasheq))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct xml-doctype
  ([location : (U String Symbol)]
   [version : (Option Nonnegative-Flonum)]
   [encoding : (Option String)]
   [standalone? : Boolean]
   [name : Symbol]
   [external : XML-External-ID]
   [internal-dtd : XML-DTD])
  #:transparent
  #:type-name XML-DocType)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-doctype-metadata : (-> (Option Symbol) (Option String) (Option String) (Option XML-DocType-Metadata))
  (lambda [name public system]
    (and name
         (cons name
               (cond [(and public system) (cons public system)]
                     [else system])))))

(define xml-doctype-values : (-> (Option XML-DocType-Metadata) (Values (Option Symbol) XML-External-ID))
  (lambda [dt]
    (cond [(not dt) (values #false #false)]
          [else (values (car dt) (cdr dt))])))
