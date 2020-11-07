#lang typed/racket/base

(provide (all-defined-out))

(require "digicore.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type XML-External-ID (U False String (Pairof String String)))
(define-type XML-Doctype-Metadata (Pairof Symbol XML-External-ID))
(define-type XML-Entities (HashTable Symbol (U String (Boxof String))))

(define-type XML-External-ID* (U False XML:String (Pairof XML:String XML:String)))
(define-type XML-DocType-Metadata* (Pairof XML:Name XML-External-ID*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct xml-prolog
  ([location : (U String Symbol)]
   [version : (Option Nonnegative-Flonum)]
   [encoding : (Option String)]
   [standalone? : Boolean])
  #:transparent
  #:type-name XML-Prolog)

(struct xml-doctype
  ([name : (Option Symbol)]
   [external : XML-External-ID])
  #:transparent
  #:type-name XML-DocType)

(struct xml-doctype*
  ([name : (Option XML:Name)]
   [external : XML-External-ID*])
  #:transparent
  #:type-name XML-DocType*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-doctype-metadata : (-> (Option Symbol) (Option String) (Option String) (Option XML-Doctype-Metadata))
  (lambda [name public system]
    (and name
         (cons name
               (cond [(and public system) (cons public system)]
                     [else system])))))

(define xml-doctype-metadata* : (-> (Option XML:Name) (Option XML:String) (Option XML:String) (Option XML-DocType-Metadata*))
  (lambda [name public system]
    (and name
         (cons name
               (cond [(and public system) (cons public system)]
                     [else system])))))

(define xml-doctype-values : (-> (Option XML-Doctype-Metadata) (Values (Option Symbol) XML-External-ID))
  (lambda [dt]
    (cond [(not dt) (values #false #false)]
          [else (values (car dt) (cdr dt))])))

(define xml-doctype-values* : (-> (Option XML-DocType-Metadata*) (Values (Option XML:Name) XML-External-ID*))
  (lambda [dt]
    (cond [(not dt) (values #false #false)]
          [else (values (car dt) (cdr dt))])))
