#lang typed/racket/base

(provide (all-defined-out))

(require digimon/number)

(require "datatype.rkt")
(require "../shared/datatype.rkt")

(require (for-syntax racket/base))
(require (for-syntax syntax/parse))
(require (for-syntax racket/syntax))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct (T) XML::Attr::Type
  ([category-hint : Datum]
   [datum? : (-> Any Boolean : #:+ T)]
   [datum->string : (-> T String)]
   [xexpr->datum : (XML-Attribute-Value->Datum (Option T))])
  #:constructor-name unsafe-make-xml::attr::type
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-for-syntax (xml:attr-type-name <a>)
  (define attrib (syntax-e <a>))
  (format-id <a> "XML::~a" attrib))

(define-syntax (define-xml-attribute-type-datum stx)
  (syntax-parse stx #:literals [:]
    [(_ [T:id AT:id] (~optional (~seq #:as category) #:defaults ([category #'#false]))
        (~alt (~optional (~seq #:λstring λstring) #:defaults ([λstring #'xml:attr-datum->value]))
              (~optional (~seq #:datum? datum?) #:defaults ([datum? #'string?]))
              (~optional (~seq #:λdatum λxexpr) #:defaults ([λxexpr #'xml:attr-value->string])))
        ...)
     (with-syntax* ([attr (xml:attr-type-name #'AT)])
       (syntax/loc stx
         (define attr : (XML::Attr::Type T)
           (unsafe-make-xml::attr::type (or category 'attr)
                                        datum? λstring λxexpr))))]
    [(_ [T:id AT:id] #:for xtype (~optional (~seq #:as category) #:defaults ([category #'#false]))
        (~alt (~optional (~seq #:λstring λstring) #:defaults ([λstring #'xml:attr-datum->value]))
              (~optional (~seq #:datum? datum?) #:defaults ([datum? #'string?])))
        ...)
     (with-syntax* ([λxexpr (format-id #'xtype "xml:attr-value->~a" (syntax-e #'xtype))])
       (syntax/loc stx
         (define-xml-attribute-type-datum [T AT] #:as category
           #:λstring λstring #:datum? datum? #:λdatum λxexpr)))]
    [(_ T:id rest ...) (syntax/loc stx (define-xml-attribute-type-datum [T T] rest ...))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-xml-attribute-type-datum String)
(define-xml-attribute-type-datum Symbol #:for boolean #:datum? symbol?)
(define-xml-attribute-type-datum Keyword #:for keyword #:datum? keyword?)
(define-xml-attribute-type-datum [String Token] #:for token)
(define-xml-attribute-type-datum [XML-Boolean Boolean] #:for boolean #:datum? xml-boolean?)

(define-xml-attribute-type-datum Byte #:for byte #:datum? byte?)
(define-xml-attribute-type-datum Index #:for index #:datum? index?)
(define-xml-attribute-type-datum [Index Hexadecimal] #:for hexadecimal #:datum? index?)
(define-xml-attribute-type-datum Natural #:for natural #:datum? exact-nonnegative-integer?)
(define-xml-attribute-type-datum Integer #:for integer #:datum? exact-integer?)
(define-xml-attribute-type-datum Fixnum #:for fixnum #:datum? fixnum?)
(define-xml-attribute-type-datum Nonnegative-Fixnum #:for nonnegative-fixnum #:datum? nonnegative-fixnum?)
(define-xml-attribute-type-datum Flonum #:for flonum #:datum? flonum?)
(define-xml-attribute-type-datum Nonnegative-Flonum #:for nonnegative-flonum #:datum? nonnegative-flonum?)
