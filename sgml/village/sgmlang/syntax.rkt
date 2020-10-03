#lang typed/racket/base

(provide (all-defined-out))

(require typed/racket/unsafe)

(require racket/symbol)

(require sgml/digitama/dtd)
(require sgml/digitama/doctype)
(require sgml/digitama/grammar)
(require sgml/digitama/document)
(require sgml/digitama/digicore)

(require css/digitama/syntax/w3s)

(require (for-syntax racket/base))
(require (for-syntax racket/syntax))

(require (for-syntax sgml/digitama/dtd))
(require (for-syntax sgml/digitama/digicore))

(unsafe-require/typed
 racket/struct
 [struct->list (-> Any [#:on-opaque Symbol] (Listof Any))])

(unsafe-require/typed
 racket/base
 [mpair? (-> Any Boolean : (MPairof Any Any))])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Struct-Datum (List* Keyword (Listof Any)))
(define-type XML-PI-Datum (Boxof (Pairof Struct-Datum (Option Struct-Datum))))

(define-type XML-Name (W3S-Token-Datumof Symbol))
(define-type XML-String (W3S-Token-Datumof String))
(define-type XML-Char (W3S-Token-Datumof Index))
(define-type XML-Reference (W3S-Token-Datumof Symbol))
(define-type XML-PEReference (W3S-Token-Datumof Keyword))

(define-type XML-External-ID (U False XML-String (Pairof XML-String XML-String)))

(define-type XML-Element-Attribute-Value-Datum (U XML-String XML-Name (Listof XML-Name)))
(define-type XML-Element-Attribute-Datum (Pairof XML-Name XML-Element-Attribute-Value-Datum))
(define-type XML-Subdatum-Datum (U XML-String XML-Char XML-Reference XML-PI-Datum))
(define-type XML-Element-Datum (Rec elem (List XML-Name (Listof XML-Element-Attribute-Datum) (Listof (U elem XML-Subdatum-Datum)))))

(define-type XML-Content-Datum (U XML-PI-Datum XML-Element-Datum))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(begin-for-syntax
  (define (token-datum->syntax <datum>)
    (define datum (syntax-e <datum>))

    (cond [(and (list? datum) (= (length datum) 7) (keyword? (syntax-e (car datum))))
           (let ([start (syntax-e (list-ref datum 4))])
             (datum->syntax #false (syntax-e (last datum))
                            (list (syntax-e (list-ref datum 1))
                                  (syntax-e (list-ref datum 2)) (syntax-e (list-ref datum 3))
                                  start (- (syntax-e (list-ref datum 5)) start))))]
          [else <datum>])))

(define-syntax (token-syntax->datum stx)
  (syntax-case stx []
    [(_ (#:xml:pereference datum ...)) #'(xml:pereference 'datum ...)]
    [(_ (#:xml:reference datum ...)) #'(xml:reference 'datum ...)]
    [(_ (#:xml:name datum ...)) #'(xml:name 'datum ...)]
    [(_ (#:xml:&string datum ...)) #'(xml:&string 'datum ...)]
    [(_ (#:xml:string datum ...)) #'(xml:string 'datum ...)]
    [(_ (#:xml:newline datum ...)) #'(xml:newline 'datum ...)]
    [(_ (#:xml:comment datum ...)) #'(xml:comment 'datum ...)]
    [(_ (#:xml:whitespace datum ...)) #'(xml:whitespace 'datum ...)]
    [(_ (#:xml:delim datum ...)) #'(xml:delim 'datum ...)]
    [(_ (#:xml:char datum ...)) #'(xml:char 'datum ...)]
    [(_ (dcar . dcdr)) #'(cons (token-syntax->datum dcar) (token-syntax->datum dcdr))]
    [(_ (option ...)) #'(list (token-syntax->datum option) ...)]
    [(_ #(seq ...)) #'(vector-immutable (token-syntax->datum seq) ...)]
    [(_ #&(pi-name . pi-body)) #'(mcons (token-syntax->datum pi-name) (token-syntax->datum pi-body))]
    [(_ datum) #''datum]))

(define-syntax (dtd-syntax->entity stx)
  (syntax-case stx []
    [(_ #:dtd-token-entity datum ...) #'(dtd-token-entity (token-syntax->datum datum) ... null)]
    [(_ #:dtd-unparsed-entity datum ...) #'(dtd-unparsed-entity (token-syntax->datum datum) ...)]
    [(_ #:dtd-external-entity datum ...) #'(dtd-external-entity (token-syntax->datum datum) ...)]))

(define-syntax (dtd-syntax->notation stx)
  (syntax-case stx []
    [(_ #:dtd-notation datum ...) #'(dtd-notation (token-syntax->datum datum) ...)]))

(define-syntax (dtd-syntax->element stx)
  (syntax-case stx []
    [(_ #:dtd-empty-element datum ...) #'(dtd-empty-element (token-syntax->datum datum) ...)]
    [(_ #:dtd-element+children datum ...) #'(dtd-element+children (token-syntax->datum datum) ...)]
    [(_ #:dtd-mixed-element datum ...) #'(dtd-mixed-element (token-syntax->datum datum) ...)]
    [(_ #:dtd-element datum ...) #'(dtd-element (token-syntax->datum datum) ...)]))

(define-syntax (dtd-syntax->attribute-type stx)
  (syntax-case stx []
    [(_ #:dtd-attribute-token-type datum ...) #'(dtd-attribute-token-type (token-syntax->datum datum) ...)]
    [(_ #:dtd-attribute-enum-type datum ...) #'(dtd-attribute-enum-type (token-syntax->datum datum) ...)]
    [(_ dtd-attribute-string-type ...) #'dtd:attribute:cdata]))

(define-syntax (dtd-syntax->attribute stx)
  (syntax-case stx []
    [(_ #:dtd-attribute element name type datum ...)
     #'(dtd-attribute (token-syntax->datum element) (token-syntax->datum name) (dtd-syntax->attribute-type type) (token-syntax->datum datum) ...)]
    [(_ #:dtd-attribute/required element name type datum ...)
     #'(dtd-attribute/required (token-syntax->datum element) (token-syntax->datum name) (dtd-syntax->attribute-type type) (token-syntax->datum datum) ...)]
    [(_ #:dtd-attribute+default element name type datum ...)
     #'(dtd-attribute+default (token-syntax->datum element) (token-syntax->datum name) (dtd-syntax->attribute-type type) (token-syntax->datum datum) ...)]))

(define-syntax (xml-syntax->dtd stx)
  (syntax-case stx []
    [(_ #:root-element [declaration ...])
     #'#false]))

(define-syntax (xml-syntax->content stx)
  (syntax-case stx []
    [(_ #:root-element [content ...])
     #'(list (xml-syntax->content content) ...)]
    [(_ [tagname [(attrname . attrvalue) ...] [children ...]])
     #'(list (token-syntax->datum tagname)
             (list (cons (token-syntax->datum attrname) (token-syntax->datum attrvalue)) ...)
             (list (xml-syntax->content children) ...))]
    [(_ pi)
     #'(token-syntax->datum pi)]))

(define-syntax (xml-syntax->document stx)
  (syntax-case stx []
    [(_ location version encoding standalone? root-name external-id internal-dtd ?external-dtd ?type contents)
     #'(make-xml-document* location version encoding standalone?

                           (token-syntax->datum root-name)
                           (token-syntax->datum external-id)
                           
                           (xml-syntax->dtd #:root-element internal-dtd)
                           (xml-syntax->dtd #:root-element ?external-dtd)
                           (xml-syntax->type ?type)
                           
                           (xml-syntax->content #:root-element contents))]))

(define-syntax (xml-syntax->type stx)
  (syntax-case stx []
    [(_ [[[ent-name ent-id ent-datum ...] ...]
         [[not-name not-id not-datum ...] ...]
         [[ele-name ele-id ele-datum ...] ...]
         [[target-element [[att-name att-id att-datum ...] ...]] ...]])
     #'(xml-type (make-immutable-hasheq (list (cons 'ent-name (dtd-syntax->entity ent-id ent-datum ...)) ...))
                 (make-immutable-hasheq (list (cons 'not-name (dtd-syntax->notation not-id not-datum ...)) ...))
                 (make-immutable-hasheq (list (cons 'ele-name (dtd-syntax->element ele-id ele-datum ...)) ...))
                 (make-immutable-hasheq (list (cons 'target-element
                                                    (make-immutable-hasheq
                                                     (list (cons 'att-name
                                                                 (dtd-syntax->attribute
                                                                  att-id att-datum ...)) ...))) ...)))]
    [(_ nothing ...)
     #'#false]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-document->sexp : (-> XML-Document* (Values W3S-Token-Source (Option Nonnegative-Flonum) (Option String) Boolean Any Any (Listof Any)))
  (lambda [doc.xml]
    (define prolog (xml-document*-prolog doc.xml))
    (define doctype (xml-document*-doctype doc.xml))
    (define root-name (xml-doctype*-name doctype))
    (define extid (xml-doctype*-external doctype))

    (values (xml-prolog-location prolog)
            (xml-prolog-version prolog) (xml-prolog-encoding prolog)
            (xml-prolog-standalone? prolog)

            (xml-datum->sexp root-name)
            (xml-datum->sexp extid)

            (map xml-datum->sexp (xml-document*-contents doc.xml)))))

(define xml-dtd->sexp : (-> XML-DTD (Pairof W3S-Token-Source (Listof Any)))
  (lambda [doc.dtd]
    (cons (xml-dtd-location doc.dtd)
          (map xml-datum->sexp (xml-dtd-declarations doc.dtd)))))

(define xml-type->sexp : (-> XML-Type (List (Listof (List* (U Symbol Keyword) Keyword (Listof Any)))
                                            (Listof (List* Symbol Keyword (Listof Any)))
                                            (Listof (List* Symbol Keyword (Listof Any)))
                                            (Listof (List Symbol (Listof (List* Symbol Keyword (Listof Any)))))))
  (lambda [type]
    (list
     (for/list : (Listof (List* (U Symbol Keyword) Keyword (Listof Any))) ([(name entity) (in-hash (xml-type-entities type))])
       (list* name (xml-struct->list entity)))
     (for/list : (Listof (List* Symbol Keyword (Listof Any))) ([(name notation) (in-hash (xml-type-notations type))])
       (list* name (xml-struct->list notation)))
     (for/list : (Listof (List* Symbol Keyword (Listof Any))) ([(name element) (in-hash (xml-type-elements type))])
       (list* name (xml-struct->list element)))
     (for/list : (Listof (List Symbol (Listof (List* Symbol Keyword (Listof Any))))) ([(element attlist) (in-hash (xml-type-attributes type))])
       (list element (for/list : (Listof (List* Symbol Keyword (Listof Any))) ([(name attribute) (in-hash attlist)])
                       (list* name (xml-struct->list attribute))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-datum->sexp : (-> Any Any)
  (lambda [d]
    (cond [(struct? d) (xml-struct->list d)]
          [(list? d) (map xml-datum->sexp d)]
          [(vector? d) (apply vector-immutable (map xml-datum->sexp (vector->list d)))]
          [(pair? d) (cons (xml-datum->sexp (car d)) (xml-datum->sexp (cdr d)))]
          [(mpair? d) (box (cons (xml-datum->sexp (mcar d)) (xml-datum->sexp (mcdr d))))]
          [else d])))

(define xml-struct->list : (-> Any Struct-Datum)
  (lambda [t]
    (define typeid (assert (object-name t) symbol?))
    (define datums (map xml-datum->sexp (struct->list t #:on-opaque 'skip)))
    
    (cons (string->keyword (symbol->immutable-string typeid))
          datums)))
