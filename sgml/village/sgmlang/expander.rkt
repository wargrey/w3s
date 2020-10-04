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
(begin-for-syntax
  (define (token-datum->syntax <datum>)
    (define datum (syntax-e <datum>))

    (cond [(and (list? datum) (= (length datum) 7) (keyword? (syntax-e (car datum))))
           (let ([start (syntax-e (list-ref datum 4))])
             (datum->syntax <datum> (syntax-e (list-ref datum 6))
                            <datum>))]
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
    [(_ #:dtd-external-entity datum ...) #'(dtd-external-entity (token-syntax->datum datum) ...)]
    [(_ error extra ...) (raise-syntax-error #false "dtd-syntax->entity"
                                             (token-datum->syntax #'error) #false
                                             (map token-datum->syntax (syntax->list #'(extra ...))))]))

(define-syntax (dtd-syntax->notation stx)
  (syntax-case stx []
    [(_ #:dtd-notation datum ...) #'(dtd-notation (token-syntax->datum datum) ...)]
    [(_ error extra ...) (raise-syntax-error #false "dtd-syntax->notation"
                                             (token-datum->syntax #'error) #false
                                             (map token-datum->syntax (syntax->list #'(extra ...))))]))

(define-syntax (dtd-syntax->element stx)
  (syntax-case stx []
    [(_ #:dtd-empty-element datum ...) #'(dtd-empty-element (token-syntax->datum datum) ...)]
    [(_ #:dtd-element+children datum ...) #'(dtd-element+children (token-syntax->datum datum) ...)]
    [(_ #:dtd-mixed-element datum ...) #'(dtd-mixed-element (token-syntax->datum datum) ...)]
    [(_ #:dtd-element datum ...) #'(dtd-element (token-syntax->datum datum) ...)]
    [(_ error extra ...) (raise-syntax-error #false "dtd-syntax->element"
                                             (token-datum->syntax #'error) #false
                                             (map token-datum->syntax (syntax->list #'(extra ...))))]))

(define-syntax (dtd-syntax->attribute-type stx)
  (syntax-case stx []
    [(_ (#:dtd-attribute-token-type datum ...)) #'(dtd-attribute-token-type (token-syntax->datum datum) ...)]
    [(_ (#:dtd-attribute-enum-type [options ...] flag)) #'(dtd-attribute-enum-type (list (token-syntax->datum options) ...) flag)]
    [(_ (#:dtd-attribute-string-type placeholder ...)) #'dtd:attribute:cdata]
    [(_ (error extra ...)) (raise-syntax-error #false "dtd-syntax->attribute-type"
                                               (token-datum->syntax #'error) #false
                                               (map token-datum->syntax (syntax->list #'(extra ...))))]))

(define-syntax (dtd-syntax->attribute stx)
  (syntax-case stx []
    [(_ #:dtd-attribute element name type datum ...)
     #'(dtd-attribute (token-syntax->datum element) (token-syntax->datum name) (dtd-syntax->attribute-type type) (token-syntax->datum datum) ...)]
    [(_ #:dtd-attribute/required element name type datum ...)
     #'(dtd-attribute/required (token-syntax->datum element) (token-syntax->datum name) (dtd-syntax->attribute-type type) (token-syntax->datum datum) ...)]
    [(_ #:dtd-attribute+default element name type datum ...)
     #'(dtd-attribute+default (token-syntax->datum element) (token-syntax->datum name) (dtd-syntax->attribute-type type) (token-syntax->datum datum) ...)]
    [(_ error extra ...) (raise-syntax-error #false "dtd-syntax->attribute"
                                             (token-datum->syntax #'error) #false
                                             (map token-datum->syntax (syntax->list #'(extra ...))))]))

(define-syntax (xml-syntax->dtd stx)
  (syntax-case stx []
    [(_ #:subset [source declaration ...]) #'(xml-dtd source (list (xml-syntax->dtd declaration) ...))]
    [(_ #:subset false) #'#false]
    [(_ (#:entity sexp ...)) #'(dtd-syntax->entity sexp ...)]
    [(_ (#:element sexp ...)) #'(dtd-syntax->element sexp ...)]
    [(_ (#:attribute sexp ...)) #'(dtd-syntax->attribute sexp ...)]
    [(_ (#:notation sexp ...)) #'(dtd-syntax->notation sexp ...)]
    [(_ (#:condsection condition section ...)) #'(cons (token-syntax->datum condition) (list (xml-syntax->dtd section) ...))]
    [(_ #(raw-datum ...)) #'(vector-immutable (token-syntax->datum raw-datum) ...)]
    [(_ token/pi) #'(token-syntax->datum token/pi)]))

(define-syntax (xml-syntax->content stx)
  (syntax-case stx []
    [(_ #:root-element [content ...])
     #'(list (xml-syntax->content content) ...)]
    [(_ [tagname [(attrname . attrvalue) ...] [children ...]])
     #'(list (token-syntax->datum tagname)
             (list (cons (token-syntax->datum attrname) (token-syntax->datum attrvalue)) ...)
             (list (xml-syntax->content children) ...))]
    [(_ pi) #'(token-syntax->datum pi)]))

(define-syntax (xml-syntax->document stx)
  (syntax-case stx []
    [(_ location version encoding standalone? root-name external-id internal-dtd ?external-dtd ?type contents)
     #'(make-xml-document* location version encoding standalone?

                           (token-syntax->datum root-name)
                           (token-syntax->datum external-id)
                           
                           (xml-syntax->dtd #:subset internal-dtd)
                           (xml-syntax->dtd #:subset ?external-dtd)
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
    [(_ false ...) #'#false]))

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
          (let dtd-declaration->sexp : (Listof Any) ([declarations : (Listof DTD-Declaration*) (xml-dtd-declarations doc.dtd)])
            (for/list : (Listof Any) ([d (in-list declarations)])
              (cond [(pair? d) (list* '#:condsection (xml-datum->sexp (car d)) (dtd-declaration->sexp (cdr d)))]
                    [(dtd-entity? d) (list* '#:entity (xml-datum->sexp d))]
                    [(dtd-element? d) (list* '#:element (xml-datum->sexp d))]
                    [(dtd-attribute? d) (list* '#:attribute (xml-datum->sexp d))]
                    [(dtd-notation? d) (list* '#:notation (xml-datum->sexp d))]
                    [else (xml-datum->sexp d)]))))))

(define xml-type->sexp : (-> XML-Type (List (Listof (List* (U Symbol Keyword) Keyword (Listof Any)))
                                            (Listof (List* Symbol Keyword (Listof Any)))
                                            (Listof (List* Symbol Keyword (Listof Any)))
                                            (Listof (List Symbol (Listof (List* Symbol Keyword (Listof Any)))))))
  (lambda [type]
    (list (for/list : (Listof (List* (U Symbol Keyword) Keyword (Listof Any))) ([(name entity) (in-hash (xml-type-entities type))])
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

(define xml-struct->list : (-> Any (List* Keyword (Listof Any)))
  (lambda [t]
    (define typeid (assert (object-name t) symbol?))
    (define datums (map xml-datum->sexp (struct->list t #:on-opaque 'skip)))
    
    (cons (string->keyword (symbol->immutable-string typeid))
          (if (null? datums) (list #false) datums))))
