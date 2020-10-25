#lang typed/racket/base

(provide (all-defined-out))

(require racket/string)
(require racket/symbol)

(require typed/racket/unsafe)

(require "dtd.rkt")
(require "digicore.rkt")
(require "grammar.rkt")
(require "normalize.rkt")
(require "tokenizer/characters.rkt")

(unsafe-require/typed
 racket/base
 [(list? xml-element?) (-> Any Boolean : XML-Element*)]
 [(pair? xml-free-id?) (-> Any Boolean : DTD-Free-ID)])

(require (for-syntax racket/base))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type DTD-Free-ID (Pairof (Listof XML:Name) (Option XML:Name)))
(define-type DTD-IDs (Immutable-HashTable Symbol (U True DTD-Free-ID)))
(define-type DTD-AttList (Immutable-HashTable Symbol DTD-Attribute))
(define-type DTD-Required-Attrs (Immutable-HashTable Symbol XML:Name))
(define-type DTD-ID-Attribute (List XML:Name XML:Name))
(define-type DTD-Notation-Attribute (List (List XML:Name XML:Name) XML:Name (Pairof XML:Name (Listof XML:Name))))

(define log-level/not-an-error : Log-Level 'debug)

(define-syntax (and* stx)
  (syntax-case stx []
    [(_ bexp bexps ...)
     #'(let* ([b bexp] [b (and bexps b)] ...)
         b)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-validate : (-> XML-Type (Listof XML-Content*) Boolean (Option Index) Boolean)
  (lambda [dtype contents standalone? topsize]
    (define ENTITY : DTD-Entities (xml-type-entities dtype))
    (define NOTATION : DTD-Notations (xml-type-notations dtype))
    (define ELEMENT : DTD-Elements (xml-type-elements dtype))
    (define ATTLIST : DTD-Attributes (xml-type-attributes dtype))
    
    (for ([(elem attlist) (in-hash ATTLIST)]
          #:unless (hash-has-key? ELEMENT elem))
      (let ([vs (hash-values attlist)])
        (when (pair? vs)
          (make+exn:xml:undeclared (dtd-attribute-element (car vs))
                                   #false log-level/not-an-error))))

    (define ent-notation-okay? : Boolean
      (for/fold ([valid? : Boolean #true])
                ([ent (in-hash-values ENTITY)]
                 #:when (dtd-unparsed-entity? ent))
        (and* valid?
              (let ([<ndata> (dtd-unparsed-entity-ndata ent)])
                (or (hash-has-key? NOTATION (xml:name-datum <ndata>))
                    (not (make+exn:xml:undeclared <ndata> (dtd-entity-name ent) 'warning)))))))

    (define attr-id-okay? : Boolean
      (for/fold ([valid? : Boolean #true])
                ([(_ attlist) (in-hash ATTLIST)])
        (define ids : (Listof DTD-ID-Attribute)
          ((inst sort DTD-ID-Attribute Positive-Integer)
           (filter-map dtd-id-attribute? (hash-values attlist))
           < #:key dtd-id-position))
        
        (and* valid?
              (or (null? ids)
                  ; https://www.w3.org/TR/xml/#one-id-per-el
                  (or (null? (cdr ids))
                      (not (for ([idtype (in-list (cdr ids))])
                             (make+exn:xml:multiple (car idtype) (cadr idtype) 'warning))))))))
  
    (define attr-notation-okay? : Boolean
      (for/fold ([valid? : Boolean #true])
                ([(elem attlist) (in-hash ATTLIST)])
        (define pelement : (Option DTD-Element) (hash-ref ELEMENT elem (λ [] #false)))
        (define notations : (Listof DTD-Notation-Attribute)
          ((inst sort DTD-Notation-Attribute Positive-Integer)
           (filter-map dtd-notation-attribute? (hash-values attlist))
           < #:key dtd-notation-position))
        
        (and* valid?
              (or (null? notations)
                  (and*
                   ; https://www.w3.org/TR/xml/#OneNotationPer
                   (or (null? (cdr notations))
                       (not (for ([ntype (in-list (cdr notations))])
                              (make+exn:xml:multiple (car ntype) (cadr ntype) 'warning))))
                   ; https://www.w3.org/TR/xml/#NoNotationEmpty
                   (or (dtd-empty-element? pelement)
                       (not (for ([ntype (in-list notations)])
                              (make+exn:xml:nonempty (car ntype) (cadr ntype)))))
                   ; https://www.w3.org/TR/xml/#notatn
                   (for*/fold ([declared? : Boolean #true])
                              ([ns (in-list notations)]
                               [n (in-list (caddr ns))])
                     (and* declared?
                           (or (hash-has-key? NOTATION (xml:name-datum n))
                               (not (make+exn:xml:undeclared n (caar ns) 'warning))))))))))
    
    (let verify ([rest : (Listof XML-Content*) contents]
                 [nroot : Natural 0]
                 [all-ids : DTD-IDs (make-immutable-hasheq)]
                 [valid? : Boolean #false])
      (if (pair? rest)
          (let-values ([(self rest++) (values (car rest) (cdr rest))])
            (cond [(mpair? self) (verify rest++ nroot all-ids valid?)]
                  [else (let-values ([(ids++ self-valid?) (xml-validate-element self all-ids dtype standalone? topsize)])
                          (when (> nroot 0) (make+exn:xml:multi-root (car self)))
                          (cond [(not self-valid?) (verify rest++ (+ nroot 1) ids++ #false)]
                                [else (verify rest++ (+ nroot 1) ids++ (if (zero? nroot) #true valid?))]))]))
          (and* valid? (= nroot 1)
                ent-notation-okay? attr-id-okay? attr-notation-okay?
                (let ([unknown-ids (filter xml-free-id? (hash-values all-ids))])
                  (or (null? unknown-ids)
                      (not (for ([uid (in-list unknown-ids)])
                             (make+exn:xml:id (car uid) (cdr uid)))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-validate-element : (-> XML-Element* DTD-IDs XML-Type Boolean (Option Index) (Values DTD-IDs Boolean))
  (lambda [elem ids dtype standalone? topsize]
    (define <self> : XML:Name (car elem))
    (define self-name : Symbol (xml:name-datum (car elem)))
    (define self-attributes : DTD-AttList (hash-ref (xml-type-attributes dtype) self-name (λ [] empty-attributes)))
    (define self-requireds : DTD-Required-Attrs
      (for/hash : DTD-Required-Attrs ([(aname attr) (in-hash self-attributes)]
                                      #:when (dtd-attribute/required? attr))
        (values aname (dtd-attribute-name attr))))

    (define-values (ids++ literal-valid? requireds rest-required-count)
      (let validate-attributes+children : (Values DTD-IDs Boolean DTD-Required-Attrs Index)
        ([a-ids : DTD-IDs ids]
         [a-requireds : DTD-Required-Attrs self-requireds]
         [rest : (Listof XML-Element-Attribute*) (cadr elem)]
         [valid? : Boolean #true])
        (if (pair? rest)
            (let-values ([(ids++ requireds-- okay?) (xml-validate-attribute <self> (car rest) a-ids a-requireds self-attributes standalone? dtype topsize)])
              (validate-attributes+children ids++ requireds-- (cdr rest) (and valid? okay?)))
            (let-values ([(ids++ self-valid?) (xml-validate-child <self> (caddr elem) a-ids dtype standalone? topsize)])
              (values ids++ (and valid? self-valid?) a-requireds (hash-count a-requireds))))))

    (when (> rest-required-count 0)
      (for ([required (in-hash-values requireds)])
        (make+exn:xml:missing-attr required <self>)))

    (values ids++ (and literal-valid? (= rest-required-count 0)))))

(define xml-validate-attribute : (-> XML:Name XML-Element-Attribute* DTD-IDs DTD-Required-Attrs DTD-AttList Boolean XML-Type (Option Index)
                                     (Values DTD-IDs DTD-Required-Attrs Boolean))
  ;;; https://www.w3.org/TR/xml/#ValueType
  ;;; https://www.w3.org/TR/xml/#vc-check-rmd
  (lambda [<self> attr ids requireds attributes standalone? dtype topsize]
    (define-values (<aname> <avalue>) (values (car attr) (cdr attr)))
    (define aname : Symbol (xml:name-datum <aname>))
    (define requireds-- : DTD-Required-Attrs (if (hash-has-key? requireds aname) (hash-remove requireds aname) requireds))
    (define aself : (Option DTD-Attribute) (hash-ref attributes aname (λ [] #false)))
    (define atype : (Option DTD-Attribute-Type) (and aself (dtd-attribute-type aself)))

    (define fixed-value-okay? : Boolean
      (cond [(and (dtd-attribute+default? aself) (dtd-attribute+default-fixed? aself))
             (let ([?attr (xml-element-attribute-normalize aself (xml-type-entities dtype) null topsize #false)])
               (and ?attr
                    (let ([fvalue (cdr ?attr)])
                      (cond [(and (xml:string? <avalue>) (xml:string? fvalue))
                             (or (xml:string=? <avalue> fvalue) (not (make+exn:xml:fixed (list <aname> <avalue>) <self>)))]
                            [(and (xml:name? <avalue>) (xml:name? fvalue))
                             (or (xml:name=? <avalue> fvalue) (not (make+exn:xml:fixed (list <aname> <avalue>) <self>)))]
                            [(and (list? <avalue>) (list? fvalue))
                             (and (= (length <avalue>) (length fvalue))
                                  (let ([fvalues (map xml:name-datum fvalue)])
                                    (for/fold ([okay? : Boolean #true])
                                              ([av (in-list <avalue>)])
                                      (and (or (memq (xml:name-datum av) fvalues)
                                               (not (make+exn:xml:fixed (list <aname> av) <self>)))
                                           okay?))))]
                            [else (not (make+exn:xml:fixed (dtd-error-tokens <aname> <avalue>) <self>))]))))]
            [else #true]))

    (cond [(dtd-attribute-token-type? atype)
           (let ([names? (dtd-attribute-token-type-names? atype)])
             (case (xml:name-datum (dtd-attribute-token-type-name atype))
               [(ID)
                (cond [(not (xml:name? <avalue>)) (values ids requireds-- #false)]
                      [else (values (hash-set ids (xml:name-datum <avalue>) #true) requireds-- fixed-value-okay?)])]
               [(IDREF IDREFS)
                (cond [(xml:string? <avalue>) (values ids requireds-- #false)]
                      [else (values (for/fold ([ids++ : DTD-IDs ids])
                                              ([<ref> (if (xml:name? <avalue>) (in-value <avalue>) (in-list <avalue>))])
                                      (let ([ref (xml:name-datum <ref>)])
                                        (cond [(hash-has-key? ids++ ref) ids++]
                                              [else (hash-set ids++ ref (cons (list <aname> <ref>) <self>))])))
                                      requireds-- fixed-value-okay?)])]
               [(ENTITY ENTITIES)
                (cond [(xml:string? <avalue>) (values ids requireds-- #false)]
                      [else (let ([ENTITY (xml-type-entities dtype)])
                              (values ids requireds--
                                      (for/fold ([entity-valid? : Boolean fixed-value-okay?])
                                                ([<ent> (if (xml:name? <avalue>) (in-value <avalue>) (in-list <avalue>))])
                                        (let ([?e (hash-ref ENTITY (xml:name-datum <ent>) (λ [] #false))])
                                          (cond [(dtd-unparsed-entity? ?e) entity-valid?]
                                                [(dtd-entity? ?e) (not (make+exn:xml:entity (list <aname> <ent>) <self>))]
                                                [else (not (make+exn:xml:undeclared (list <aname> <ent>) <self> 'warning))])))))])]
               [(NMTOKEN NMTOKENS)
                (cond [(xml:string? <avalue>) (values ids requireds-- #false)]
                      [else (values ids requireds--
                                    (for/fold ([name-valid? : Boolean fixed-value-okay?])
                                              ([<nmt> (if (xml:name? <avalue>) (in-value <avalue>) (in-list <avalue>))])
                                      (and (or (for/and : Boolean ([ch (in-string (symbol->immutable-string (xml:name-datum <nmt>)))])
                                                 (xml-name-char? ch))
                                               (not (make+exn:xml:char (list <aname> <nmt>) <self> 'warning)))
                                           name-valid?)))])]
               [else (values ids requireds-- #false)]))]
          [(dtd-attribute-enum-type? atype)
           (let ([options (map xml:name-datum (dtd-attribute-enum-type-options atype))])
             (values ids requireds--
                     (and (or (and (xml:name? <avalue>) (memq (xml:name-datum <avalue>) options) #true)
                              (not (make+exn:xml:enum (dtd-error-tokens <aname> <avalue>) <self>)))
                          fixed-value-okay?)))]
          [(dtd-attribute-string-type? atype)
           (values ids requireds-- (and fixed-value-okay? (xml:string? <avalue>)))]
          [else
           (make+exn:xml:undeclared <aname> <self> (if (xml:attname? aname) log-level/not-an-error 'warning))
           (values ids requireds-- #false)])))

(define xml-validate-child : (-> XML:Name (Listof (U XML-Subdatum* XML-Element*)) DTD-IDs XML-Type Boolean (Option Index) (Values DTD-IDs Boolean))
  ;;; https://www.w3.org/TR/xml/#elementvalid
  ;;; https://www.w3.org/TR/xml/#vc-check-rmd
  (lambda [<self> children ids dtype standalone? topsize]
    (define self : (Option DTD-Element) (hash-ref (xml-type-elements dtype) (xml:name-datum <self>) (λ [] #false)))

    (cond [(dtd-empty-element? self)
           (values ids
                   (or (null? children)
                       (not (make+exn:xml:nonempty <self>))))]
          [(dtd-element+children? self)
           (let*-values ([(cdata rest) (partition xml:string? children)]
                         [(subelems) (filter list rest)])
             (values ids
                     (or (null? cdata)
                         (not (for ([subc (in-list cdata)])
                                (make+exn:xml:adoptee subc <self>))))))]
          [(dtd-mixed-element? self)
           (let validate-mixed ([rest : (Listof XML-Element*) (filter xml-element? children)]
                                [ids : DTD-IDs ids]
                                [valid? : Boolean #true])
             (cond [(null? rest) (values ids valid?)]
                   [else (let* ([child (car rest)]
                                [<child> (car child)]
                                [child-name (xml:name-datum <child>)])
                           (cond [(memq child-name (dtd-mixed-element-children self))
                                  (let-values ([(ids++ subvalid?) (xml-validate-element child ids dtype standalone? topsize)])
                                    (validate-mixed (cdr rest) ids++ (and valid? subvalid?)))]
                                 [else (validate-mixed (cdr rest) ids (not (make+exn:xml:adoptee <child> <self>)))]))]))]
          [(dtd-element? self)
           (let validate-any ([rest : (Listof XML-Element*) (filter xml-element? children)]
                              [ids : DTD-IDs ids]
                              [valid? : Boolean #true])
             (cond [(null? rest) (values ids valid?)]
                   [else (let-values ([(ids++ subvalid?) (xml-validate-element (car rest) ids dtype standalone? topsize)])
                           (validate-any (cdr rest) ids++ (and valid? subvalid?)))]))]
          [else (values ids #false)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml:attname? : (-> Symbol Boolean)
  (lambda [aname]
    (string-prefix? (symbol->immutable-string aname) "xml:")))

(define dtd-id-attribute? : (-> DTD-Attribute (Option DTD-ID-Attribute))
  (lambda [attr]
    (define type (dtd-attribute-type attr))
    (and (dtd-attribute-token-type? type)
         (eq? 'ID (xml:name-datum (dtd-attribute-token-type-name type)))
         (list (dtd-attribute-name attr)
               (dtd-attribute-element attr)))))

(define dtd-id-position : (-> DTD-ID-Attribute Positive-Integer)
  (lambda [idtype]
    (w3s-token-start (car idtype))))

(define dtd-notation-attribute? : (-> DTD-Attribute (Option DTD-Notation-Attribute))
  (lambda [attr]
    (define type (dtd-attribute-type attr))
    (and (dtd-attribute-enum-type? type)
         (let ([?notation (dtd-attribute-enum-type-?notation type)])
           (and ?notation
                (list (list (dtd-attribute-name attr) ?notation)
                      (dtd-attribute-element attr)
                      (dtd-attribute-enum-type-options type)))))))

(define dtd-notation-position : (-> DTD-Notation-Attribute Positive-Integer)
  (lambda [ntype]
    (w3s-token-start (caar ntype))))

(define dtd-error-tokens : (-> XML-Token XML-Element-Attribute-Value* (Listof XML-Token))
  (lambda [<aname> <avalue>]
    (if (list? <avalue>)
        (cons <aname> <avalue>)
        (list <aname> <avalue>))))
