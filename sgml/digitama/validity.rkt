#lang typed/racket/base

(provide (all-defined-out))

(require racket/string)
(require racket/symbol)
(require racket/vector)

(require typed/racket/unsafe)

(require "schema.rkt")
(require "digicore.rkt")
(require "grammar.rkt")
(require "normalize.rkt")
(require "tokenizer/characters.rkt")

(unsafe-require/typed
 racket/base
 [(list? xml-element?) (-> Any Boolean : XML-Element*)]
 [(pair? xml-free-id?) (-> Any Boolean : XSch-Free-ID)])

(require (for-syntax racket/base))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type XSch-Free-ID (Pairof (Listof XML:Name) (Option XML:Name)))
(define-type XSch-IDs (Immutable-HashTable Symbol (U True XSch-Free-ID)))
(define-type XSch-AttList (Immutable-HashTable Symbol XSch-Attribute))
(define-type XSch-Required-Attrs (Immutable-HashTable Symbol XML:Name))
(define-type XSch-ID-Attribute (List XML:Name XML:Name))
(define-type XSch-Notation-Attribute (List (List XML:Name XML:Name) XML:Name (Pairof XML:Name (Listof XML:Name))))

(define log-level/not-an-error : Log-Level 'debug)

(define-syntax (and* stx)
  (syntax-case stx []
    [(_ bexp bexps ...)
     #'(let* ([b bexp] [b (and bexps b)] ...)
         b)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-validate : (-> XML-Schema (Listof XML-Content*) Boolean (Option Index) Boolean)
  (lambda [schema contents standalone? topsize]
    (define ENTITY : Schema-Entities (xml-schema-entities schema))
    (define NOTATION : Schema-Notations (xml-schema-notations schema))
    (define ELEMENT : Schema-Elements (xml-schema-elements schema))
    (define ATTLIST : Schema-Attributes (xml-schema-attributes schema))
    
    (for ([(elem attlist) (in-hash ATTLIST)]
          #:unless (hash-has-key? ELEMENT elem))
      (let ([vs (hash-values attlist)])
        (when (pair? vs)
          (make+exn:xml:undeclared (xsch-attribute-element (car vs))
                                   #false log-level/not-an-error))))

    (define ent-notation-okay? : Boolean
      (for/fold ([valid? : Boolean #true])
                ([ent (in-hash-values ENTITY)]
                 #:when (xsch-unparsed-entity? ent))
        (and* valid?
              (let ([<ndata> (xsch-unparsed-entity-ndata ent)])
                (or (hash-has-key? NOTATION (xml:name-datum <ndata>))
                    (not (make+exn:xml:undeclared <ndata> (xsch-entity-name ent) 'warning)))))))

    (define attr-id-okay? : Boolean
      (for/fold ([valid? : Boolean #true])
                ([(_ attlist) (in-hash ATTLIST)])
        (define ids : (Listof XSch-ID-Attribute)
          ((inst sort XSch-ID-Attribute Positive-Integer)
           (filter-map xsch-id-attribute? (hash-values attlist))
           < #:key xsch-id-position))
        
        (and* valid?
              (or (null? ids)
                  ; https://www.w3.org/TR/xml/#one-id-per-el
                  (or (null? (cdr ids))
                      (not (for ([idtype (in-list (cdr ids))])
                             (make+exn:xml:multiple (car idtype) (cadr idtype) 'warning))))))))
  
    (define attr-notation-okay? : Boolean
      (for/fold ([valid? : Boolean #true])
                ([(elem attlist) (in-hash ATTLIST)])
        (define pelement : (Option XSch-Element) (hash-ref ELEMENT elem (λ [] #false)))
        (define notations : (Listof XSch-Notation-Attribute)
          ((inst sort XSch-Notation-Attribute Positive-Integer)
           (filter-map xsch-notation-attribute? (hash-values attlist))
           < #:key xsch-notation-position))
        
        (and* valid?
              (or (null? notations)
                  (and*
                   ; https://www.w3.org/TR/xml/#OneNotationPer
                   (or (null? (cdr notations))
                       (not (for ([ntype (in-list (cdr notations))])
                              (make+exn:xml:multiple (car ntype) (cadr ntype) 'warning))))
                   ; https://www.w3.org/TR/xml/#NoNotationEmpty
                   (or (xsch-empty-element? pelement)
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
                 [all-ids : XSch-IDs (make-immutable-hasheq)]
                 [valid? : Boolean #false])
      (if (pair? rest)
          (let-values ([(self rest++) (values (car rest) (cdr rest))])
            (cond [(mpair? self) (verify rest++ nroot all-ids valid?)]
                  [else (let-values ([(ids++ self-valid?) (xml-validate-element self all-ids schema standalone? topsize)])
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
(define xml-validate-element : (-> XML-Element* XSch-IDs XML-Schema Boolean (Option Index) (Values XSch-IDs Boolean))
  (lambda [elem ids schema standalone? topsize]
    (define <self> : XML:Name (car elem))
    (define self-name : Symbol (xml:name-datum (car elem)))
    (define self-attributes : XSch-AttList (hash-ref (xml-schema-attributes schema) self-name (λ [] xsch-empty-attributes)))
    (define self-requireds : XSch-Required-Attrs
      (for/hash : XSch-Required-Attrs ([(aname attr) (in-hash self-attributes)]
                                       #:when (xsch-attribute/required? attr))
        (values aname (xsch-attribute-name attr))))

    (define-values (ids++ literal-valid? requireds rest-required-count)
      (let validate-attributes+children : (Values XSch-IDs Boolean XSch-Required-Attrs Index)
        ([a-ids : XSch-IDs ids]
         [a-requireds : XSch-Required-Attrs self-requireds]
         [rest : (Listof XML-Element-Attribute*) (cadr elem)]
         [valid? : Boolean #true])
        (if (pair? rest)
            (let-values ([(ids++ requireds-- okay?) (xml-validate-attribute <self> (car rest) a-ids a-requireds self-attributes standalone? schema topsize)])
              (validate-attributes+children ids++ requireds-- (cdr rest) (and valid? okay?)))
            (let-values ([(ids++ self-valid?) (xml-validate-children <self> (caddr elem) a-ids schema standalone? topsize)])
              (values ids++ (and valid? self-valid?) a-requireds (hash-count a-requireds))))))

    (when (> rest-required-count 0)
      (for ([required (in-hash-values requireds)])
        (make+exn:xml:missing-attr required <self>)))

    (values ids++ (and literal-valid? (= rest-required-count 0)))))

(define xml-validate-attribute : (-> XML:Name XML-Element-Attribute* XSch-IDs XSch-Required-Attrs XSch-AttList Boolean XML-Schema (Option Index)
                                     (Values XSch-IDs XSch-Required-Attrs Boolean))
  ;;; https://www.w3.org/TR/xml/#ValueType
  ;;; https://www.w3.org/TR/xml/#vc-check-rmd
  (lambda [<self> attr ids requireds attributes standalone? schema topsize]
    (define-values (<aname> <avalue>) (values (car attr) (cdr attr)))
    (define aname : Symbol (xml:name-datum <aname>))
    (define requireds-- : XSch-Required-Attrs (if (hash-has-key? requireds aname) (hash-remove requireds aname) requireds))
    (define aself : (Option XSch-Attribute) (hash-ref attributes aname (λ [] #false)))
    (define atype : (Option Schema-Attribute-Type) (and aself (xsch-attribute-type aself)))

    (define fixed-value-okay? : Boolean
      (cond [(and (xsch-attribute+default? aself) (xsch-attribute+default-fixed? aself))
             (let ([?attr (xml-element-attribute-normalize aself (xml-schema-entities schema) null topsize default-xxe-guard)])
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
                            [else (not (make+exn:xml:fixed (xsch-error-tokens <aname> <avalue>) <self>))]))))]
            [else #true]))

    (cond [(xsch-attribute-token-type? atype)
           (let ([names? (xsch-attribute-token-type-names? atype)])
             (case (xml:name-datum (xsch-attribute-token-type-name atype))
               [(ID)
                (cond [(not (xml:name? <avalue>)) (values ids requireds-- #false)]
                      [else (values (hash-set ids (xml:name-datum <avalue>) #true) requireds-- fixed-value-okay?)])]
               [(IDREF IDREFS)
                (cond [(xml:string? <avalue>) (values ids requireds-- #false)]
                      [else (values (for/fold ([ids++ : XSch-IDs ids])
                                              ([<ref> (if (xml:name? <avalue>) (in-value <avalue>) (in-list <avalue>))])
                                      (let ([ref (xml:name-datum <ref>)])
                                        (cond [(hash-has-key? ids++ ref) ids++]
                                              [else (hash-set ids++ ref (cons (list <aname> <ref>) <self>))])))
                                      requireds-- fixed-value-okay?)])]
               [(ENTITY ENTITIES)
                (cond [(xml:string? <avalue>) (values ids requireds-- #false)]
                      [else (let ([ENTITY (xml-schema-entities schema)])
                              (values ids requireds--
                                      (for/fold ([entity-valid? : Boolean fixed-value-okay?])
                                                ([<ent> (if (xml:name? <avalue>) (in-value <avalue>) (in-list <avalue>))])
                                        (let ([?e (hash-ref ENTITY (xml:name-datum <ent>) (λ [] #false))])
                                          (cond [(xsch-unparsed-entity? ?e) entity-valid?]
                                                [(xsch-entity? ?e) (not (make+exn:xml:parsed (list <aname> <ent>) <self>))]
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
          [(xsch-attribute-enum-type? atype)
           (let ([options (map xml:name-datum (xsch-attribute-enum-type-options atype))])
             (values ids requireds--
                     (and (or (and (xml:name? <avalue>) (memq (xml:name-datum <avalue>) options) #true)
                              (not (make+exn:xml:enum (xsch-error-tokens <aname> <avalue>) <self>)))
                          fixed-value-okay?)))]
          [(xsch-attribute-string-type? atype)
           (values ids requireds-- (and fixed-value-okay? (xml:string? <avalue>)))]
          [else
           (values ids requireds--
                   (not (make+exn:xml:undeclared <aname> <self>
                                                 (cond [(xml:attname? aname) log-level/not-an-error]
                                                       [else 'warning]))))])))

(define xml-validate-children : (-> XML:Name (Listof (U XML-Subdatum* XML-Element*)) XSch-IDs XML-Schema Boolean (Option Index) (Values XSch-IDs Boolean))
  ;;; https://www.w3.org/TR/xml/#elementvalid
  ;;; https://www.w3.org/TR/xml/#vc-check-rmd
  (lambda [<self> children ids schema standalone? topsize]
    (define self : (Option XSch-Element) (hash-ref (xml-schema-elements schema) (xml:name-datum <self>) (λ [] #false)))

    (cond [(xsch-empty-element? self) ; NONE is allowed
           (values ids
                   (or (null? children)
                       (not (make+exn:xml:nonempty <self>))))]
          [(xsch-element+children? self) ; whitespaces, comments, PIs are allowed
           (for/fold ([ids : XSch-IDs ids]
                      [valid? : Boolean (xml-children-match? <self> (filter xml-element? children) (xsch-element+children-content self))])
                     ([child (in-list children)])
             (cond [(xml:string? child)
                    (values ids (and valid? (not (make+exn:xml:adoptee child <self>))))]
                   [(xml-element? child)
                    (let-values ([(ids++ subvalid?) (xml-validate-element child ids schema standalone? topsize)])
                      (values ids++ (and valid? subvalid?)))]
                   [else (values ids valid?)]))]
          [(xsch-mixed-element? self) ; whitespaces, comments, PIs are allowed
           (for/fold ([ids : XSch-IDs ids] [valid? : Boolean #true])
                     ([child (in-list children)] #:when (xml-element? child))
             (define <child> : XML:Name (car child))
             (unless (memq (xml:name-datum <child>) (xsch-mixed-element-children self))
               (make+exn:xml:adoptee <child> <self>))
             (define-values (ids++ subvalid?) (xml-validate-element child ids schema standalone? topsize))
             (values ids++ (and valid? subvalid?)))]
          [(xsch-element? self) ; All is allowed
           (for/fold ([ids : XSch-IDs ids] [valid? : Boolean #true])
                     ([child (in-list children)] #:when (xml-element? child))
             (define-values (ids++ subvalid?) (xml-validate-element child ids schema standalone? topsize))
             (values ids++ (and valid? subvalid?)))]
          [else (values ids (not (make+exn:xml:undeclared <self> #false log-level/not-an-error)))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; WARNING: untested
(define xml-children-match? : (-> XML:Name (Listof XML-Element*) (Pairof Schema-Element-Children Char) Boolean)
  ;;; https://www.w3.org/TR/xml/#determinism
  (lambda [<self> children content]
    (define-values (pattern particle) (values (car content) (cdr content)))
    (define-values (match? rest)
      (if (vector? pattern)
          (xml-car-choice children pattern particle)
          (xml-car-sequence children pattern particle)))

    (or (and match? (null? rest))
        (not (make+exn:xml:children <self>)))))

(define xml-car-choice : (-> (Listof XML-Element*) Schema-Element-Choice Char (Values Boolean (Listof XML-Element*)))
  (lambda [children choice particle]
    (let match-next-round ([matcnt : Integer 0]
                           [subelems : (Listof XML-Element*) children])
      (cond [(null? subelems) (values (xml-quantifier-okay? matcnt particle) null)]
            [else (let ([?rest (for/or : (Option (Listof XML-Element*)) ([cp (in-vector choice #| at least one options is guaranteed |#)])
                                 (define-values (okay? rest) (xml-car-children subelems cp))
                                 (and okay? rest))])
                    (cond [(not ?rest) (values (xml-quantifier-okay? matcnt particle) subelems)]
                          [(eq? subelems ?rest) (values (xml-quantifier-okay? matcnt particle) ?rest)]
                          [else (match-next-round (add1 matcnt) ?rest)]))]))))

(define xml-car-sequence : (-> (Listof XML-Element*) Schema-Element-Sequence Char (Values Boolean (Listof XML-Element*)))
  (lambda [children seq particle]
    (let match-next-round ([matcnt : Integer 0]
                           [children : (Listof XML-Element*) children])
      (let match-this-round ([subelems : (Listof XML-Element*) children]
                             [seqrest : (Listof (Pairof (U Symbol Schema-Element-Children) Char)) seq #| initially at least one components is guaranteed to exist |#])
        (cond [(pair? seqrest)
               (let-values ([(okay? subrest) (xml-car-children subelems (car seqrest))])
                 (cond [(not okay?) (values (xml-quantifier-okay? matcnt particle) children)]
                       [else (match-this-round subrest (cdr seqrest))]))]
              [(eq? subelems children) (values (xml-quantifier-okay? matcnt particle) children)]
              [(pair? subelems) (match-next-round (add1 matcnt) subelems)]
              [else (values (xml-quantifier-okay? (add1 matcnt) particle) null)])))))

(define xml-car-elements : (-> (Listof XML-Element*) Symbol Char (Values Boolean (Listof XML-Element*)))
  (lambda [children name particle]
    (define rest : (Listof XML-Element*)
      (let match-next ([rest : (Listof XML-Element*) children])
        (cond [(null? rest) rest]
              [(eq? (xml:name-datum (caar rest)) name) (match-next (cdr rest))]
              [else rest])))

    (let ([maxcnt (length children)]
          [rstcnt (length rest)])
      (values (xml-quantifier-okay? (- maxcnt rstcnt) particle)
              rest))))

(define xml-car-children : (-> (Listof XML-Element*) (Pairof (U Symbol Schema-Element-Children) Char) (Values Boolean (Listof XML-Element*)))
  (lambda [children cp]
    (define-values (pattern particle) (values (car cp) (cdr cp)))
    
    (cond [(symbol? pattern) (xml-car-elements children pattern particle)]
          [(vector? pattern) (xml-car-choice children pattern particle)]
          [else (xml-car-sequence children pattern particle)])))

(define xml-quantifier-okay? : (-> Integer Char Boolean)
  (lambda [matcnt particle]
    ;;; TODO: is greedy matching okay?
    (cond [(eq? particle #\+) (>= matcnt 1)]
          [(eq? particle #\*) (>= matcnt 0)]
          [(eq? particle #\?) (<= matcnt 1)]
          [else (= matcnt 1)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml:attname? : (-> Symbol Boolean)
  (lambda [aname]
    (string-prefix? (symbol->immutable-string aname) "xml:")))

(define xsch-id-attribute? : (-> XSch-Attribute (Option XSch-ID-Attribute))
  (lambda [attr]
    (define type (xsch-attribute-type attr))
    (and (xsch-attribute-token-type? type)
         (eq? 'ID (xml:name-datum (xsch-attribute-token-type-name type)))
         (list (xsch-attribute-name attr)
               (xsch-attribute-element attr)))))

(define xsch-id-position : (-> XSch-ID-Attribute Positive-Integer)
  (lambda [idtype]
    (w3s-token-start (car idtype))))

(define xsch-notation-attribute? : (-> XSch-Attribute (Option XSch-Notation-Attribute))
  (lambda [attr]
    (define type (xsch-attribute-type attr))
    (and (xsch-attribute-enum-type? type)
         (let ([?notation (xsch-attribute-enum-type-?notation type)])
           (and ?notation
                (list (list (xsch-attribute-name attr) ?notation)
                      (xsch-attribute-element attr)
                      (xsch-attribute-enum-type-options type)))))))

(define xsch-notation-position : (-> XSch-Notation-Attribute Positive-Integer)
  (lambda [ntype]
    (w3s-token-start (caar ntype))))

(define xsch-error-tokens : (-> XML-Token XML-Element-Attribute-Value* (Listof XML-Token))
  (lambda [<aname> <avalue>]
    (if (list? <avalue>)
        (cons <aname> <avalue>)
        (list <aname> <avalue>))))
