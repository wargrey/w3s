#lang typed/racket/base

(provide (all-defined-out))

(require racket/string)
(require racket/symbol)

(require typed/racket/unsafe)

(require "dtd.rkt")
(require "digicore.rkt")
(require "grammar.rkt")
(require "normalize.rkt")

(unsafe-require/typed
 racket/base
 [mpair? (-> Any Boolean : XML-Processing-Instruction*)]
 [list? (-> Any Boolean : XML-Element*)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type DTD-IDs (Immutable-HashTable Symbol (U True XML-Token)))
(define-type DTD-Required-Attrs (Immutable-HashTable Symbol XML:Name))

(define log-level/not-an-error : Log-Level 'debug)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-validate : (-> XML-Type (Listof XML-Content*) Boolean (Option Index) Boolean)
  (lambda [dtype contents standalone? topsize]
    (define ELEMENT : DTD-Elements (xml-type-elements dtype))
    (define ATTLIST : DTD-Attributes (xml-type-attributes dtype))
    (define ENTITY : DTD-Entities (xml-type-entities dtype))

    (for ([(elem attlist) (in-hash ATTLIST)]
          #:unless (hash-has-key? ELEMENT elem))
      (define vs : (Listof DTD-Attribute) (hash-values attlist))
      (when (pair? vs)
        (make+exn:xml:undeclared (dtd-attribute-element (car vs))
                                 #false log-level/not-an-error)))
    
    (let verify ([rest : (Listof XML-Content*) contents]
                 [nroot : Natural 0]
                 [all-ids : DTD-IDs (make-immutable-hasheq)]
                 [valid? : Boolean #false])
      (if (pair? rest)
          (let-values ([(self rest++) (values (car rest) (cdr rest))])
            (cond [(mpair? self) (verify rest++ nroot all-ids valid?)]
                  [else (let-values ([(ids++ valid?++) (xml-validate-element self all-ids ELEMENT ATTLIST standalone? ENTITY topsize)])
                          (when (> nroot 0) (make+exn:xml:multi-root (car self)))
                          (cond [(not valid?++) (verify rest++ (+ nroot 1) all-ids #false)]
                                [else (verify rest++ (+ nroot 1) ids++ (if (zero? nroot) #true valid?))]))]))
          (and (let ([unknown-ids (filter xml-token? (hash-values all-ids))])
                 (or (null? unknown-ids)
                     (not (for-each make+exn:xml:id unknown-ids))))
               valid?
               (= nroot 1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-validate-element : (-> XML-Element* DTD-IDs DTD-Elements DTD-Attributes Boolean DTD-Entities (Option Index) (Values DTD-IDs Boolean))
  (lambda [elem ids elements attributes standalone? entities topsize]
    (define self-tag : XML:Name (car elem))
    (define self-name : Symbol (xml:name-datum (car elem)))
    (define self-attributes : (Immutable-HashTable Symbol DTD-Attribute) (hash-ref attributes self-name (λ [] empty-attributes)))
    (define self-requireds : DTD-Required-Attrs
      (for/hash : DTD-Required-Attrs ([(aname attr) (in-hash self-attributes)]
                                      #:when (dtd-attribute/required? attr))
        (values aname (dtd-attribute-name attr))))

    (define-values (ids++ read-valid? requireds rcount)
      (let validate-attributes+children : (Values DTD-IDs Boolean DTD-Required-Attrs Index)
        ([a-ids : DTD-IDs ids]
         [a-requireds : DTD-Required-Attrs self-requireds]
         [rest : (Listof XML-Element-Attribute*) (cadr elem)]
         [valid? : Boolean #true])
        (if (pair? rest)
            (let-values ([(ids++ requireds-- self-valid?) (xml-validate-attribute self-tag (car rest) a-ids a-requireds self-attributes standalone? entities topsize)])
              (validate-attributes+children ids++ requireds-- (cdr rest) (and valid? self-valid?)))
            (let-values ([(ids++ self-valid?) (xml-validate-child self-tag (caddr elem) a-ids elements attributes standalone? entities topsize)])
              (values ids++ (and valid? self-valid?) a-requireds (hash-count a-requireds))))))

    (when (> rcount 0)
      (for ([required (in-hash-values requireds)])
        (make+exn:xml:missing-attr required self-tag)))

    (values ids++ (and read-valid? (= rcount 0)))))

(define xml-validate-attribute : (-> XML:Name XML-Element-Attribute* DTD-IDs DTD-Required-Attrs (Immutable-HashTable Symbol DTD-Attribute) Boolean
                                     DTD-Entities (Option Index) (Values DTD-IDs DTD-Required-Attrs Boolean))
  ;;; https://www.w3.org/TR/xml/#ValueType
  ;;; https://www.w3.org/TR/xml/#vc-check-rmd
  (lambda [self-tag attr ids requireds attributes standalone? entities topsize]
    (define-values (aname avalue) (values (car attr) (cdr attr)))
    (define attrname : Symbol (xml:name-datum aname))
    (define requireds-- : DTD-Required-Attrs (if (hash-has-key? requireds attrname) (hash-remove requireds attrname) requireds))
    (define aself : (Option DTD-Attribute) (hash-ref attributes attrname (λ [] #false)))
    (define atype : (Option DTD-Attribute-Type) (and aself (dtd-attribute-type aself)))

    (define fixed-value-okay? : Boolean
      (cond [(and (dtd-attribute+default? aself) (dtd-attribute+default-fixed? aself))
             (let ([?attr (xml-element-attribute-normalize aself entities null topsize #false)])
               (and ?attr
                    (let ([fvalue (cdr ?attr)])
                      (cond [(and (xml:string? avalue) (xml:string? fvalue))
                             (or (xml:string=? avalue fvalue) (not (make+exn:xml:fixed avalue aname)))]
                            [(and (xml:name? avalue) (xml:name? fvalue))
                             (or (xml:name=? avalue fvalue) (not (make+exn:xml:fixed avalue aname)))]
                            [(and (list? avalue) (list? fvalue))
                             (and (= (length avalue) (length fvalue))
                                  (let ([fvalues (map xml:name-datum fvalue)])
                                    (for/and ([av (in-list avalue)])
                                      (or (memq (xml:name-datum av) fvalues)
                                          (not (make+exn:xml:fixed av aname))))))]
                            [else (not (make+exn:xml:fixed avalue aname))]))))]
            [else #true]))

    (cond [(dtd-attribute-enum-type? atype)
           (displayln (cons attrname avalue))
           (values ids requireds
                   (and (or (and (xml:name? avalue)
                                 (memq (xml:name-datum avalue)
                                       (dtd-attribute-enum-type-options atype)) #true)
                            (not (make+exn:xml:enum avalue aname)))
                        fixed-value-okay?))]
          [(dtd-attribute-string-type? atype)
           (values ids requireds (and fixed-value-okay? (xml:string? avalue)))]
          [else
           (make+exn:xml:undeclared aname self-tag (if (xml:attname? attrname) log-level/not-an-error 'warning))
           (values ids requireds-- #false)])))

(define xml-validate-child : (-> XML:Name (Listof (U XML-Subdatum* XML-Element*)) DTD-IDs DTD-Elements DTD-Attributes Boolean
                                 DTD-Entities (Option Index) (Values DTD-IDs Boolean))
  ;;; https://www.w3.org/TR/xml/#elementvalid
  ;;; https://www.w3.org/TR/xml/#vc-check-rmd
  (lambda [self-tag children ids elements attributes standalone? entities topsize]
    (define self : (Option DTD-Element) (hash-ref elements (xml:name-datum self-tag) (λ [] #false)))

    (cond [(dtd-empty-element? self)
           (values ids
                   (or (null? children)
                       (not (make+exn:xml:nonempty self-tag))))]
          [(dtd-element+children? self)
           (let*-values ([(cdata rest) (partition xml:string? children)]
                         [(subelems) (filter list rest)])
             (values ids
                     (or (null? cdata)
                         (not (for ([subc (in-list cdata)])
                                (make+exn:xml:adoptee subc self-tag))))))]
          [(dtd-mixed-element? self)
           (let validate-mixed ([rest : (Listof XML-Element*) (filter list? children)]
                                [ids : DTD-IDs ids]
                                [valid? : Boolean #true])
             (cond [(null? rest) (values ids valid?)]
                   [else (let* ([child (car rest)]
                                [child-tag (car child)]
                                [child-name (xml:name-datum child-tag)])
                           (cond [(memq child-name (dtd-mixed-element-children self))
                                  (let-values ([(ids++ subvalid?) (xml-validate-element child ids elements attributes standalone? entities topsize)])
                                    (validate-mixed (cdr rest) ids++ (and valid? subvalid?)))]
                                 [else (validate-mixed (cdr rest) ids (not (make+exn:xml:adoptee child-tag self-tag)))]))]))]
          [(dtd-element? self)
           (let validate-any ([rest : (Listof XML-Element*) (filter list? children)]
                              [ids : DTD-IDs ids]
                              [valid? : Boolean #true])
             (cond [(null? rest) (values ids valid?)]
                   [else (let-values ([(ids++ subvalid?) (xml-validate-element (car rest) ids elements attributes standalone? entities topsize)])
                           (validate-any (cdr rest) ids++ (and valid? subvalid?)))]))]
          [else (values ids #false)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml:attname? : (-> Symbol Boolean)
  (lambda [aname]
    (string-prefix? (symbol->immutable-string aname) "xml:")))
