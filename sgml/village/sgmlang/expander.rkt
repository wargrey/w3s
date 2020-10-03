#lang typed/racket/base

(provide (all-defined-out))

(require sgml/digitama/dtd)
(require sgml/digitama/doctype)
(require sgml/digitama/grammar)
(require sgml/digitama/document)
(require sgml/digitama/digicore)

(require css/digitama/syntax/w3s)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

(struct xml-pi-datum ; Typed Racket has trouble in generating contracts for mutable pairs
  ([name : XML-Name]
   [body : (Option XML-String)])
  #:prefab
  #:type-name XML-PI-Datum)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type XML-DTD-Datum (Pairof W3S-Token-Source (Listof DTD-Declaration-Datum)))

(define-type DTD-Raw-Declaration-Datum (Immutable-Vector XML-Name (Listof (W3S-Token-Datumof Any))))

(define-type DTD-Declaration-Datum
  (Rec defs (U XML-PI-Datum DTD-Entity-Datum DTD-Notation-Datum DTD-Element-Datum DTD-AttList-Datum XML-PEReference
               DTD-Raw-Declaration-Datum (Pairof (U XML-Name XML-PEReference) (Listof defs)))))

(define-type DTD-Element-Mixed-Datum (Immutable-Vectorof XML-Name))
(define-type DTD-Element-Sequence-Datum (Immutable-Vectorof (Pairof Char (U XML-Name DTD-Element-Children-Datum))))
(define-type DTD-Element-Choice-Datum (Listof (Pairof Char (U XML-Name DTD-Element-Children-Datum))))
(define-type DTD-Element-Children-Datum (U DTD-Element-Sequence-Datum DTD-Element-Choice-Datum))

(struct dtd-entity-datum
  ([name : (U XML-Reference XML-PEReference)]
   [value : (Option XML-String)]
   [public : (Option XML-String)]
   [system : (Option XML-String)]
   [ndata : (Option XML-Name)])
  #:prefab
  #:type-name DTD-Entity-Datum)

(struct dtd-notation-datum
  ([name : XML-Name]
   [public : (Option XML-String)]
   [system : (Option XML-String)])
  #:prefab
  #:type-name DTD-Notation-Datum)

(struct dtd-attribute-datum
  ([element : XML-Name]
   [name : XML-Name]
   [type : (U False XML-Name (Pairof XML-Name (Listof XML-Name)))]
   [typeflag : Boolean]
   [default : (U XML-String Boolean)]
   [fixed? : Boolean])
  #:prefab
  #:type-name DTD-Attribute-Datum)

(struct dtd-attlist-datum
  ([element : XML-Name]
   [body : (Listof DTD-Attribute-Datum)])
  #:prefab
  #:type-name DTD-AttList-Datum)

(struct dtd-element-datum
  ([name : XML-Name]
   [body : (U Boolean DTD-Element-Mixed-Datum (Pairof Char DTD-Element-Children-Datum))])
  #:prefab
  #:type-name DTD-Element-Datum)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type DTD-Entity-Data (Listof (Pairof (U Symbol Keyword) DTD-Entity-Datum)))
(define-type DTD-Notation-Data (Listof (Pairof Symbol DTD-Notation-Datum)))
(define-type DTD-Element-Data (Listof (Pairof Symbol DTD-Element-Datum)))
(define-type DTD-Attribute-Data (Listof (Pairof Symbol (Listof (Pairof Symbol DTD-Attribute-Datum)))))

; Typed Racket may have trouble in restoring types of prefab structs
; Especially when they are the root types of arguments
(define-type XML-Type-Datum (Immutable-Vector DTD-Entity-Data DTD-Notation-Data DTD-Element-Data DTD-Attribute-Data))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-document->location+datum : (-> XML-Document*
                                           (Values W3S-Token-Source (Option Nonnegative-Flonum) (Option String) Boolean
                                                   (Option XML-Name) XML-External-ID
                                                   (Listof XML-Content-Datum)))
  (lambda [doc.xml]
    (define prolog (xml-document*-prolog doc.xml))
    (define doctype (xml-document*-doctype doc.xml))
    (define root-name (xml-doctype*-name doctype))
    (define extid (xml-doctype*-external doctype))

    (values (xml-prolog-location prolog)
            (xml-prolog-version prolog) (xml-prolog-encoding prolog)
            (xml-prolog-standalone? prolog)

            (w3s-token->location+datum* root-name xml:name-datum)
            (cond [(pair? extid)
                   (cons (w3s-token->location+datum (car extid) xml:string-datum)
                         (w3s-token->location+datum (cdr extid) xml:string-datum))]
                  [(xml:string? extid)
                   (w3s-token->location+datum extid xml:string-datum)]
                  [else #false])

            (map xml-content->location+datum (xml-document*-contents doc.xml)))))

(define xml-dtd->location+datum : (-> XML-DTD XML-DTD-Datum)
  (lambda [doc.dtd]
    (cons (xml-dtd-location doc.dtd)
          (map dtd-declaration->location+datum (xml-dtd-declarations doc.dtd)))))

(define xml-type->location+datum : (-> XML-Type XML-Type-Datum)
  (lambda [type]
    (vector-immutable
     (for/list : DTD-Entity-Data ([(name entity) (in-hash (xml-type-entities type))])
       (cons name (dtd-entity->location+datum entity)))
     (for/list : DTD-Notation-Data ([(name notation) (in-hash (xml-type-notations type))])
       (cons name (dtd-notation->location+datum notation)))
     (for/list : DTD-Element-Data ([(name element) (in-hash (xml-type-elements type))])
       (cons name (dtd-element->location+datum element)))
     (for/list : DTD-Attribute-Data ([(element attlist) (in-hash (xml-type-attributes type))])
       ((inst cons Symbol (Listof (Pairof Symbol DTD-Attribute-Datum)))
        element (for/list : (Listof (Pairof Symbol DTD-Attribute-Datum)) ([(name attr) (in-hash attlist)])
                  (cons name (dtd-attribute->location+datum attr))))))))

(define xml-location+datum->document : (-> W3S-Token-Source (Option Nonnegative-Flonum) (Option String) Boolean (Option XML-Name) XML-External-ID
                                           XML-DTD-Datum (Option XML-DTD-Datum) (Option XML-Type-Datum) (Listof XML-Content-Datum)
                                           XML-Document*)
  (lambda [location version encoding standalone? root-name external-id internal-dtd ?external-dtd ?type contents]
    (make-xml-document* location version encoding standalone?

                        (w3s-location+datum->token* xml:name root-name)

                        (cond [(pair? external-id)
                               (cons (w3s-location+datum->token xml:string (car external-id))
                                     (w3s-location+datum->token xml:string (cdr external-id)))]
                              [(xml:string? external-id)
                               (w3s-location+datum->token xml:string external-id)]
                              [else #false])

                        (xml-location+datum->dtd internal-dtd)
                        (and ?external-dtd (xml-location+datum->dtd ?external-dtd))
                        (and ?type (xml-location+datum->type ?type))

                        (map xml-location+datum->content contents))))


(define xml-location+datum->dtd : (-> XML-DTD-Datum XML-DTD)
  (lambda [doc.dtd]
    (xml-dtd (car doc.dtd) (map dtd-location+datum->declaration (cdr doc.dtd)))))

(define xml-location+datum->type : (-> XML-Type-Datum XML-Type)
  (lambda [type]
    (xml-type
     (for/hash : DTD-Entities ([n.e (in-list (vector-ref type 0))])
       (values (car n.e) (dtd-location+datum->entity (cdr n.e))))
     (for/hash : DTD-Notations ([n.n (in-list (vector-ref type 1))])
       (values (car n.n) (dtd-location+datum->notation (cdr n.n))))
     (for/hash : DTD-Elements ([n.e (in-list (vector-ref type 2))])
       (values (car n.e) (dtd-location+datum->element (cdr n.e))))
     (for/hash : DTD-Attributes ([e.al (in-list (vector-ref type 3))])
       (values (car e.al)
               (for/hash : (Immutable-HashTable Symbol DTD-Attribute) ([n.a (in-list (cdr e.al))])
                 (values (car n.a) (dtd-location+datum->attribute (cdr n.a)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-content->location+datum : (-> XML-Content* XML-Content-Datum)
  (lambda [c]
    (cond [(list? c) (xml-element->location+datum c)]
          [else (xml-pi->location+datum c)])))

(define xml-pi->location+datum : (-> XML-Processing-Instruction* XML-PI-Datum)
  (lambda [pi]
    (define pi-name (mcar pi))
    (define pi-body (mcdr pi))
    
    (xml-pi-datum (w3s-token->location+datum pi-name xml:name-datum)
                  (w3s-token->location+datum* pi-body xml:string-datum))))

(define xml-element->location+datum : (-> XML-Element* XML-Element-Datum)
  (lambda [e]
    (define tagname (car e))
    
    (list (w3s-token->location+datum tagname xml:name-datum)
          (map xml-attribute->location+datum (cadr e))
          (map xml-subdatum->location+datum (caddr e)))))

(define xml-attribute->location+datum : (-> XML-Element-Attribute* XML-Element-Attribute-Datum)
  (lambda [a]
    (define attr-name (car a))
    (define attr-value (cdr a))
   
    (cons (w3s-token->location+datum attr-name xml:name-datum)
          (cond [(xml:string? attr-value) (w3s-token->location+datum attr-value xml:string-datum)]
                [(xml:name? attr-value) (w3s-token->location+datum attr-value xml:name-datum)]
                [else (for/list : (Listof XML-Name) ([name (in-list attr-value)])
                        (w3s-token->location+datum name xml:name-datum))]))))

(define xml-subdatum->location+datum : (-> (U XML-Content* XML-Subdatum*) (U XML-Content-Datum XML-Subdatum-Datum))
  (lambda [c]
    (cond [(list? c) (xml-element->location+datum c)]
          [(xml-cdata-token? c) (w3s-token->location+datum c (assert (xml-cdata-token->datum c)))]
          [(xml:char? c) (w3s-token->location+datum c xml:char-datum)]
          [(xml:reference? c) (w3s-token->location+datum c xml:reference-datum)]
          [(mpair? c) (xml-pi->location+datum c)]
          [else w3s:token:datum:string:deadcode])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-location+datum->content : (-> XML-Content-Datum XML-Content*)
  (lambda [c]
    (cond [(list? c) (xml-location+datum->element c)]
          [else (xml-location+datum->pi c)])))

(define xml-location+datum->pi : (-> XML-PI-Datum XML-Processing-Instruction*)
  (lambda [pi]
    (define pi-name (xml-pi-datum-name pi))
    (define pi-body (xml-pi-datum-body pi))
    
    (mcons (w3s-location+datum->token xml:name pi-name)
           (and pi-body (xml-datum->string-token pi-body)))))

(define xml-location+datum->element : (-> XML-Element-Datum XML-Element*)
  (lambda [e]
    (define tagname (car e))
    
    (list (w3s-location+datum->token xml:name tagname)
          (map xml-location+datum->attribute (cadr e))
          (filter-map xml-location+datum->subdatum (caddr e)))))

(define xml-location+datum->attribute : (-> XML-Element-Attribute-Datum XML-Element-Attribute*)
  (lambda [a]
    (define attr-name (car a))
    (define attr-value (cdr a))

    (cons (w3s-location+datum->token xml:name attr-name)
          (if (list? attr-value)
              (for/list : (Listof XML:Name) ([name (in-list attr-value)])
                (w3s-location+datum->token xml:name name))
              (let ([payload (w3s-token-datum-payload attr-value)])
                (cond [(symbol? payload) (w3s-location+datum->token xml:name attr-value payload)]
                      [else (or (xml-datum->string-token* attr-value)
                                xml:string:token:deadcode)]))))))

(define xml-location+datum->subdatum : (-> (U XML-Content-Datum XML-Subdatum-Datum) (U XML-Content* XML-Subdatum*))
  (lambda [c]
    (cond [(list? c) (xml-location+datum->element c)]
          [(xml-pi-datum? c) (xml-location+datum->pi c)]
          [else (or (xml-datum->whitespace-token* c)
                    (xml-datum->string-token* c)
                    (w3s-location+datum->token* xml:char c index?)
                    (w3s-location+datum->token* xml:reference c symbol?)
                    xml:string:token:deadcode)])))

(define xml-datum->string-token : (-> XML-String XML:String)
  (lambda [s]
    (or (w3s-location+datum->token* xml:&string s)
        (w3s-location+datum->token xml:string s))))

(define xml-datum->string-token* : (-> (W3S-Token-Datumof Any) (Option XML:String))
  (lambda [s]
    (or (w3s-location+datum->token* xml:&string s string?)
        (w3s-location+datum->token* xml:string s string?))))

(define xml-datum->whitespace-token* : (-> (W3S-Token-Datumof Any) (Option XML:WhiteSpace))
  (lambda [s]
    (or (w3s-location+datum->token* xml:newline s string?)
        (w3s-location+datum->token* xml:comment s string?)
        (w3s-location+datum->token* xml:whitespace s string?))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dtd-declaration->location+datum : (-> DTD-Declaration* DTD-Declaration-Datum)
  (lambda [d]
    (cond [(dtd-entity? d) (dtd-entity->location+datum d)]
          [(dtd-attlist? d) (dtd-attlist->location+datum d)]
          [(dtd-element? d) (dtd-element->location+datum d)]
          [(xml:pereference? d) (w3s-token->location+datum d xml:pereference-datum)]
          [(list? d) (dtd-section->location+datum d)]
          [(dtd-notation? d) (dtd-notation->location+datum d)]
          [(vector? d) (dtd-raw-declaration->location+datum d)]
          [else (xml-pi->location+datum d)])))

(define dtd-raw-declaration->location+datum : (-> DTD-Raw-Declaration* DTD-Raw-Declaration-Datum)
  (lambda [rd]
    (vector-immutable (w3s-token->location+datum (vector-ref rd 0) xml:name-datum)
                      (for/list : (Listof (W3S-Token-Datumof Any)) ([t (in-list (vector-ref rd 1))])
                        (cond [(xml:pereference? t) (w3s-token->location+datum t xml:pereference-datum)]
                              [(xml:name? t) (w3s-token->location+datum t xml:name-datum)]
                              [(xml:string? t) (w3s-token->location+datum t xml:string-datum)]
                              [(xml:delim? t) (w3s-token->location+datum t xml:delim-datum)]
                              [else w3s:token:datum:string:deadcode])))))

(define dtd-section->location+datum : (-> (Pairof (U XML:Name XML:PEReference) (Listof DTD-Declaration*))
                                          (Pairof (U XML-Name XML-PEReference) (Listof DTD-Declaration-Datum)))
  (lambda [cs]
    (define condition (car cs))
    
    (cons (if (xml:name? condition)
              (w3s-token->location+datum condition xml:name-datum)
              (w3s-token->location+datum condition xml:pereference-datum))
          (map dtd-declaration->location+datum (cdr cs)))))

(define dtd-entity->location+datum : (-> DTD-Entity DTD-Entity-Datum)
  (lambda [e]
    (define name
      (let ([n (dtd-entity-name e)])
        (if (xml:reference? n)
            (w3s-token->location+datum n xml:reference-datum)
            (w3s-token->location+datum n xml:pereference-datum))))
    
    (cond [(dtd-internal-entity? e)
           (dtd-entity-datum name (w3s-token->location+datum (dtd-internal-entity-value e) xml:string-datum) #false #false #false)]
          [(dtd-external-entity? e)
           (dtd-entity-datum name #false
                             (w3s-token->location+datum* (dtd-external-entity-public e) xml:string-datum)
                             (w3s-token->location+datum* (dtd-external-entity-system e) xml:string-datum)
                             (w3s-token->location+datum* (and (dtd-unparsed-entity? e) (dtd-unparsed-entity-ndata e))
                                                         xml:name-datum))]
          [else (dtd-entity-datum name w3s:token:datum:string:deadcode #false #false #false)])))

(define dtd-attlist->location+datum : (-> DTD-AttList DTD-AttList-Datum)
  (lambda [al]
    (dtd-attlist-datum (w3s-token->location+datum (dtd-attlist-element al) xml:name-datum)
                       (map dtd-attribute->location+datum (dtd-attlist-body al)))))

(define dtd-attribute->location+datum : (-> DTD-Attribute DTD-Attribute-Datum)
  (lambda [a]
    (define element (w3s-token->location+datum (dtd-attribute-element a) xml:name-datum))
    (define name (w3s-token->location+datum (dtd-attribute-name a) xml:name-datum))

    (define-values (type flag)
      (cond [(dtd-attribute-token-type? a)
             (values (w3s-token->location+datum (dtd-attribute-token-type-name a) xml:name-datum)
                     (dtd-attribute-token-type-names? a))]
            [(dtd-attribute-enum-type? a)
             (values (let ([opts (dtd-attribute-enum-type-options)])
                       (cons (w3s-token->location+datum (car opts) xml:name-datum)
                             (for/list ([o (in-list (cdr opts))])
                               (w3s-token->location+datum o xml:name-datum))))
                     (dtd-attribute-enum-type-notation? a))]
            [else (values #false #false)]))
    
    (if (dtd-attribute+default? a)
        (dtd-attribute-datum element name type flag
                             (w3s-token->location+datum (dtd-attribute+default-value a) xml:string-datum)
                             (dtd-attribute+default-fixed? a))
        (dtd-attribute-datum element name type flag
                             (dtd-attribute/required? a)
                             #false))))

(define dtd-notation->location+datum : (-> DTD-Notation DTD-Notation-Datum)
  (lambda [n]
    (dtd-notation-datum (w3s-token->location+datum (dtd-notation-name n) xml:name-datum)
                        (w3s-token->location+datum* (dtd-notation-public n) xml:string-datum)
                        (w3s-token->location+datum* (dtd-notation-system n) xml:string-datum))))

(define dtd-element->location+datum : (-> DTD-Element DTD-Element-Datum)
  (lambda [e]
    (dtd-element-datum (w3s-token->location+datum (dtd-element-name e) xml:name-datum)
                       (cond [(dtd-mixed-element? e)
                              (vector->immutable-vector
                               (for/vector : (Vectorof XML-Name) ([name (in-list (dtd-mixed-element-children e))])
                                 (w3s-token->location+datum name xml:name-datum)))]
                             [(dtd-element+children? e)
                              (let ([child (dtd-element+children-content e)])
                                (cons (cdr child)
                                      (dtd-children->location+datum (car child))))]
                             [else (not (dtd-empty-element? e))]))))

(define dtd-children->location+datum : (-> (U DTD-Element-Sequence DTD-Element-Choice) (U DTD-Element-Sequence-Datum DTD-Element-Choice-Datum))
  (lambda [child]
    (if (vector? child)
        (dtd-sequence->location+datum child)
        (dtd-choice->location+datum child))))

(define dtd-sequence->location+datum : (-> DTD-Element-Sequence DTD-Element-Sequence-Datum)
  (lambda [seq]
    (vector->immutable-vector
     (for/vector : (Vectorof (Pairof Char (U XML-Name DTD-Element-Children-Datum))) ([c (in-vector seq)])
       (cons (cdr c)
             (let ([v (car c)])
               (if (xml:name? v)
                   (w3s-token->location+datum v xml:name-datum)
                   (dtd-children->location+datum v))))))))

(define dtd-choice->location+datum : (-> DTD-Element-Choice DTD-Element-Choice-Datum)
  (lambda [opt]
    (for/list : (Listof (Pairof Char (U XML-Name DTD-Element-Children-Datum))) ([s (in-list opt)])
      (cons (cdr s)
            (let ([v (car s)])
             (if (xml:name? v)
                 (w3s-token->location+datum v xml:name-datum)
                 (dtd-children->location+datum v)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dtd-location+datum->declaration : (-> DTD-Declaration-Datum DTD-Declaration*)
  (lambda [d]
    (cond [(dtd-entity-datum? d) (dtd-location+datum->entity d)]
          [(dtd-attlist-datum? d) (dtd-location+datum->attlist d)]
          [(dtd-element-datum? d) (dtd-location+datum->element d)]
          [(w3s-token-datum? d) (w3s-location+datum->token xml:pereference d)]
          [(list? d) (dtd-location+datum->section d)]
          [(dtd-notation-datum? d) (dtd-location+datum->notation d)]
          [(vector? d) (dtd-location+datum->raw-declaration d)]
          [else (xml-location+datum->pi d)])))

(define dtd-location+datum->raw-declaration : (-> DTD-Raw-Declaration-Datum DTD-Raw-Declaration*)
  (lambda [rd]
    (vector-immutable (w3s-location+datum->token xml:name (vector-ref rd 0))
                      (for/list : (Listof XML-Token) ([t (in-list (vector-ref rd 1))])
                        (or (w3s-location+datum->token* xml:pereference t keyword?)
                            (w3s-location+datum->token* xml:delim t char?)
                            (and (w3s-token-datum-typeof? t struct:xml:name)
                                 (w3s-location+datum->token* xml:name t symbol?))
                            (and (w3s-token-datum-typeof? t struct:xml:&string)
                                 (w3s-location+datum->token* xml:&string t string?))
                            (and (w3s-token-datum-typeof? t struct:xml:string)
                                 (w3s-location+datum->token* xml:string t string?))
                            (w3s-location+datum->token* xml:delim t symbol?)
                            xml:string:token:deadcode)))))

(define dtd-location+datum->section : (-> (Pairof (U XML-Name XML-PEReference) (Listof DTD-Declaration-Datum))
                                          (Pairof (U XML:Name XML:PEReference) (Listof DTD-Declaration*)))
  (lambda [cs]
    (define condition (car cs))
    
    (cons (or (w3s-location+datum->token* xml:pereference condition keyword?)
              (w3s-location+datum->token* xml:name condition symbol?)
              xml:pereference:token:deadcode)
          (map dtd-location+datum->declaration (cdr cs)))))

(define dtd-location+datum->entity : (-> DTD-Entity-Datum DTD-Entity)
  (lambda [e]
    (define name : (U XML:Reference XML:PEReference)
      (let ([n (dtd-entity-datum-name e)])
        (or (w3s-location+datum->token* xml:reference n symbol?)
            (w3s-location+datum->token* xml:pereference n keyword?)
            xml:pereference:token:deadcode)))
    
    (cond [(dtd-entity-datum-value e) (dtd-internal-entity name (xml-datum->string-token (dtd-entity-datum-value e)))]
          [else (let ([public (w3s-location+datum->token* xml:string (dtd-entity-datum-public e))]
                      [system (w3s-location+datum->token* xml:string (dtd-entity-datum-system e))]
                      [ndata (dtd-entity-datum-ndata e)])
                  (if (not ndata)
                      (dtd-external-entity name public system)
                      (dtd-unparsed-entity name public system (w3s-location+datum->token xml:name ndata))))])))

(define dtd-location+datum->attlist : (-> DTD-AttList-Datum DTD-AttList)
  (lambda [al]
    (define target-element (w3s-location+datum->token xml:name (dtd-attlist-datum-element al)))
    
    (dtd-attlist target-element
                 (map dtd-location+datum->attribute (dtd-attlist-datum-body al)))))

(define dtd-location+datum->attribute : (-> DTD-Attribute-Datum DTD-Attribute)
  (lambda [a]
    (define element (w3s-location+datum->token xml:name (dtd-attribute-datum-element a)))
    (define name (w3s-location+datum->token xml:name (dtd-attribute-datum-name a)))
    (define defval (dtd-attribute-datum-default a))
    (define fixed? (dtd-attribute-datum-fixed? a))
    
    (define type
      (let ([type (dtd-attribute-datum-type a)]
            [flag (dtd-attribute-datum-typeflag a)])
        (cond [(vector? type)
               (dtd-attribute-token-type (w3s-location+datum->token xml:name type) flag)]
              [(pair? type)
               (dtd-attribute-enum-type
                (cons (w3s-location+datum->token xml:name (car type))
                      (for/list : (Listof XML:Name) ([o (in-list (cdr type))])
                        (w3s-location+datum->token xml:name o)))
                flag)]
              [else dtd:attribute:cdata])))
    
    (cond [(vector? defval) (dtd-attribute+default element name type (xml-datum->string-token defval) fixed?)]
          [(eq? defval #true) (dtd-attribute/required element name type)]
          [else (dtd-attribute element name type)])))

(define dtd-location+datum->notation : (-> DTD-Notation-Datum DTD-Notation)
  (lambda [n]
    (dtd-notation (w3s-location+datum->token xml:name (dtd-notation-datum-name n))
                  (w3s-location+datum->token* xml:string (dtd-notation-datum-public n))
                  (w3s-location+datum->token* xml:string (dtd-notation-datum-system n)))))

(define dtd-location+datum->element : (-> DTD-Element-Datum DTD-Element)
  (lambda [e]
    (define name (w3s-location+datum->token xml:name (dtd-element-datum-name e)))
    (define body (dtd-element-datum-body e))

    (cond [(vector? body)
           (dtd-mixed-element name
                              (for/list : (Listof XML:Name) ([name (in-vector body)])
                                (w3s-location+datum->token xml:name name)))]
          [(pair? body)
           (dtd-element+children name
                                 (cons (dtd-location+datum->children (cdr body))
                                       (car body)))]
          [(not body) (dtd-empty-element name)]
          [else (dtd-element name)])))

(define dtd-location+datum->children : (-> (U DTD-Element-Sequence-Datum DTD-Element-Choice-Datum) (U DTD-Element-Sequence DTD-Element-Choice))
  (lambda [child]
    (if (vector? child)
        (dtd-location+datum->sequence child)
        (dtd-location+datum->choice child))))

(define dtd-location+datum->sequence : (-> DTD-Element-Sequence-Datum DTD-Element-Sequence)
  (lambda [seq]
    (vector->immutable-vector
     (for/vector : (Vectorof (Pairof (U XML:Name DTD-Element-Children) Char)) ([c (in-vector seq)])
       (cons (let ([v (cdr c)])
               (if (w3s-token-datum? v)
                   (w3s-location+datum->token xml:name v)
                   (dtd-location+datum->children v)))
             (car c))))))

(define dtd-location+datum->choice : (-> DTD-Element-Choice-Datum DTD-Element-Choice)
  (lambda [opt]
    (for/list : (Listof (Pairof (U XML:Name DTD-Element-Children) Char)) ([s (in-list opt)])
      (cons (let ([v (cdr s)])
             (if (w3s-token-datum? v)
                 (w3s-location+datum->token xml:name v)
                 (dtd-location+datum->children v)))
            (car s)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define w3s:token:datum:string:deadcode (make-w3s-location+datum "DEADCODE"))
(define w3s:token:datum:pereference:deadcode (make-w3s-location+datum '#:DEADCODE))

(define xml:string:token:deadcode (w3s-location+datum->token xml:string w3s:token:datum:string:deadcode))
(define xml:pereference:token:deadcode (w3s-location+datum->token xml:pereference w3s:token:datum:pereference:deadcode))
