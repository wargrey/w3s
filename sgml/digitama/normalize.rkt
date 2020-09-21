#lang typed/racket/base

(provide (all-defined-out))

(require typed/racket/unsafe)

(require racket/symbol)

(require "grammar.rkt")
(require "dtd.rkt")

(require "digicore.rkt")
(require "tokenizer.rkt")
(require "prentity.rkt")
(require "stdin.rkt")

(unsafe-require/typed
 racket/unsafe/ops
 [unsafe-string-ref (-> String Index Char)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type XML-Space-Position (U 'head 'body 'tail 'span))

(define-type XML:Space-Filter
  (case-> [Symbol (Option String) Char -> (Option Char)]
          [Symbol (Option String) String (Option String) XML-Space-Position Boolean -> (Option String)]))

(define svg:space-filter : XML:Space-Filter
  (case-lambda
    [(tag xml:lang ch) #\space]
    [(tag xml:lang raw-spaces default-replace position has-newline?)
     (cond [(not has-newline?) default-replace]
           [else (and (eq? position 'body)
                      (let ([size (string-length raw-spaces)])
                        (let svg-check-space ([idx : Nonnegative-Fixnum 0])
                          (and (< idx size)
                               (if (eq? (unsafe-string-ref raw-spaces idx) #\linefeed)
                                   (svg-check-space (+ idx 1))
                                   default-replace)))))])]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-normalize : (-> XML-DTD (Listof XML-Content*) String Symbol (Option XML:Space-Filter) (Values XML-Type (Listof XML-Content*)))
  (lambda [int-dtd content xml:lang xml:space xml:space-filter]
    (define dtype : XML-Type (xml-dtd-expand int-dtd))
    
    (parameterize ([default-xml:space xml:space])
      (let xml-content-normalize ([rest : (Listof XML-Content*) content]
                                  [clear-content : (Listof XML-Content*) null])
        (cond [(null? rest) (values dtype (reverse clear-content))]
              [else (let-values ([(self rest++) (values (car rest) (cdr rest))])
                      (cond [(list? self)
                             (let ([?elem (xml-element-normalize self dtype (xml:lang-ref xml:lang) xml:space xml:space-filter 0)])
                               (xml-content-normalize rest++ (if (exn:xml:loop? ?elem) clear-content (cons ?elem clear-content))))]
                            [else (xml-content-normalize rest++ (cons self clear-content))]))])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-dtd-entity-expand : (-> XML-DTD XML-Type-Entities)
  (lambda [dtd]
    (let expand-dtd ([rest : (Listof XML-Type-Declaration*) (xml-dtd-declarations dtd)]
                     [entities : XML-Type-Entities (make-immutable-hasheq)])
      (cond [(null? rest) entities]
            [else (let-values ([(self rest++) (values (car rest) (cdr rest))])
                    (cond [(xml-entity? self)
                           ; NOTE: NDATA only concerns validity constraint, and therefore is not handled here.
                           (if (xml-entity-value self)
                               (expand-dtd rest++ (xml-entity-cons (xml-dtd-included-as-literal self entities) entities))
                               (expand-dtd rest++ (xml-entity-cons self entities)))]
                          [(xml:pereference? self) (expand-dtd (append (xml-dtd-included-as-PE self entities) rest++) entities)]
                          [(pair? self) (expand-dtd (append (xml-dtd-expand-section (car self) (cdr self) entities) rest++) entities)]
                          [else (expand-dtd rest++ entities)]))]))))

(define xml-dtd-expand : (-> XML-DTD XML-Type)
  (lambda [dtd]
    (let expand-dtd ([rest : (Listof XML-Type-Declaration*) (xml-dtd-declarations dtd)]
                     [entities : XML-Type-Entities (make-immutable-hasheq)]
                     [notations : XML-Type-Notations (make-immutable-hasheq)]
                     [elements : XML-Type-Elements (make-immutable-hasheq)]
                     [attributes : XML-Type-Attributes (make-immutable-hasheq)])
      (cond [(null? rest) (xml-type entities notations elements attributes)]
            [else (let-values ([(self rest++) (values (car rest) (cdr rest))])
                    (cond [(xml-entity? self)
                           ; NOTE: NDATA only concerns validity constraint, and therefore is not handled here. 
                           (if (xml-entity-value self)
                               (expand-dtd rest++ (xml-entity-cons (xml-dtd-included-as-literal self entities) entities) notations elements attributes)
                               (expand-dtd rest++ (xml-entity-cons self entities) notations elements attributes))]
                          [(xml-element-content? self) (expand-dtd rest++ entities notations (xml-element-cons self elements) attributes)]
                          [(xml-attribute-list? self) (expand-dtd rest++ entities notations elements (xml-attributes-cons self attributes))]
                          [(xml:pereference? self) (expand-dtd (append (xml-dtd-included-as-PE self entities) rest++) entities notations elements attributes)]
                          [(pair? self) (expand-dtd (append (xml-dtd-expand-section (car self) (cdr self) entities) rest++) entities notations elements attributes)]
                          [(xml-notation? self) (expand-dtd rest++ entities (xml-notation-cons self notations) elements attributes)]
                          [else (or (and (vector? self)
                                         (let ([DECL (vector-ref self 0)])
                                           (case (xml:name-datum DECL)
                                             [(ATTLIST)
                                              (let ([?attr (xml-dtd-expand-declaration xml-dtd-extract-attributes* DECL (vector-ref self 1) entities)])
                                                (and (xml-attribute-list? ?attr)
                                                     (expand-dtd rest++ entities notations elements (xml-attributes-cons ?attr attributes))))]
                                             [(ELEMENT)
                                              (let ([?elem (xml-dtd-expand-declaration xml-dtd-extract-element* DECL (vector-ref self 1) entities)])
                                                (and (xml-element-content? ?elem)
                                                     (expand-dtd rest++ entities notations (xml-element-cons ?elem elements) attributes)))]
                                             [else #false])))
                                    (expand-dtd rest++ entities notations elements attributes))]))]))))

(define xml-dtd-expand-section : (-> (U XML:Name XML:PEReference) (Listof XML-Type-Declaration*) XML-Type-Entities (Listof XML-Type-Declaration*))
  (lambda [condition body entities]
    (define sec-name : (U False Symbol String)
      (cond [(xml:name? condition) (xml:name-datum condition)]
            [else (let-values ([(?value ge?) (xml-entity-value-ref condition (xml:pereference-datum condition) entities)])
                    ?value)]))

    (cond [(or (eq? sec-name 'INCLUDE) (equal? sec-name "INCLUDE")) body]
          [(or (eq? sec-name 'IGNORE) (equal? sec-name "IGNORE")) null]
          [else null])))

(define xml-dtd-expand-declaration : (All (T) (-> (-> XML:Name (Listof XML-Doctype-Body*) T) XML:Name (Listof XML-Doctype-Body*) XML-Type-Entities T))
  (lambda [extract DECL body entities]
    (extract DECL
             (let expand-body ([rest : (Listof XML-Doctype-Body*) body]
                               [ydob : (Listof XML-Doctype-Body*) null])
               (cond [(null? rest) (reverse ydob)]
                     [else (let-values ([(self rest++) (values (car rest) (cdr rest))])
                             (if (xml:pereference? self)
                                  ; TODO: DECL might affect the parser in some rare cases
                                 (let* ([pevalue->tokens (make-entity-value->tokens (xml:name-datum DECL) read-dtd-declaration-tokens*)]
                                        [?tokens (xml-entity-value-tokens-ref self (xml:pereference-datum self) entities pevalue->tokens)])
                                   (expand-body (if (not ?tokens) rest++ (append ?tokens rest++)) ydob))
                                 (expand-body rest++ (cons self ydob))))])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-dtd-included-as-literal : (-> XML-Entity XML-Type-Entities (Option XML-Entity))
  ;;; https://www.w3.org/TR/xml11/#inliteral
  ;;; https://www.w3.org/TR/xml11/#bypass
  (lambda [e entities]
    (define ?value (xml-entity-value e))
    
    (cond [(not (xml:&string? ?value)) e]
          [else (let ([?new-value (xml-dtd-entity-replace (xml-entity-name e) ?value entities)])
                  (and ?new-value
                       (struct-copy xml-entity e
                                    [value ?new-value])))])))

(define xml-dtd-included-as-PE : (-> XML:PEReference XML-Type-Entities (Listof XML-Type-Declaration*))
  ;;; https://www.w3.org/TR/xml11/#as-PE
  (lambda [pe entities]
    (define pentity->tokens : (-> XML:String (Listof XML-Token)) (make-entity-value->tokens #false read-xml-tokens*))
    (define ?tokens : (Option (Listof XML-Token)) (xml-entity-value-tokens-ref pe (xml:pereference-datum pe) entities pentity->tokens))

    (cond [(not ?tokens) null]
          [else (let ([subset (xml-syntax->definition* ?tokens)])
                  (xml-dtd-definitions->declarations subset #true))])))

(define xml-dtd-included : (-> XML:Name XML:Reference XML-Type-Entities Index (Listof (U XML-Subdatum* XML-Element*)))
  ;;; https://www.w3.org/TR/xml11/#included
  (lambda [tagname e entities depth]
    (define name : Symbol (xml:reference-datum e))
    (define prentity-value : (Option String) (xml-prentity-value-ref name))

    (cond [(and prentity-value) (list (w3s-remake-token e xml:string prentity-value))]
          [else (let* ([gvalue->tokens (make-entity-value->tokens depth read-xml-content-tokens*)]
                       [?tokens (xml-entity-value-tokens-ref e name entities gvalue->tokens tagname)])
                  (cond [(not ?tokens) null]
                        [else (let-values ([(children rest) (xml-syntax-extract-subelement* tagname ?tokens #true)])
                                (cond [(and children (null? rest)) children]
                                      [else (make+exn:xml:malformed e tagname) null]))]))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-element-normalize : (->* (XML-Element* XML-Type (Option String) Symbol (Option XML:Space-Filter) Index) ((Listof Symbol)) (U XML-Element* exn:xml:loop))
  (lambda [e dtype xml:lang xml:space xml:filter depth [recursion-stack null]]
    (define entities : XML-Type-Entities (xml-type-entities dtype))
    (define tagname : XML:Name (car e))
    
    (define-values (setubirtta xml:this:lang xml:this:space)
      (let attribute-filter-map : (Values (Listof (Pairof XML:Name XML:String)) (Option String) Symbol)
        ([rest : (Listof (Pairof XML:Name XML:String)) (cadr e)]
         [setubirtta : (Listof (Pairof XML:Name XML:String)) null]
         [names : (Listof Symbol) null]
         [lang : (Option String) xml:lang]
         [space : Symbol xml:space])
        (if (pair? rest)
            (let*-values ([(self rest++) (values (car rest) (cdr rest))]
                          [(?attr) (xml-element-attribute-normalize self entities names tagname)])
              (cond [(not ?attr) (attribute-filter-map rest++ setubirtta names lang space)]
                    [else (let ([attr-name (xml:name-datum (car ?attr))])
                            (attribute-filter-map rest++ (cons ?attr setubirtta) (cons attr-name names)
                                                  (if (eq? attr-name 'xml:lang) (xml:lang-ref tagname ?attr) lang)
                                                  (if (eq? attr-name 'xml:space) (xml:space-ref tagname ?attr space) space)))]))
            (for/fold ([setubirtta : (Listof (Pairof XML:Name XML:String)) setubirtta]
                       [this:lang : (Option String) lang]
                       [this:space : Symbol space])
                      ([(key attr) (in-hash (hash-ref (xml-type-attributes dtype) (xml:name-datum tagname) (λ [] empty-attributes)))]
                       #:unless (memq key names))
              (let ([?attr (xml-element-attribute-normalize attr entities names)])
                (cond [(not ?attr) (values setubirtta this:lang this:space)]
                      [else (let ([attname (xml-attribute-element attr)])
                              (values (cons ?attr setubirtta)
                                      (if (eq? key 'xml:lang) (xml:lang-ref attname ?attr) this:lang)
                                      (if (eq? key 'xml:space) (xml:space-ref attname ?attr space) this:space)))]))))))

    (define ?children : (U (Listof (U XML-Subdatum* XML-Element*)) exn:xml:loop)
      (xml-subelement-normalize tagname (caddr e) dtype
                                xml:this:lang xml:this:space xml:filter
                                (assert (+ depth 1) index?) recursion-stack))
    
    (cond [(list? ?children) (list tagname (reverse setubirtta) ?children)]
          [else ?children])))

(define xml-element-attribute-normalize : (case-> [(Pairof XML:Name XML:String) XML-Type-Entities (Listof Symbol) XML-Token -> (Option (Pairof XML:Name XML:String))]
                                                  [XML-Attribute XML-Type-Entities (Listof Symbol) -> (Option (Pairof XML:Name XML:String))])
  (case-lambda
    [(name=value entities attrnames tagname)
     (let ([name (car name=value)]
           [value (cdr name=value)])
       (cond [(memq (xml:name-datum name) attrnames) (make+exn:xml:unique name tagname) #false]
             [(not (xml:&string? value)) name=value]
             [else (let ([?value (xml-attr-entity-replace name value entities)])
                     (and (xml:string? ?value)
                          (cons name ?value)))]))]
    [(attr entities attrnames)
     (let ([name (xml-attribute-name attr)]
           [value (xml-attribute-default attr)])
       (cond [(or (memq (xml:name-datum name) attrnames) (xml:name? value)) #false]
             [(not (xml:&string? value)) (cons name value)]
             [else (let ([?value (xml-attr-entity-replace name value entities)])
                     (and (xml:string? ?value)
                          (cons name ?value)))]))]))

(define xml-subelement-normalize : (-> XML:Name (Listof (U XML-Subdatum* XML-Element*)) XML-Type (Option String) Symbol (Option XML:Space-Filter) Index (Listof Symbol)
                                       (U (Listof (U XML-Subdatum* XML-Element*)) exn:xml:loop))
  (lambda [tagname children dtype xml:lang xml:space xml:filter depth recursion-stack]
    (define tag : Symbol (xml:name-datum tagname))
    
    (let normalize-subelement ([rest : (Listof (U XML-Subdatum* XML-Element*)) children]
                               [nerdlihc : (Listof (U XML-Subdatum* XML-Element*)) null]
                               [secaps : (Listof XML:WhiteSpace) null]
                               [rstack : (Listof Symbol) recursion-stack])
      (cond [(pair? rest)
             (let-values ([(self rest++) (values (car rest) (cdr rest))])
               (cond [(list? self)
                      (let ([?elem (xml-element-normalize self dtype xml:lang xml:space xml:filter depth rstack)])
                        (cond [(list? ?elem) (normalize-subelement rest++ (cons ?elem (xml-child-cons secaps nerdlihc xml:filter tag xml:lang)) null rstack)]
                              [else ?elem]))]
                     [(xml:reference? self)
                      (let ([selfname (xml:reference-datum self)])
                        (cond [(memq selfname rstack) (make+exn:xml:loop self tagname)]
                              [else (let*-values ([(air0) (xml-dtd-included tagname self (xml-type-entities dtype) depth)]
                                                  [(?lspace air-elements ?tspace) (xml-air-elements-trim air0)]
                                                  [(nerdlihc++) (xml-child-cons (if (not ?lspace) secaps (cons ?lspace secaps)) nerdlihc xml:filter tag xml:lang)]
                                                  [(sp.rest++) (if (not ?tspace) rest++ (cons ?tspace rest++))]
                                                  [(?fly-children) (normalize-subelement air-elements null null (cons selfname rstack))])
                                      (cond [(list? ?fly-children) (normalize-subelement sp.rest++ (append (reverse ?fly-children) nerdlihc++) null rstack)]
                                            [(null? rstack) (normalize-subelement rest++ nerdlihc secaps rstack)]
                                            [else ?fly-children]))]))]
                     [(xml:char? self)
                      (let ([content (w3s-remake-token self xml:string (string (integer->char (xml:char-datum self))))])
                        (normalize-subelement rest++ (cons content (xml-child-cons secaps nerdlihc xml:filter tag xml:lang)) null rstack))]
                     [(not (xml:whitespace? self)) (normalize-subelement rest++ (cons self (xml-child-cons secaps nerdlihc xml:filter tag xml:lang)) null rstack)]
                     [(xml:comment? self) (normalize-subelement rest++ nerdlihc secaps rstack)]
                     [(eq? xml:space 'preserve) (normalize-subelement rest++ (cons (xml:space=preserve tag self xml:filter xml:lang) nerdlihc) secaps rstack)]
                     [else (normalize-subelement rest++ nerdlihc (cons self secaps) rstack)]))]
            [else (let cdata-reverse ([rest : (Listof (U XML-Subdatum* XML-Element*)) (xml-child-cons secaps nerdlihc xml:filter tag xml:lang #true)]
                                      [children : (Listof (U XML-Subdatum* XML-Element*)) null]
                                      [cdatas : (Listof XML-CDATA-Token) null]
                                      [start-token : (Option XML-CDATA-Token) #false]
                                      [end-token : (Option XML-CDATA-Token) #false])
                    (cond [(null? rest) (xml-cdata-cons cdatas start-token end-token children)]
                          [else (let-values ([(self rest++) (values (car rest) (cdr rest))])
                                  (cond [(xml-cdata-token? self) (cdata-reverse rest++ children (cons self cdatas) self (or end-token self))]
                                        [else (cdata-reverse rest++ (cons self (xml-cdata-cons cdatas start-token end-token children)) null #false #false)]))]))]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NOTE
;; The use context of Parameter Entity and General Entity has no overlaps.
;; The PEs are used in DTD where GEs should be bypassed, and are expanded immediately, and therefore no recursions;
;; The GEs are used in element attributes or content where PE should not be recognized;
;;
;; Besides, Char references are expanded immediately in all contexts.

(define xml-dtd-entity-replace : (-> XML-Token XML:&String XML-Type-Entities (Option XML:String))
  (let ([/dev/evout (open-output-string '/dev/evout)])
    (lambda [ename etoken entities]
      (define src : String (xml:string-datum etoken))
      (define size : Index (string-length src))
      (define false-idx : Nonnegative-Fixnum (+ size 1))
      
      (let dtd-entity-normalize ([idx : Nonnegative-Fixnum 0]
                                 [leader : (Option Char) #false]
                                 [srahc : (Listof Char) null]
                                 [gexists? : Boolean #false])
          (cond [(< idx size)
                 (let ([ch (unsafe-string-ref src idx)])
                   (define idx++ : Nonnegative-Fixnum (+ idx 1))

                   (when (eq? ch #\<)
                     (make+exn:xml:char etoken ename 'debug))
                   
                   (if (not leader)
                       (cond [(or (eq? ch #\%) (eq? ch #\&)) (dtd-entity-normalize idx++ ch srahc gexists?)]
                             [else (write-char ch /dev/evout) (dtd-entity-normalize idx++ leader srahc gexists?)])
                       (cond [(not (eq? ch #\;)) (dtd-entity-normalize idx++ leader (cons ch srahc) gexists?)]
                             [(null? srahc) (make+exn:xml:malformed etoken ename) (dtd-entity-normalize false-idx leader srahc gexists?)]
                             [else (let ([estr (list->string (reverse srahc))])
                                     (cond [(eq? leader #\%)
                                            (let-values ([(replacement ge?) (xml-entity-value-ref etoken (string->keyword estr) entities ename)])
                                              (cond [(not replacement) (dtd-entity-normalize false-idx #false null gexists?)]
                                                    [else (write-string replacement /dev/evout)
                                                          (dtd-entity-normalize idx++ #false null (or ge? gexists?))]))]
                                             [(eq? (unsafe-string-ref estr 0) #\#)
                                              (let ([replacement (xml-char-unreference etoken estr ename)])
                                                (cond [(not replacement) (dtd-entity-normalize false-idx #false null gexists?)]
                                                      [else (write-char replacement /dev/evout)
                                                            (dtd-entity-normalize idx++ #false null gexists?)]))]
                                             [else (write-char #\& /dev/evout)
                                                   (write-string estr /dev/evout)
                                                   (write-char #\; /dev/evout)
                                                   (dtd-entity-normalize idx++ #false null #true)]))])))]
                [(= idx size)
                 (let ([new-value : String (bytes->string/utf-8 (get-output-bytes /dev/evout #true) #\uFFFD)])
                   (cond [(string=? new-value src) etoken]
                         [(not gexists?) (w3s-remake-token etoken xml:string new-value)]
                         [else (w3s-remake-token etoken xml:&string new-value)]))]
                
                [else (not (get-output-bytes /dev/evout #true))])))))

(define xml-attr-entity-replace : (->* (XML:Name XML:String XML-Type-Entities) ((Listof Symbol) (Listof Char)) (U XML:String (Listof Char) False))
  ;;; https://www.w3.org/TR/xml11/#sec-line-ends
  ;;; https://www.w3.org/TR/xml11/#AVNormalize
  (lambda [attname vtoken entities [rstack : (Listof Symbol) null] [prev-eulav : (Listof Char) null]]
    (define src : String (xml:string-datum vtoken))
    (define size : Index (string-length src))
    (define false-idx : Nonnegative-Fixnum (+ size 1))
    
    (let attr-value-normalize ([idx : Nonnegative-Fixnum 0]
                               [leader : (Option Symbol) #false]
                               [srahc : (Listof Char) null]
                               [eulav : (Listof Char) prev-eulav])
      (cond [(< idx size)
             (let ([ch (unsafe-string-ref src idx)])
               (define idx++ : Nonnegative-Fixnum (+ idx 1))
               
               (cond [(eq? leader '&)
                      (cond [(not (eq? ch #\;)) (attr-value-normalize idx++ leader (cons ch srahc) eulav)]
                            [(null? srahc) (make+exn:xml:malformed vtoken attname) (attr-value-normalize false-idx leader srahc eulav)]
                            [else (let ([estr (list->string (reverse srahc))])
                                    (if (eq? (unsafe-string-ref estr 0) #\#)
                                        (let ([replacement (xml-char-unreference vtoken estr attname)])
                                          (cond [(not replacement) (attr-value-normalize false-idx #false null eulav)]
                                                [else (attr-value-normalize idx++ #false null (cons replacement eulav))]))
                                        (let ([ename (string->unreadable-symbol estr)])
                                          (cond [(memv ename rstack) (make+exn:xml:loop vtoken attname) (attr-value-normalize false-idx #false null eulav)]
                                                [else (let ([etoken (xml-entity-value-token-ref vtoken ename entities attname)])
                                                        (cond [(not etoken) (attr-value-normalize false-idx #false null eulav)]
                                                              [else (let ([raction (xml-attr-entity-replace attname etoken entities (cons ename rstack) eulav)])
                                                                      (cond [(list? raction) (attr-value-normalize idx++ #false null raction)]
                                                                            [else (attr-value-normalize false-idx #false null eulav)]))]))]))))])]

                     ; NOTE: the EOL handling and attribute normalization conincide here.
                     [(eq? leader 'xD)
                      (if (or (eq? ch #\newline) (eq? ch #\u0085))
                          (attr-value-normalize idx++ #false srahc (cons #\space eulav))
                          (attr-value-normalize idx #false srahc (cons #\space eulav)))]
                     [(eq? ch #\&) (attr-value-normalize idx++ '& srahc eulav)]
                     [(eq? ch #\return) (attr-value-normalize idx++ 'xD srahc eulav)]
                     #;[(eq? ch #\u0085) (attr-value-normalize idx++ leader srahc (cons #\space eulav))]
                     #;[(eq? ch #\u2028) (attr-value-normalize idx++ leader srahc (cons #\space eulav))]
                     [(eq? ch #\newline) (attr-value-normalize idx++ leader srahc (cons #\space eulav))]
                     ; End EOL
                     
                     [else (attr-value-normalize idx++ leader srahc (cons ch eulav))]))]
            
            [(= idx size)
             (cond [(null? rstack) (w3s-remake-token vtoken xml:string (list->string (reverse eulav)))]
                   [else eulav])]
            [else #false]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-entity-cons : (-> (Option XML-Entity) XML-Type-Entities XML-Type-Entities)
  (lambda [e entities]
    (cond [(not e) entities]
          [else (let* ([ntoken (xml-entity-name e)]
                       [name (if (xml:reference? ntoken) (xml:reference-datum ntoken) (xml:pereference-datum ntoken))])
                  (cond [(not (hash-has-key? entities name)) (hash-set entities name e)]
                        [else (make+exn:xml:duplicate ntoken) entities]))])))

(define xml-notation-cons : (-> XML-Notation XML-Type-Notations XML-Type-Notations)
  (lambda [n notations]
    (let* ([ntoken (xml-notation-name n)]
           [name (xml:name-datum ntoken)])
      (cond [(not (hash-has-key? notations name)) (hash-set notations name n)]
            [else (make+exn:xml:duplicate ntoken) notations]))))

(define xml-element-cons : (-> XML-Element-Content XML-Type-Elements XML-Type-Elements)
  (lambda [e elements]
    (let* ([etoken (xml-element-content-name e)]
           [name (xml:name-datum etoken)])
      (cond [(not (hash-has-key? elements name)) (hash-set elements name e)]
            [else (make+exn:xml:duplicate etoken) elements]))))

(define xml-attributes-cons : (-> XML-Attribute-List XML-Type-Attributes XML-Type-Attributes)
  (lambda [as attributes]
    (let* ([etoken (xml-attribute-list-element as)]
           [ename (xml:name-datum etoken)])
      (hash-set attributes ename
                (for/fold ([apool : (Immutable-HashTable Symbol XML-Attribute) (hash-ref attributes ename (λ [] empty-attributes))])
                          ([a (in-list (xml-attribute-list-body as))])
                  (let* ([atoken (xml-attribute-name a)]
                         [aname (xml:name-datum atoken)])
                    (cond [(not (hash-has-key? apool aname)) (hash-set apool aname a)]
                          [else (make+exn:xml:duplicate atoken etoken) apool])))))))

(define xml-cdata-cons : (-> (Listof XML-CDATA-Token) (Option XML-CDATA-Token) (Option XML-CDATA-Token) (Listof (U XML-Subdatum* XML-Element*))
                             (Listof (U XML-Subdatum* XML-Element*)))
  (lambda [cdatas start-token end-token children]
    (cond [(null? cdatas) children]
          [(null? (cdr cdatas)) (cons (car cdatas) children)]
          [(not (and start-token end-token)) #| <==> (null? cdatas) |# children]
          [else (let ([cdata (apply string-append (filter-map xml-cdata-token->datum cdatas))])
                  (cons (if (eq? (w3s-token-source start-token) (w3s-token-source end-token))
                            (w3s-remake-token [start-token end-token] xml:string cdata)
                            ; TODO: resolve the right position
                            (w3s-remake-token [start-token end-token] xml:string cdata))
                        children))])))

(define xml-char-unreference : (->* (XML-Token String) ((Option XML-Token)) (Option Char))
  (lambda [etoken estr [context #false]]
    (define ?codepoint : (Option Complex)
      (if (regexp-match? #rx"^#x" estr)
          (string->number (substring estr 2) 16)
          (string->number (substring estr 1) 10)))

    (cond [(index? ?codepoint) (integer->char ?codepoint)]
          [else (make+exn:xml:char etoken context) #false])))

(define xml-entity-value-token-ref : (->* (XML-Token (U Symbol Keyword) XML-Type-Entities) ((Option XML-Token)) (Option XML:String))
  (lambda [ntoken name entities [context #false]]
    (let ([e (hash-ref entities name (λ [] #false))])
      (cond [(not e) (make+exn:xml:undeclared ntoken context) #false]
            [else (let ([?vtoken (xml-entity-value e)])
                    (cond [(xml:string? ?vtoken) ?vtoken]
                          [(not (xml-entity-ndata e)) (make+exn:xml:external ntoken context) #false]
                          [else (make+exn:xml:foreign ntoken context) #false]))]))))

(define xml-entity-value-tokens-ref : (->* (XML-Token (U Symbol Keyword) XML-Type-Entities (-> XML:String (Listof XML-Token)))
                                           ((Option XML-Token)) (Option (Listof XML-Token)))
  (lambda [ntoken name entities value->tokens [context #false]]
    (let ([e (hash-ref entities name (λ [] #false))])
      (cond [(not e) (make+exn:xml:undeclared ntoken context) #false]
            [else (let ([?vtoken (xml-entity-value e)])
                    (cond [(xml:string? ?vtoken)
                           (let ([?tokens (unbox (xml-entity-pvalues e))])
                             (cond [(list? ?tokens) ?tokens]
                                   [else (let ([pvalues (value->tokens ?vtoken)])
                                           (set-box! (xml-entity-pvalues e) pvalues)
                                           pvalues)]))]
                          [(not (xml-entity-ndata e)) (make+exn:xml:external ntoken context) #false]
                          [else (make+exn:xml:foreign ntoken context) #false]))]))))

(define xml-prentity-value-ref : (-> Symbol (Option String))
  ;;; https://www.w3.org/TR/xml11/#sec-predefined-ent
  (lambda [name]
    (cond [(eq? name &lt) "\x3c"]
          [(eq? name &gt) "\x3e"]
          [(eq? name &amp) "\x26"]
          [(eq? name &apos) "\x27"]
          [(eq? name &quot) "\x22"]
          [else #false])))

(define xml-entity-value-ref : (->* (XML-Token (U Symbol Keyword) XML-Type-Entities) ((Option XML-Token)) (Values (Option String) Boolean))
  (lambda [ntoken name entities [context #false]]
    (let ([?xstr (xml-entity-value-token-ref ntoken name entities context)])
      (cond [(not ?xstr) (values #false #false)]
            [(xml:&string? ?xstr) (values (xml:string-datum ?xstr) #true)]
            [else (values (xml:string-datum ?xstr) #false)]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml:lang-ref : (case-> [String -> (Option String)]
                               [XML-Token (Pairof XML:Name XML:String) -> (Option String)])
  (case-lambda
    [(lang) (and (> (string-length lang) 0) lang)]
    [(tagname attr) (xml:lang-ref (xml:string-datum (cdr attr)))]))

(define xml:space-ref : (-> XML-Token (Pairof XML:Name XML:String) Symbol Symbol)
  (lambda [tagname attr inherited-space]
    (define this:space : String (xml:string-datum (cdr attr)))

    (cond [(string=? this:space "preserve") 'preserve]
          [(string=? this:space "default") (default-xml:space)]
          [else (make+exn:xml:enum (list (car attr) (cdr attr)) tagname) inherited-space])))

(define xml:space=preserve : (-> Symbol XML:WhiteSpace (Option XML:Space-Filter) (Option String) XML:WhiteSpace)
  (lambda [tag ws xml:filter xml:lang]
    (define has-newline? : Boolean (xml:newline? ws))

    (cond [(not (or xml:filter has-newline?)) ws]
          [else (let* ([space-filter (or xml:filter xml:space-values)]
                       [spaces : String (xml:whitespace-datum ws)]
                       [size : Index (string-length spaces)])
                  (let preserve-filter ([idx : Nonnegative-Fixnum 0]
                                        [xD? : Boolean #false]
                                        [secaps : (Option (Listof Char)) #false])
                    (if (>= idx size)
                        (let ([sp-chars (if (not xD?) secaps (assert (xml:space-cons spaces idx #\newline #\return space-filter 0 secaps tag xml:lang)))])
                          ; NOTE: XML:NewLine is designed to indicate if newlines have been normalized
                          (if (not sp-chars) ws (w3s-remake-token ws xml:whitespace (list->string (reverse sp-chars)))))
                        (let ([ch (unsafe-string-ref spaces idx)]
                              [idx+1 (+ idx 1)])
                          (cond [(not xD?)
                                 (cond [(eq? ch #\return) (preserve-filter idx+1 #true secaps)]
                                       #;[(eq? ch #\u2028) (whitespace-filter idx+1 #false (xml-whitespace-cons spaces idx #\newline ch space-filter 0 secaps))]
                                       #;[(eq? ch #\u0085) (whitespace-filter idx+1 #false (xml-whitespace-cons spaces idx #\newline ch space-filter 0 secaps))]
                                       [else (preserve-filter idx+1 #false (xml:space-cons spaces idx ch ch space-filter 0 secaps tag xml:lang))])]
                                [(or (eq? ch #\newline) #;(eq? ch #\u0085))
                                 (preserve-filter idx+1 #false (xml:space-cons spaces idx #\newline #\return space-filter 1 secaps tag xml:lang))]
                                [else (preserve-filter idx #false (xml:space-cons spaces (- idx 1) #\newline #\return space-filter 0 secaps tag xml:lang))])))))])))

(define xml:space-cons : (-> String Fixnum Char Char (-> Symbol (Option String) Char (Option Char)) (U Zero One) (Option (Listof Char))
                             Symbol (Option String) (Option (Listof Char)))
  (lambda [spaces idx s os filter nl-span secaps0 tagname xml:lang]
    (define ns : (Option Char) (filter tagname xml:lang s))
    (cond [(eq? ns os) (if (list? secaps0) (cons s secaps0) secaps0)]
          [else (let ([secaps (if (list? secaps0) secaps0 (reverse (string->list (substring spaces 0 (- idx nl-span)))))])
                  (if (not ns) secaps (cons ns secaps)))])))

(define xml-child-cons : (->* ((Listof XML:WhiteSpace) (Listof (U XML-Subdatum* XML-Element*)) (Option XML:Space-Filter) Symbol (Option String))
                                 (Boolean) (Listof (U XML-Subdatum* XML-Element*)))
  (lambda [secaps nerdlihc xml:filter tag xml:lang [tail? #false]]
    (cond [(null? secaps) nerdlihc]
          [(not xml:filter) (if (or (null? nerdlihc) tail?) nerdlihc (cons (xml-whitespaces-consolidate secaps) nerdlihc))]
          [else (let* ([raw-spaces (xml-whitespaces-fold secaps)]
                       [well-formed-raw-spaces (xml:space=preserve tag raw-spaces #false xml:lang)]
                       [has-newline? (xml:newline? raw-spaces)]
                       [raw (xml:whitespace-datum well-formed-raw-spaces)]
                       [head? (null? nerdlihc)])
                  (define default-space : (Option String)
                    (cond [(and tail? head?) (xml:filter tag xml:lang raw #false 'span has-newline?)]
                          [(and head?) (xml:filter tag xml:lang raw #false 'head has-newline?)]
                          [(and tail?) (xml:filter tag xml:lang raw #false 'tail has-newline?)]
                          [else (xml:filter tag xml:lang raw " " 'body has-newline?)]))
                  (cond [(not default-space) nerdlihc]
                        [(or (string=? default-space raw)) (cons well-formed-raw-spaces nerdlihc)]
                        [else (cons (w3s-remake-token well-formed-raw-spaces xml:whitespace default-space) nerdlihc)]))])))

(define xml-air-elements-trim : (-> (Listof (U XML-Subdatum* XML-Element*))
                                    (Values (Option XML:WhiteSpace) (Listof (U XML-Subdatum* XML-Element*)) (Option XML:WhiteSpace)))
  (lambda [air]
    (cond [(null? air) (values #false air #false)]
          [else (let*-values ([(?lspace rest0) (values (car air) (cdr air))]
                              [(head rest) (if (xml:whitespace? ?lspace) (values ?lspace rest0) (values #false air))])
                  (cond [(null? rest) (values head null #false)]
                        [else (let*-values ([(body0 ?tspace) (split-at rest (sub1 (length rest)))]
                                            [(body tail) (if (xml:whitespace? ?tspace) (values body0 ?tspace) (values rest #false))])
                                (values head body tail))]))])))

(define xml-whitespaces-consolidate : (-> (Pairof XML:WhiteSpace (Listof XML:WhiteSpace)) XML:WhiteSpace)
  (lambda [secaps]
    (define-values (head body) (values (car secaps) (cdr secaps)))
    
    (if (pair? body)
        (let ws-consolidate ([rest : (Listof XML:WhiteSpace) body]
                             [end-token : XML:WhiteSpace head]
                             [start-token : XML:WhiteSpace head])
          (cond [(null? rest) (w3s-remake-token [start-token end-token] xml:whitespace " ")]
                [else (let ([self (car rest)])
                        (ws-consolidate (cdr rest)
                                        (if (eq? (w3s-token-source end-token) (w3s-token-source self)) end-token self)
                                        self))]))
        (let ([raw (xml:whitespace-datum head)])
          (cond [(string=? raw " ") head]
                [else (w3s-remake-token head xml:whitespace " ")])))))

(define xml-whitespaces-fold : (-> (Pairof XML:WhiteSpace (Listof XML:WhiteSpace)) XML:WhiteSpace)
  (lambda [secaps]
    (define-values (head body) (values (car secaps) (cdr secaps)))
    
    (cond [(null? body) head]
          [else (let ws-fold ([rest : (Listof XML:WhiteSpace) body]
                              [spaces : (Listof String) null]
                              [end-token : XML:WhiteSpace head]
                              [start-token : XML:WhiteSpace head]
                              [has-newline? : Boolean (xml:newline? head)])
                  (if (null? rest)
                      (w3s-remake-token [start-token end-token]
                                        (if has-newline? xml:newline xml:whitespace)
                                        (apply string-append spaces))
                      (let ([self (car rest)])
                        (ws-fold (cdr rest)
                                 (cons (xml:whitespace-datum self) spaces)
                                 (if (eq? (w3s-token-source end-token) (w3s-token-source self)) end-token self)
                                 self
                                 (or has-newline? (xml:newline? self))))))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-xml:space : (Parameterof Symbol) (make-parameter 'default))
(define empty-attributes : (Immutable-HashTable Symbol XML-Attribute) (make-immutable-hasheq))

(define xml:space-values : (-> Symbol (Option String) Char (Option Char))
  (lambda [tag xml:lang char]
    char))

(define make-entity-value->tokens : (All (T) (-> T (-> Input-Port (U String Symbol) T (Listof XML-Token)) (-> XML:String (Listof XML-Token))))
  (lambda [scope read-tokens]
    (λ [[tv : XML:String]]
      (define source : String (w3s-token-location-string tv))
      (define /dev/subin : Input-Port (dtd-open-input-port (xml:string-datum tv) #true source))
      
      (read-tokens /dev/subin source scope))))
