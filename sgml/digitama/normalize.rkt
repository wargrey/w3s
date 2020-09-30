#lang typed/racket/base

(provide (all-defined-out))

(require racket/path)
(require racket/port)
(require racket/string)

(require typed/racket/unsafe)
(require racket/unsafe/ops)

(require "grammar.rkt")
(require "dtd.rkt")

(require "digicore.rkt")
(require "tokenizer.rkt")
(require "prentity.rkt")
(require "stdin.rkt")

(require "tokenizer/port.rkt")
(require "tokenizer/characters.rkt")

(unsafe-require/typed
 racket/unsafe/ops ; only works for Latin-1 Strings
 [unsafe-string-ref (-> String Index Char)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type XML-Space-Position (U 'head 'body 'tail 'span))

(define-type XML:Space-Filter
  (case-> [Symbol (Option String) Char -> (Option Char)]
          [Symbol (Option String) String (Option String) XML-Space-Position Boolean -> (Option String)]))

(define-type Open-Input-XML-XXE
  (-> Path-String (Option String) (Option String) (Option Index) (Boxof (U False String Symbol))
      (U False Input-Port (Pairof Input-Port Boolean))))

(define-type XML-XXE-Reader (All (E) (->* (Input-Port) ((U False Symbol String)) E)))

(define default-xml-ipe-topsize : (Parameterof (Option Index)) (make-parameter #x2000))
(define default-xml-xxe-topsize : (Parameterof (Option Index)) (make-parameter #x200000))
(define default-xml-xxe-timeout : (Parameterof (Option Real)) (make-parameter 4.0))

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
(define xml-normalize : (-> XML-DTD (Option XML-DTD) (Listof XML-Content*) String Symbol (Option XML:Space-Filter)
                            (Option Index) (Option Open-Input-XML-XXE) (Option Index) (Option Real)
                            (Values XML-Type (Listof XML-Content*)))
  (lambda [int-dtd ?ext-dtd content xml:lang xml:space xml:space-filter ipe-topsize open-port xxe-topsize timeout]
    (define xxec : XML-XXE-Config (and open-port (vector open-port xxe-topsize timeout)))
    (define dtype : XML-Type
      (xml-dtd-expand #:open-xxe-input-port open-port
                      #:ipe-topsize ipe-topsize #:xxe-topsize xxe-topsize #:xxe-timeout timeout
                      int-dtd ?ext-dtd))
    
    (parameterize ([default-xml:space xml:space])
      (let xml-content-normalize ([rest : (Listof XML-Content*) content]
                                  [clear-content : (Listof XML-Content*) null])
        (cond [(null? rest) (values dtype (reverse clear-content))]
              [else (let-values ([(self rest++) (values (car rest) (cdr rest))])
                      (cond [(list? self)
                             (let ([elem (xml-element-normalize self dtype (xml:lang-ref xml:lang) xml:space xml:space-filter 0 ipe-topsize xxec)])
                               (xml-content-normalize rest++ (cons elem clear-content)))]
                            [else (xml-content-normalize rest++ (cons self clear-content))]))])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-dtd-expand : (->* (XML-DTD)
                              (#:open-xxe-input-port (Option Open-Input-XML-XXE)
                               #:ipe-topsize (Option Index) #:xxe-topsize (Option Index) #:xxe-timeout (Option Real)
                               (Option XML-DTD))
                              XML-Type)
  (lambda [#:open-xxe-input-port [open-port #false] #:ipe-topsize [ipe-topsize (default-xml-ipe-topsize)]
           #:xxe-topsize [xxe-topsize (default-xml-xxe-topsize)] #:xxe-timeout [timeout (default-xml-xxe-timeout)]
           dtd [?ext-dtd #false]]
    ;;; NOTE
    ;  Declarations in the intsubset may refer to external (parameter) entities
    ;  whose values could depend on previously defined entities within the intsubset
    ;
    ;  Although parameter entities references are not allowed inside the declarations
    ;  in the internal DTD, SVG's extensibility depends on this feature.

    (define xxec : XML-XXE-Config (and open-port (vector open-port xxe-topsize timeout)))
    (define-values (int-entities iothers)
      (xml-dtd-entity-expand/partition dtd #false #true ipe-topsize xxec))
    
    (define-values (entities eothers)
      (cond [(not ?ext-dtd) (values int-entities null)]
            [else (xml-dtd-entity-expand/partition ?ext-dtd int-entities #true #false xxec)]))
    
    (xml-dtd-type-declaration-expand entities (append iothers eothers) ipe-topsize xxec)))

(define xml-dtd-entity-expand/partition : (-> XML-DTD (Option DTD-Entities) Boolean (Option Index) XML-XXE-Config (Values DTD-Entities (Listof DTD-Declaration*)))
  (lambda [dtd internal-entities merge? topsize xxec]
    (define-values (initial-entities int-entities)
      (cond [(not merge?) (values empty-entities internal-entities)]
            [else (values (or internal-entities empty-entities) #false)]))

    (let partition-dtd ([rest : (Listof DTD-Declaration*) (xml-dtd-declarations dtd)]
                        [snoitaralced : (Listof DTD-Declaration*) null]
                        [entities : DTD-Entities initial-entities]
                        [PErefers : (Listof Keyword) null])
      (cond [(null? rest) (values entities (reverse snoitaralced))]
            [else (let-values ([(self rest++) (values (car rest) (cdr rest))])
                    (cond [(dtd-entity? self) ; NOTE: NDATA only concerns validity constraint, and therefore is not handled here.
                           (let* ([ntoken (dtd-entity-name self)]
                                  [name (if (xml:reference? ntoken) (xml:reference-datum ntoken) (xml:pereference-datum ntoken))])
                             (cond [(hash-has-key? entities name) (make+exn:xml:multiple ntoken) (partition-dtd rest++ snoitaralced entities PErefers)]
                                   [(dtd-internal-entity? self)
                                    (let ([?expanded (xml-dtd-included-in-literal self entities int-entities topsize xxec)])
                                      (partition-dtd rest++ snoitaralced (if (not ?expanded) entities (hash-set entities name ?expanded)) PErefers))]
                                   [else (partition-dtd rest++ snoitaralced (hash-set entities name self) PErefers)]))]
                          [(xml:pereference? self)
                           (let ([name (xml:pereference-datum self)])
                             (cond [(memq name PErefers) (make+exn:xml:defense self) (partition-dtd rest++ snoitaralced entities PErefers)]
                                   [else (let ([PErefers++ (cons name PErefers)]
                                               [subset (xml-dtd-included-as-PE self entities int-entities topsize xxec)])
                                           (cond [(null? subset) (partition-dtd rest++ snoitaralced entities PErefers++)]
                                                 [else (let-values ([(entities++ snoitaralced++) (partition-dtd subset null entities PErefers++)])
                                                         (partition-dtd rest++ (append (reverse snoitaralced++) snoitaralced) entities++ PErefers++))]))]))]
                          [(pair? self)
                           (partition-dtd (append (xml-dtd-expand-section (car self) (cdr self) entities int-entities topsize) rest++)
                                          snoitaralced entities PErefers)]
                          [else (partition-dtd rest++ (cons self snoitaralced) entities PErefers)]))]))))

(define xml-dtd-type-declaration-expand : (-> DTD-Entities (Listof DTD-Declaration*) (Option Index) XML-XXE-Config XML-Type)
  (lambda [entities declarations topsize xxec]
    (let expand-dtd ([rest : (Listof DTD-Declaration*) declarations]
                     [notations : DTD-Notations empty-notations]
                     [elements : DTD-Elements empty-elements]
                     [attributes : DTD-Attributes empty-element-attributes])
      (cond [(null? rest) (xml-type entities notations elements attributes)]
            [else (let-values ([(self rest++) (values (car rest) (cdr rest))])
                    (cond [(dtd-element? self) (expand-dtd rest++ notations (dtd-element-cons self elements) attributes)]
                          [(dtd-attlist? self) (expand-dtd rest++ notations elements (dtd-attributes-cons self attributes))]
                          [(dtd-notation? self) (expand-dtd rest++ (dtd-notation-cons self notations) elements attributes)]
                          [else (or (and (vector? self)
                                         (let ([DECL (vector-ref self 0)]
                                               [body (vector-ref self 1)])
                                           (case (xml:name-datum DECL)
                                             [(ATTLIST)
                                              (let ([?attr (xml-dtd-expand-raw-declaration xml-dtd-extract-attributes* DECL body entities topsize xxec)])
                                                (and (dtd-attlist? ?attr)
                                                     (expand-dtd rest++ notations elements (dtd-attributes-cons ?attr attributes))))]
                                             [(ELEMENT)
                                              (let ([?elem (xml-dtd-expand-raw-declaration xml-dtd-extract-element* DECL body entities topsize xxec)])
                                                (and (dtd-element? ?elem)
                                                     (expand-dtd rest++ notations (dtd-element-cons ?elem elements) attributes)))]
                                             [else #false])))
                                    (expand-dtd rest++ notations elements attributes))]))]))))

(define xml-dtd-expand-section : (-> (U XML:Name XML:PEReference) (Listof DTD-Declaration*) DTD-Entities (Option DTD-Entities) (Option Index) (Listof DTD-Declaration*))
  (lambda [condition body entities int-entities topsize]
    (define sec-name : (U False Symbol String)
      (cond [(xml:name? condition) (xml:name-datum condition)]
            [else (let-values ([(?value ge?) (xml-internal-entity-value-ref condition (xml:pereference-datum condition) entities int-entities)])
                    (cond [(not (xml-entity-value-normalize? ?value topsize)) ?value]
                          [else (xml-dtd-entity-replace condition ?value entities int-entities topsize)]))]))

    (cond [(or (eq? sec-name 'INCLUDE) (equal? sec-name "INCLUDE")) body]
          [(or (eq? sec-name 'IGNORE) (equal? sec-name "IGNORE")) null]
          [else null])))

(define xml-dtd-expand-raw-declaration : (All (T) (-> (-> XML:Name (Listof XML-Doctype-Body*) T) XML:Name (Listof XML-Doctype-Body*)
                                                      DTD-Entities (Option Index) XML-XXE-Config
                                                      T))
  (lambda [extract DECL body entities topsize xxec]
    (extract DECL
             (let expand-body ([rest : (Listof XML-Doctype-Body*) body]
                               [ydob : (Listof XML-Doctype-Body*) null])
               (cond [(null? rest) (reverse ydob)]
                     [else (let-values ([(self rest++) (values (car rest) (cdr rest))])
                             (if (xml:pereference? self)
                                  ; TODO: if might DECL affect the parser in some rare cases
                                 (let ([?tokens (xml-entity-value-tokens-ref self (xml:pereference-datum self) entities #false xml-dtd-entity-replace topsize
                                                                             (xml-make-entity->tokens (xml:name-datum DECL) read-dtd-declaration-tokens*)
                                                                             xxec xml-dtd-reader)])
                                   (expand-body (if (pair? ?tokens) (append ?tokens rest++) rest++) ydob))
                                 (expand-body rest++ (cons self ydob))))])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-dtd-included-in-literal : (-> DTD-Internal-Entity DTD-Entities (Option DTD-Entities) (Option Index) XML-XXE-Config (Option DTD-Entity))
  ;;; https://www.w3.org/TR/xml/#inliteral
  ;;; https://www.w3.org/TR/xml/#bypass
  (lambda [e entities ?int-entities topsize xxec]
    (define ?value (dtd-internal-entity-value e))
    
    (cond [(not (xml-entity-value-normalize? ?value topsize)) e]
          [else (let ([?new-value (xml-dtd-entity-replace (dtd-entity-name e) ?value entities ?int-entities topsize xxec)])
                  (and ?new-value
                       (dtd-token-entity (dtd-entity-name e)
                                         ?new-value #false)))])))

(define xml-dtd-included-as-PE : (-> XML:PEReference DTD-Entities (Option DTD-Entities) (Option Index) XML-XXE-Config (Listof DTD-Declaration*))
  ;;; https://www.w3.org/TR/xml/#as-PE
  (lambda [pe entities ?int-entities topsize xxec]
    (define ?tokens : (U (Listof XML-Token) XML-Syntax-Error)
      (xml-entity-value-tokens-ref pe (xml:pereference-datum pe)
                                   entities ?int-entities xml-dtd-entity-replace topsize
                                   (xml-make-entity->tokens #false read-xml-tokens*)
                                   xxec xml-dtd-reader))

    (if (pair? ?tokens)
        (xml-dtd-definitions->declarations (xml-syntax->definition* ?tokens))
        null)))

(define xml-dtd-included : (-> XML:Name XML:Reference DTD-Entities (Option Index) XML-XXE-Config Index (Listof (U XML-Subdatum* XML-Element*)))
  ;;; https://www.w3.org/TR/xml/#included
  (lambda [tagname e entities topsize xxec depth]
    (define ?tokens : (U (Listof XML-Token) XML-Syntax-Error)
      (xml-entity-value-tokens-ref e (xml:reference-datum e)
                                   entities #false xml-cdata-entity-replace topsize
                                   (xml-make-entity->tokens depth read-xml-content-tokens*)
                                   xxec (xml-make-cdata-reader e) tagname))

    (if (pair? ?tokens)
        (let-values ([(children rest) (xml-syntax-extract-subelement* tagname ?tokens #true)])
          (cond [(and children (null? rest)) children]
                [else (make+exn:xml:malformed e tagname) null]))
        null)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-element-normalize : (-> XML-Element* XML-Type (Option String) Symbol (Option XML:Space-Filter) Index (Option Index) XML-XXE-Config XML-Element*)
  (lambda [e dtype xml:lang xml:space xml:filter depth topsize xxec]
    (define entities : DTD-Entities (xml-type-entities dtype))
    (define tagname : XML:Name (car e))
    (define attlist : (Immutable-HashTable Symbol DTD-Attribute)
      (hash-ref (xml-type-attributes dtype) (xml:name-datum tagname)
                (λ [] empty-attributes)))
    
    (define-values (setubirtta xml:this:lang xml:this:space)
      (let attribute-filter-map : (Values (Listof XML-Element-Attribute*) (Option String) Symbol) ([rest : (Listof XML-Element-Attribute*) (cadr e)]
                                                                                                   [setubirtta : (Listof XML-Element-Attribute*) null]
                                                                                                   [names : (Listof Symbol) null]
                                                                                                   [lang : (Option String) xml:lang]
                                                                                                   [space : Symbol xml:space])
        (if (pair? rest)
            (let*-values ([(self rest++) (values (car rest) (cdr rest))]
                          [(decl) (hash-ref attlist (xml:name-datum (car self)) (λ [] undeclared-attribute))]
                          [(?attr) (xml-element-attribute-normalize decl self entities names tagname topsize xxec)])
              (cond [(not ?attr) (attribute-filter-map rest++ setubirtta names lang space)]
                    [else (let ([attr-name (xml:name-datum (car ?attr))])
                            (attribute-filter-map rest++ (cons ?attr setubirtta) (cons attr-name names)
                                                  (if (eq? attr-name 'xml:lang) (xml:lang-ref tagname ?attr) lang)
                                                  (if (eq? attr-name 'xml:space) (xml:space-ref tagname ?attr space) space)))]))
            
            (for/fold ([setubirtta : (Listof XML-Element-Attribute*) setubirtta]
                       [this:lang : (Option String) lang]
                       [this:space : Symbol space])
                      ([(key attr) (in-hash attlist)]
                       #:unless (memq key names))
              (let ([?attr (xml-element-attribute-normalize attr entities names topsize xxec)])
                (cond [(not ?attr) (values setubirtta this:lang this:space)]
                      [else (let ([attname (dtd-attribute-element attr)])
                              (values (cons ?attr setubirtta)
                                      (if (eq? key 'xml:lang) (xml:lang-ref attname ?attr) this:lang)
                                      (if (eq? key 'xml:space) (xml:space-ref attname ?attr space) this:space)))]))))))

    (define ?children : (Listof (U XML-Subdatum* XML-Element*))
      (xml-subelement-normalize tagname (caddr e) dtype
                                xml:this:lang xml:this:space xml:filter
                                (assert (+ depth 1) index?) topsize xxec))
    
    (cond [(list? ?children) (list tagname (reverse setubirtta) ?children)]
          [else ?children])))

(define xml-element-attribute-normalize
  : (case-> [DTD-Attribute XML-Element-Attribute* DTD-Entities (Listof Symbol) XML-Token (Option Index) XML-XXE-Config -> (Option XML-Element-Attribute*)]
            [DTD-Attribute DTD-Entities (Listof Symbol) (Option Index) XML-XXE-Config -> (Option XML-Element-Attribute*)])
  (case-lambda
    [(?attr name=value entities attrnames tagname topsize xxec)
     (let ([name (car name=value)])
       (cond [(memq (xml:name-datum name) attrnames) (make+exn:xml:unique name tagname) #false]
             [else (let* ([value (cdr name=value)]
                          [?value (if (xml-entity-value-normalize? value topsize) (xml-attr-entity-replace name value entities #false topsize xxec) value)]
                          [?value (if (xml:string? ?value) (xml-element-attribute-normalize/further name (dtd-attribute-type ?attr) ?value) ?value)])
                     (and ?value (not (integer? ?value))
                          (cond [(eq? ?value value) name=value]
                                [else (cons name ?value)])))]))]
    [(attr entities attrnames topsize xxec)
     (let ([name (dtd-attribute-name attr)])
       (and (not (memq (xml:name-datum name) attrnames))
            (dtd-attribute+default? attr)
            (let* ([value (dtd-attribute+default-value attr)]
                   [?value (if (xml-entity-value-normalize? value topsize) (xml-attr-entity-replace name value entities #false topsize xxec) value)]
                   [?value (and (xml:string? ?value) (xml-element-attribute-normalize/further name (dtd-attribute-type attr) ?value))])
              (and ?value (cons name ?value)))))]))

(define xml-element-attribute-normalize/further : (-> XML:Name DTD-Attribute-Type XML:String (Option XML-Element-Attribute-Value*))
  (lambda [attname atype value]
    (cond [(dtd-attribute-string-type? atype) value]
          [(dtd-attribute-name-type? atype)
           (w3s-remake-token value xml:name
                             (let ([clean-value (xml-attribute-token-value-consolidate value)])
                               (case (xml:name-datum (dtd-attribute-token-type-name atype))
                                 [(ENTITY) (string->unreadable-symbol clean-value)]
                                 [else (string->symbol clean-value)])))]
          [(dtd-attribute-names-type? atype)
           (let ([names (string-split (xml:string-datum value))])
             (map (λ [[name : Symbol]] (w3s-remake-token value xml:name name))
                  (case (xml:name-datum (dtd-attribute-token-type-name atype))
                    [(ENTITY) (map string->unreadable-symbol names)]
                    [else (map string->symbol names)])))]
          [(dtd-attribute-enum-type? atype)
           (let ([option (string->symbol (xml-attribute-token-value-consolidate value))])
             (cond [(memq option (map xml:name-datum (dtd-attribute-enum-type-options atype))) (w3s-remake-token value xml:name option)]
                   [(and (eq? (xml:name-datum attname) 'xml:space) (memq option '(default preserve))) (w3s-remake-token value xml:name option)]
                   [else (make+exn:xml:enum value attname) #false]))]
          [else #| deadcode |# value])))

(define xml-subelement-normalize : (-> XML:Name (Listof (U XML-Subdatum* XML-Element*)) XML-Type (Option String) Symbol (Option XML:Space-Filter)
                                       Index (Option Index) XML-XXE-Config (Listof (U XML-Subdatum* XML-Element*)))
  (lambda [tagname children dtype xml:lang xml:space xml:filter depth topsize xxec]
    (define tag : Symbol (xml:name-datum tagname))
    
    (let normalize-subelement ([rest : (Listof (U XML-Subdatum* XML-Element*)) children]
                               [nerdlihc : (Listof (U XML-Subdatum* XML-Element*)) null]
                               [secaps : (Listof XML:WhiteSpace) null])
      (if (pair? rest)
          (let-values ([(self rest++) (values (car rest) (cdr rest))])
            (cond [(list? self)
                   (let ([?elem (xml-element-normalize self dtype xml:lang xml:space xml:filter depth topsize xxec)])
                     (cond [(list? ?elem) (normalize-subelement rest++ (cons ?elem (xml-child-cons secaps nerdlihc xml:filter tag xml:lang)) null)]
                           [else ?elem]))]

                  [(xml:reference? self)
                   (let*-values ([(air0) (xml-dtd-included tagname self (xml-type-entities dtype) topsize xxec depth)]
                                 [(?lspace air-elements ?tspace) (xml-air-elements-trim air0)]
                                 [(nerdlihc++) (xml-child-cons (if (not ?lspace) secaps (cons ?lspace secaps)) nerdlihc xml:filter tag xml:lang)]
                                 [(sp.rest++) (if (not ?tspace) rest++ (cons ?tspace rest++))])
                     (normalize-subelement sp.rest++ (append (reverse (normalize-subelement air-elements null null)) nerdlihc++) null))]

                  [(xml:char? self)
                   (let ([content (w3s-remake-token self xml:string (string (integer->char (xml:char-datum self))))])
                     (normalize-subelement rest++ (cons content (xml-child-cons secaps nerdlihc xml:filter tag xml:lang)) null))]

                  [(not (xml:whitespace? self)) (normalize-subelement rest++ (cons self (xml-child-cons secaps nerdlihc xml:filter tag xml:lang)) null)]
                  [(xml:comment? self) (normalize-subelement rest++ nerdlihc secaps)]
                  [(eq? xml:space 'preserve) (normalize-subelement rest++ (cons (xml:space=preserve tag self xml:filter xml:lang) nerdlihc) secaps)]
                  [else (normalize-subelement rest++ nerdlihc (cons self secaps))]))

          (let cdata-reverse ([rest : (Listof (U XML-Subdatum* XML-Element*)) (xml-child-cons secaps nerdlihc xml:filter tag xml:lang #true)]
                              [children : (Listof (U XML-Subdatum* XML-Element*)) null]
                              [cdatas : (Listof XML-CDATA-Token) null]
                              [start-token : (Option XML-CDATA-Token) #false]
                              [end-token : (Option XML-CDATA-Token) #false])
            (cond [(null? rest) (dtd-cdata-cons cdatas start-token end-token children)]
                  [else (let-values ([(self rest++) (values (car rest) (cdr rest))])
                          (cond [(xml-cdata-token? self) (cdata-reverse rest++ children (cons self cdatas) self (or end-token self))]
                                [else (cdata-reverse rest++ (cons self (dtd-cdata-cons cdatas start-token end-token children)) null #false #false)]))]))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NOTE
;; The use contexts of Parameter Entity and General Entity have no overlaps.
;; PEs are used in DTD where GEs should be bypassed, and are expanded immediately, and therefore no recursions;
;; GEs are used in element attributes or content where PE should not be recognized;
;;
;; Besides, Char references are expanded immediately in all contexts.

(define xml-dtd-entity-replace : (-> XML-Token XML:String DTD-Entities (Option DTD-Entities) (Option Index) XML-XXE-Config (Option XML:String))
  (lambda [ename etoken entities int-entities topsize xxec]
    (define /dev/evout : Output-Port (open-output-string '/dev/evout))
    (define src : String (xml:string-datum etoken))
    (define size : Index (string-length src))
    (define false-idx : Nonnegative-Fixnum (+ size 1))
    (define safe? : Boolean (eq? (string-utf-8-length src) size))
    
    (let dtd-normalize ([idx : Nonnegative-Fixnum 0]
                        [leader : (Option Char) #false]
                        [srahc : (Listof Char) null]
                        [gexists? : Boolean #false]
                        [expanded : Nonnegative-Fixnum 0])
      (cond [(< idx size)
             (let ([ch (unsafe-string-ref src idx)])
               (define idx++ : Nonnegative-Fixnum (+ idx 1))
               
               (when (eq? ch #\<)
                 (make+exn:xml:char etoken ename 'debug))
               
               (if (not leader)
                   (cond [(or (eq? ch #\%) (eq? ch #\&)) (dtd-normalize idx++ ch srahc gexists? expanded)]
                         [(eq? expanded topsize) (make+exn:xml:bomb etoken ename) (dtd-normalize false-idx #false null gexists? expanded)]
                         [else (write-char ch /dev/evout) (dtd-normalize idx++ leader srahc gexists? (unsafe-fx+ expanded 1))])
                   (cond [(not (eq? ch #\;)) (dtd-normalize idx++ leader (cons ch srahc) gexists? expanded)]
                         [(null? srahc) (make+exn:xml:malformed etoken ename) (dtd-normalize false-idx leader srahc gexists? expanded)]
                         [else (let ([estr (list->string (reverse srahc))])
                                 (cond [(eq? leader #\%)
                                        (let-values ([(replacement ge?) (xml-internal-entity-value-ref etoken (string->keyword estr) entities int-entities ename)])
                                          (if (not replacement)
                                              (dtd-normalize false-idx leader srahc gexists? expanded)
                                              (let* ([self-size (string-length replacement)]
                                                     [expanded++ (unsafe-fx+ self-size expanded)])
                                                (if (or (not topsize) (<= expanded++ topsize))
                                                    (and (write-string replacement /dev/evout 0 self-size)
                                                         (dtd-normalize idx++ #false null (or ge? gexists?) expanded++))
                                                    (and (make+exn:xml:bomb etoken ename)
                                                         (dtd-normalize false-idx leader srahc gexists? expanded))))))]
                                       [(eq? (unsafe-string-ref estr 0) #\#)
                                        (let ([replacement (xml-char-unreference etoken estr ename)])
                                          (if (not replacement)
                                              (dtd-normalize false-idx #false null gexists? expanded)
                                              (if (eq? expanded topsize)
                                                  (and (make+exn:xml:bomb etoken ename)
                                                       (dtd-normalize false-idx #false null gexists? expanded))
                                                  (and (write-char replacement /dev/evout)
                                                       (dtd-normalize idx++ #false null gexists? (unsafe-fx+ expanded 1))))))]
                                       [else ; GEs are allowed to be declared later unless they are referenced in the default values of <!ATTLIST>
                                        (let* ([self-size (string-length estr)]
                                               [expanded++ (unsafe-fx+ (+ self-size 2) expanded)])
                                          (if (or (not topsize) (<= expanded++ topsize))
                                              (and (write-char #\& /dev/evout)
                                                   (write-string estr /dev/evout)
                                                   (write-char #\; /dev/evout)
                                                   (dtd-normalize idx++ #false null #true expanded++))
                                              (and (make+exn:xml:bomb etoken ename)
                                                   (dtd-normalize false-idx leader srahc gexists? expanded))))]))])))]
            [(and (= idx size) (not leader))
             (let ([new-value (get-output-string /dev/evout)])
               (if (not gexists?)
                   (w3s-remake-token etoken xml:string new-value)
                   (w3s-remake-token etoken xml:&string new-value)))]
            
            [else #false]))))

(define xml-attr-entity-replace : (->* (XML-Token XML:String DTD-Entities (Option DTD-Entities) (Option Index) XML-XXE-Config)
                                       (Nonnegative-Fixnum (Listof Symbol) Output-Port)
                                       (U XML:String Nonnegative-Fixnum False))
  ;;; https://www.w3.org/TR/xml/#sec-line-ends
  ;;; https://www.w3.org/TR/xml/#AVNormalize
  ;;; https://www.w3.org/TR/xml/#NoExternalRefs
  (lambda [aname vtoken entities int-entities topsize xxec [expanded 0] [rstack null] [/dev/avout (open-output-string '/dev/avout)]]
    (define src : String (xml:string-datum vtoken))
    (define size : Index (string-length src))
    (define false-idx : Nonnegative-Fixnum (+ size 1))
    
    (let attv-normalize ([idx : Nonnegative-Fixnum 0]
                         [leader : (Option Symbol) #false]
                         [srahc : (Listof Char) null]
                         [expanded : Nonnegative-Fixnum expanded])
      (cond [(< idx size)
             (let ([ch (string-ref src idx)])
               (define idx++ : Nonnegative-Fixnum (+ idx 1))
               
               (cond [(eq? leader '&)
                      (cond [(not (eq? ch #\;)) (attv-normalize idx++ leader (cons ch srahc) expanded)]
                            [(null? srahc) (make+exn:xml:malformed vtoken aname) (attv-normalize false-idx leader srahc expanded)]
                            [else (let* ([estr (list->string (reverse srahc))]
                                         [ename (string->unreadable-symbol estr)]
                                         [?prev (xml-prentity-value-ref ename)])
                                    (cond [(char? ?prev)
                                           (cond [(eq? expanded topsize) (make+exn:xml:bomb vtoken aname) (attv-normalize false-idx #false null expanded)]
                                                 [else (write-char ?prev /dev/avout) (attv-normalize idx++ #false null (unsafe-fx+ expanded 1))])]
                                          [(eq? (string-ref estr 0) #\#)
                                           (let ([replacement (xml-char-unreference vtoken estr aname)])
                                             (cond [(not replacement) (attv-normalize false-idx #false null expanded)]
                                                   [(eq? expanded topsize) (make+exn:xml:bomb vtoken aname) (attv-normalize false-idx #false null expanded)]
                                                   [else (write-char replacement /dev/avout) (attv-normalize idx++ #false null (unsafe-fx+ expanded 1))]))]
                                          [(memv ename rstack) (make+exn:xml:loop vtoken aname) (attv-normalize false-idx #false null expanded)]
                                          [else (let ([?etoken (xml-internal-entity-value-token-ref vtoken ename entities int-entities aname)])
                                                  (cond [(not ?etoken) (attv-normalize false-idx #false null expanded)]
                                                        [else (let ([expanded++ (xml-attr-entity-replace aname ?etoken entities int-entities topsize #false
                                                                                                         expanded (cons ename rstack) /dev/avout)])
                                                                (cond [(index? expanded++) (attv-normalize idx++ #false null expanded++)]
                                                                      [else (attv-normalize false-idx #false null expanded)]))]))]))])]

                     [(eq? expanded topsize) (make+exn:xml:bomb vtoken aname) (attv-normalize false-idx #false null expanded)]

                     ; NOTE: the EOL handling and attribute normalization conincide here.
                     [(eq? leader 'xD) (write-char #\space /dev/avout) (attv-normalize (if (eq? ch #\newline) idx++ idx) #false srahc expanded)]
                     [(eq? ch #\&) (attv-normalize idx++ '& srahc expanded)]
                     [(eq? ch #\return) (attv-normalize idx++ 'xD srahc (unsafe-fx+ expanded 1 #| for `return`s at the end of the string |#))]
                     [(eq? ch #\newline) (write-char #\space /dev/avout) (attv-normalize idx++ leader srahc (unsafe-fx+ expanded 1))]
                     ; End EOL
                     
                     [else (write-char ch /dev/avout) (attv-normalize idx++ leader srahc (unsafe-fx+ expanded 1))]))]
            
            [(and (= idx size) (not (eq? leader '&)))
             (when (eq? leader 'xD) #| already counted on |# (write-char #\space /dev/avout))
             (if (null? rstack)
                 (w3s-remake-token vtoken xml:string (get-output-string /dev/avout))
                 expanded)]
            
            [else #false]))))

(define xml-cdata-entity-replace : (->* (XML-Token XML:String DTD-Entities (Option DTD-Entities) (Option Index) XML-XXE-Config)
                                        (Nonnegative-Fixnum (Listof Symbol) Output-Port)
                                        (U XML:String Nonnegative-Fixnum False))
  (lambda [aname vtoken entities int-entities topsize xxec [expanded 0] [rstack null] [/dev/cvout (open-output-string '/dev/cvout)]]
    (define src : String (xml:string-datum vtoken))
    (define size : Index (string-length src))
    (define false-idx : Nonnegative-Fixnum (+ size 1))
    
    (let content-normalize ([idx : Nonnegative-Fixnum 0]
                            [leader : (Option Symbol) #false]
                            [srahc : (Listof Char) null]
                            [expanded : Nonnegative-Fixnum expanded])
      (cond [(< idx size)
             (let ([ch (string-ref src idx)])
               (define idx++ : Nonnegative-Fixnum (+ idx 1))
               
               (cond [(eq? leader '&)
                      (cond [(not (eq? ch #\;)) (content-normalize idx++ leader (cons ch srahc) expanded)]
                            [(null? srahc) (make+exn:xml:malformed vtoken aname) (content-normalize false-idx leader srahc expanded)]
                            [else (let* ([estr (list->string (reverse srahc))]
                                         [ename (string->unreadable-symbol estr)]
                                         [?prev (xml-prentity-value-ref ename)]
                                         [expanded+1 (unsafe-fx+ expanded 1)])
                                    (cond [(char? ?prev)
                                           (cond [(eq? expanded topsize) (make+exn:xml:bomb vtoken aname) (content-normalize false-idx #false null expanded)]
                                                 [(eq? ?prev #\&) (write-string "&#38;" /dev/cvout) (content-normalize idx++ #false null expanded+1)]
                                                 [else (write-char ?prev /dev/cvout) (content-normalize idx++ #false null expanded+1)])]
                                          [(eq? (string-ref estr 0) #\#)
                                           (let ([replacement (xml-char-unreference vtoken estr aname)])
                                             (cond [(not replacement) (content-normalize false-idx #false null expanded)]
                                                   [(eq? expanded topsize) (make+exn:xml:bomb vtoken aname) (content-normalize false-idx #false null expanded)]
                                                   [(eq? replacement #\&) (write-string "&#38;" /dev/cvout) (content-normalize idx++ #false null expanded+1)]
                                                   [else (write-char replacement /dev/cvout) (content-normalize idx++ #false null expanded+1)]))]
                                          [(memv ename rstack) (make+exn:xml:loop vtoken aname) (content-normalize false-idx #false null expanded)]
                                          [else (let ([?etoken (xml-entity-value-token-ref vtoken ename entities int-entities aname)])
                                                  (cond [(xml:string? ?etoken)
                                                         (let ([expanded++ (xml-cdata-entity-replace aname ?etoken entities int-entities topsize xxec
                                                                                                     expanded (cons ename rstack) /dev/cvout)])
                                                           (cond [(index? expanded++) (content-normalize idx++ #false null expanded++)]
                                                                 [else (content-normalize false-idx #false null expanded)]))]
                                                        [(and ?etoken) ; bypass external entities so that they would be included literally
                                                         (write-char #\& /dev/cvout) (write ename /dev/cvout) (write-char #\; /dev/cvout)
                                                         (content-normalize idx++ #false null expanded #| yes, XXEs are not counted on |#)]
                                                        [else (content-normalize false-idx #false null expanded)]))]))])]

                     [(eq? expanded topsize) (make+exn:xml:bomb vtoken aname) (content-normalize false-idx #false null expanded)]
                     [(eq? ch #\&) (content-normalize idx++ '& srahc expanded)]
                     [else (write-char ch /dev/cvout) (content-normalize idx++ leader srahc (unsafe-fx+ expanded 1))]))]
            
            [(and (= idx size) (not leader))
             (if (null? rstack)
                 (w3s-remake-token vtoken xml:string (get-output-string /dev/cvout))
                 expanded)]
            
            [else #false]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
(define dtd-notation-cons : (-> DTD-Notation DTD-Notations DTD-Notations)
  (lambda [n notations]
    (let* ([ntoken (dtd-notation-name n)]
           [name (xml:name-datum ntoken)])
      (cond [(not (hash-has-key? notations name)) (hash-set notations name n)]
            [else (make+exn:xml:duplicate ntoken) notations]))))

(define dtd-element-cons : (-> DTD-Element DTD-Elements DTD-Elements)
  (lambda [e elements]
    (let* ([etoken (dtd-element-name e)]
           [name (xml:name-datum etoken)])
      (cond [(not (hash-has-key? elements name)) (hash-set elements name e)]
            [else (make+exn:xml:duplicate etoken) elements]))))

(define dtd-attributes-cons : (-> DTD-AttList DTD-Attributes DTD-Attributes)
  (lambda [as attributes]
    (let* ([etoken (dtd-attlist-element as)]
           [ename (xml:name-datum etoken)])
      (hash-set attributes ename
                (for/fold ([apool : (Immutable-HashTable Symbol DTD-Attribute) (hash-ref attributes ename (λ [] empty-attributes))])
                          ([a (in-list (dtd-attlist-body as))])
                  (let* ([atoken (dtd-attribute-name a)]
                         [aname (xml:name-datum atoken)])
                    (cond [(not (hash-has-key? apool aname)) (hash-set apool aname a)]
                          [else (make+exn:xml:duplicate atoken etoken) apool])))))))

(define dtd-cdata-cons : (-> (Listof XML-CDATA-Token) (Option XML-CDATA-Token) (Option XML-CDATA-Token) (Listof (U XML-Subdatum* XML-Element*))
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

    (or (and (fixnum? ?codepoint)
             (let ([?cp (natural->char-entity ?codepoint)])
               (and ?cp (integer->char ?cp))))
        (and (make+exn:xml:char etoken context)
             #false))))

(define xml-prentity-value-ref : (-> Symbol (Option Char))
  ;;; https://www.w3.org/TR/xml/#sec-predefined-ent
  (lambda [name]
    (cond [(eq? name &lt) #\u3c]
          [(eq? name &gt) #\u3e]
          [(eq? name &amp) #\u26]
          [(eq? name &apos) #\u27]
          [(eq? name &quot) #\u22]
          [else #false])))

(define xml-internal-entity-value-ref : (->* (XML-Token (U Symbol Keyword) DTD-Entities (Option DTD-Entities)) ((Option XML-Token)) (Values (Option String) Boolean))
  (lambda [ntoken name entities int-entities [context #false]]
    (define ?xstr : (Option XML:String) (xml-internal-entity-value-token-ref ntoken name entities int-entities context))
    
    (cond [(not ?xstr) (values #false #false)]
          [(xml:&string? ?xstr) (values (xml:string-datum ?xstr) #true)]
          [else (values (xml:string-datum ?xstr) #false)])))

(define xml-internal-entity-value-token-ref : (->* (XML-Token (U Symbol Keyword) DTD-Entities (Option DTD-Entities)) ((Option XML-Token)) (Option XML:String))
  (lambda [ntoken name entities ?int-entities [context #false]]
    (define e : (Option DTD-Entity)
      (cond [(not ?int-entities) (hash-ref entities name (λ [] #false))]
            [else (hash-ref ?int-entities name (λ [] (hash-ref entities name (λ [] #false))))]))
    
    (cond [(dtd-internal-entity? e) (dtd-internal-entity-value e)]
          [(dtd-unparsed-entity? e) (make+exn:xml:foreign ntoken context) #false]
          [(dtd-external-entity? e) (make+exn:xml:external ntoken context) #false]
          [else (make+exn:xml:undeclared ntoken context) #false])))

(define xml-entity-value-token-ref : (->* (XML-Token (U Symbol Keyword) DTD-Entities (Option DTD-Entities)) ((Option XML-Token)) (U XML:String Boolean))
  (lambda [ntoken name entities ?int-entities [context #false]]
    (define e : (Option DTD-Entity)
      (cond [(not ?int-entities) (hash-ref entities name (λ [] #false))]
            [else (hash-ref ?int-entities name (λ [] (hash-ref entities name (λ [] #false))))]))
    
    (cond [(dtd-internal-entity? e) (dtd-internal-entity-value e)]
          [(dtd-unparsed-entity? e) (make+exn:xml:foreign ntoken context) #false]
          [(dtd-external-entity? e) #true]
          [else (make+exn:xml:undeclared ntoken context) #false])))

(define xml-entity-value-tokens-ref : (->* ((U XML:Reference XML:PEReference)
                                            (U Symbol Keyword) DTD-Entities (Option DTD-Entities)
                                            (-> XML-Token XML:String DTD-Entities (Option DTD-Entities) (Option Index) XML-XXE-Config Any) (Option Index)
                                            (-> XML:String (Listof XML-Token))
                                            XML-XXE-Config (XML-XXE-Reader (Listof XML-Token)))
                                           ((Option XML-Token))
                                           (U (Listof XML-Token) XML-Syntax-Error))
  (lambda [ntoken name entities ?int-entities replace topsize value->tokens xxec read-xxe [context #false]]
    (define e : (Option DTD-Entity)
      (cond [(not ?int-entities) (hash-ref entities name (λ [] #false))]
            [else (hash-ref ?int-entities name (λ [] (hash-ref entities name (λ [] #false))))]))
    
    (cond [(dtd-token-entity? e)
           (let ([?tokens (dtd-token-entity-body e)])
             (cond [(list? ?tokens) ?tokens]
                   [else (let* ([ntoken (dtd-entity-name e)]
                                [vtoken (dtd-internal-entity-value e)]
                                [vflattened (if (xml-entity-value-normalize? vtoken topsize) (replace ntoken vtoken entities ?int-entities topsize xxec) vtoken)]
                                [ts (if (xml:string? vflattened) (value->tokens vflattened) null)])
                           (set-dtd-token-entity-body! e ts) ts)]))]
          [(dtd-unparsed-entity? e) (make+exn:xml:foreign ntoken context)]
          [(dtd-external-entity? e)
           (cond [(not xxec) (make+exn:xml:external ntoken context)]
                 [else (or (xml-load-external-entity (dtd-entity-name e) (dtd-external-entity-public e) (dtd-external-entity-system e)
                                                     (vector-ref xxec 0) (vector-ref xxec 1) (vector-ref xxec 2)
                                                     read-xxe)
                           null)])]
          [else (make+exn:xml:undeclared ntoken context)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-load-external-entity : (All (E) (-> (Option XML-Token) (Option XML:String) (Option XML:String)
                                                Open-Input-XML-XXE (Option Index) (Option Real) (XML-XXE-Reader E)
                                                (Option E)))
  (lambda [content public system open-port topsize0 timeout read-entity]
    (and (or public system) ; should not happen
         (parameterize ([current-custodian (make-custodian)])
           (define source : (U Symbol String) (w3s-token-source (or public system)))
           (define rootdir : Path-String
             (or (and (string? source)
                      (cond [(file-exists? source) (path-only source) #| local path |#]
                            [else (string-trim source #px"[^/]" #:left? #false #:repeat? #true) #| remote path |#]))
                 (current-directory)))

           (define (read-xxe) : (U E XML-Syntax-Error)
             (define &port-name : (Boxof (U False String Symbol)) (box #false))
             (define ?port : (U Input-Port (Pairof Input-Port Boolean) False)
               (open-port rootdir
                          (and public (xml:string-datum public))
                          (and system (xml:string-datum system))
                          topsize0 &port-name))

             (define-values (/dev/rawin topsize)
               (cond [(input-port? ?port) (values ?port topsize0)]
                     [(not ?port) (values #false #false)]
                     [(cdr ?port) (values (car ?port) #false)]
                     [else (values (car ?port) topsize0)]))
             
             (if (not /dev/rawin)
                 (make+exn:xml:defense (filter xml:string? (list system public)) content)
                 
                 (let ([/dev/entin (if (not topsize) /dev/rawin (make-limited-input-port /dev/rawin topsize #false))])
                   (unless (port-counts-lines? /dev/entin)
                     (port-count-lines! /dev/entin))
                   (let ([entity (read-entity /dev/entin (unbox &port-name))])
                     (cond [(eof-object? (peek-char /dev/rawin)) entity]
                           [else (make+exn:xml:bomb (filter xml:string? (list system public)) content)])))))

           (define ?entity : (U E XML-Syntax-Error False)
             (if (and (real? timeout) (> timeout 0.0))
                 (let* ([xxe : (Channelof (U XML-Syntax-Error E)) (make-channel)]
                        [ghostcat (thread (λ [] (channel-put xxe (read-xxe))))])
                   (sync/timeout/enable-break timeout xxe))
                 (read-xxe)))

           (custodian-shutdown-all (current-custodian))
             
           (cond [(not ?entity) (make+exn:xml:timeout (filter xml:string? (list system public)) content) #false]
                 [(exn:xml? ?entity) #false]
                 [else ?entity])))))

(define xml-make-entity->tokens : (All (T) (-> T (-> Input-Port (U String Symbol) T (Listof XML-Token)) (-> XML:String (Listof XML-Token))))
  (lambda [scope read-tokens]
    (λ [[tv : XML:String]]
      (define source : String (w3s-token-location-string tv))
      (define /dev/subin : Input-Port (dtd-open-input-port (xml:string-datum tv) #true source))
      
      (read-tokens /dev/subin source scope))))

(define xml-dtd-reader : (XML-XXE-Reader (Listof XML-Token))
  (lambda [/dev/dtdin [port-name #false]]
    (read-xml-tokens* /dev/dtdin (or port-name (sgml-port-name /dev/dtdin)) #false)))

(define xml-cdata-reader : (XML-XXE-Reader String)
  ;;; https://www.w3.org/TR/xml/#sec-line-ends
  (lambda [/dev/entin [port-name #false]]
    (define /dev/entout : Output-Port (open-output-string '/dev/entout))

    (xml-skip-whitespace /dev/entin)
      
    (for ([line (in-bytes-lines /dev/entin)])
      (write-bytes line /dev/entout)
      (write-char #\newline /dev/entout))
    
    (string-trim (get-output-string /dev/entout) #:left? #false)))

(define xml-make-cdata-reader : (-> XML:Reference (XML-XXE-Reader (List XML:String)))
  (lambda [e]
    (λ [[/dev/entin : Input-Port] [port-name : (U False String Symbol) #false]]
      (list (w3s-remake-token e xml:string (xml-cdata-reader /dev/entin port-name))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml:lang-ref : (case-> [String -> (Option String)]
                               [XML-Token XML-Element-Attribute* -> (Option String)])
  (case-lambda
    [(lang)
     (and (> (string-length lang) 0) lang)]
    [(tagname attr)
     (let ([v (cdr attr)])
       (and (xml:string? v)
            (xml:lang-ref (xml:string-datum v))))]))

(define xml:space-ref : (-> XML-Token XML-Element-Attribute* Symbol Symbol)
  (lambda [tagname attr inherited-space]
    (define attval : XML-Element-Attribute-Value* (cdr attr))
    (define this:space : Symbol
      (cond [(xml:name? attval) (xml:name-datum attval)]
            [(xml:string? attval) (string->symbol (xml-attribute-token-value-consolidate attval))]
            [else '||]))

    (cond [(eq? this:space 'preserve) 'preserve]
          [(eq? this:space 'default) (default-xml:space)]
          [else (make+exn:xml:enum (if (list? attval) (cons (car attr) attval) (list (car attr) attval)) tagname) inherited-space])))

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
                              (Boolean)
                              (Listof (U XML-Subdatum* XML-Element*)))
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

(define xml-attribute-token-value-consolidate : (-> XML:String String)
  (lambda [src]
    (define trimmed : String (string-trim (xml:string-datum src)))

    (cond [(not (string-contains? trimmed "  ")) trimmed]
          [else (string-replace trimmed #px" +" " ")])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-xml:space : (Parameterof Symbol) (make-parameter 'default))

(define-type XML-XXE-Config (Option (Vector Open-Input-XML-XXE (Option Index) (Option Real))))

(define empty-entities : DTD-Entities (make-immutable-hasheq))
(define empty-notations : DTD-Notations (make-immutable-hasheq))
(define empty-elements : DTD-Elements (make-immutable-hasheq))
(define empty-element-attributes : DTD-Attributes (make-immutable-hasheq))
(define empty-attributes : (Immutable-HashTable Symbol DTD-Attribute) (make-immutable-hasheq))

(define name-placeholder : XML:Name (xml:name '|| 1 0 1 1 '||))
(define undeclared-attribute : DTD-Attribute (dtd-attribute name-placeholder name-placeholder dtd:attribute:cdata))

(define xml:space-values : (-> Symbol (Option String) Char (Option Char))
  (lambda [tag xml:lang char]
    char))

(define xml-entity-value-normalize? : (-> Any (Option Index) Boolean : #:+ XML:&String)
  (lambda [token topsize]
    (and (xml:&string? token)
         (not (eq? topsize 0)))))
