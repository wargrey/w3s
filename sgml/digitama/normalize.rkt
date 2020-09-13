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
(define xml-normalize : (-> XML-DTD (Listof XML-Content*) (Values XML-Type (Listof XML-Content*)))
  (lambda [int-dtd content]
    (define dtype : XML-Type (xml-dtd-expand int-dtd))
    (define entities : XML-Type-Entities (xml-type-entities dtype))

    (let xml-content-normalize ([rest : (Listof XML-Content*) content]
                                [clear-content : (Listof XML-Content*) null])
      (cond [(null? rest) (values dtype (reverse clear-content))]
            [else (let-values ([(self rest++) (values (car rest) (cdr rest))])
                    (cond [(list? self)
                           (let ([?elem (xml-element-normalize self entities 0)])
                             (xml-content-normalize rest++ (if (exn:xml:loop? ?elem) clear-content (cons ?elem clear-content))))]
                          [else (xml-content-normalize rest++ (cons self clear-content))]))]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-dtd-expand : (-> XML-DTD XML-Type)
  (lambda [int-dtd]
    (let expand-dtd ([rest : (Listof XML-Type-Declaration*) (xml-dtd-declarations int-dtd)]
                     [entities : XML-Type-Entities (make-immutable-hasheq)]
                     [notations : XML-Type-Notations (make-immutable-hasheq)]
                     [attributes : XML-Type-Attributes (make-immutable-hasheq)])
      (cond [(null? rest) (xml-type entities notations attributes)]
            [else (let-values ([(self rest++) (values (car rest) (cdr rest))])
                    (cond [(xml-entity? self)
                           ; NOTE: NDATA only concerns validity constraint, and therefore is not handled here. 
                           (if (xml-entity-value self)
                               (expand-dtd rest++ (xml-entity-cons (xml-dtd-included-as-literal self entities) entities) notations attributes)
                               (expand-dtd rest++ (xml-entity-cons self entities) notations attributes))]
                          [(xml-attribute-list? self) (expand-dtd rest++ entities notations (xml-attributes-cons self attributes))]
                          [(xml:pereference? self) (expand-dtd (append (xml-dtd-included-as-PE self entities) rest++) entities notations attributes)]
                          [(pair? self) (expand-dtd (append (xml-dtd-expand-section (car self) (cdr self) entities) rest++) entities notations attributes)]
                          [(xml-notation? self) (expand-dtd rest++ entities (xml-notation-cons self notations) attributes)]
                          [else (expand-dtd rest++ entities notations attributes)]))]))))

(define xml-dtd-expand-section : (-> (U XML:Name XML:PEReference) (Listof XML-Type-Declaration*) XML-Type-Entities (Listof XML-Type-Declaration*))
  (lambda [condition body entities]
    (define sec-name : (U False Symbol String)
      (cond [(xml:name? condition) (xml:name-datum condition)]
            [else (let-values ([(?value ge?) (xml-entity-value-ref condition (xml:pereference-datum condition) entities)])
                    ?value)]))

    (cond [(or (eq? sec-name 'INCLUDE) (equal? sec-name "INCLUDE")) body]
          [(or (eq? sec-name 'IGNORE) (equal? sec-name "IGNORE")) null]
          [else null])))

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
    (define vtoken : (Option XML:String) (xml-entity-value-token-ref pe (xml:pereference-datum pe) entities))

    ;;; NOTE
    ; the `plain-value` is not surrounded by spaces since the source has already been tokenized, and
    ; the spaces would be inserted when writing the document object into a file.
    
    (cond [(not vtoken) null]
          [else (let ([pe-body (read-xml-type-definition (xml:string-datum vtoken) (w3s-token-location-string vtoken))])
                  (xml-dtd-declarations pe-body))])))

(define xml-dtd-included : (-> XML:Name XML:Reference XML-Type-Entities Index (Listof (U XML-Subdatum* XML-Element*)))
  ;;; https://www.w3.org/TR/xml11/#included
  (lambda [tagname e entities depth]
    (define name : Symbol (xml:reference-datum e))
    (define prentity-value : (Option String) (xml-prentity-value-ref name))

    (cond [(and prentity-value) (list (w3s-remake-token e xml:string prentity-value))]
          [else (let ([tv (xml-entity-value-token-ref e name entities tagname)])
                  (cond [(not tv) null]
                        [else (let*-values ([(tag-name) (symbol->immutable-string (xml:name-datum tagname))]
                                            [(source) (w3s-token-location-string tv)]
                                            [(/dev/subin) (dtd-open-input-port (xml:string-datum tv) #true source)]
                                            [(tokens) (read-xml-content-tokens* /dev/subin source depth)]
                                            [(children rest) (xml-syntax-extract-subelement* tagname tokens #true)])
                                (cond [(and children (null? rest)) children]
                                      [else (make+exn:xml:malformed e tagname) null]))]))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-element-normalize : (->* (XML-Element* XML-Type-Entities Index) ((Listof Symbol)) (U XML-Element* exn:xml:loop))
  (lambda [e entities depth [recursion-stack null]]
    (define tagname : XML:Name (car e))
    
    (define attributes : (Listof (Pairof XML:Name XML:String))
      (let attribute-filter-map ([rest : (Listof (Pairof XML:Name XML:String)) (cadr e)]
                                 [setubirtta : (Listof (Pairof XML:Name XML:String)) null]
                                 [names : (Listof Symbol) null])
        (cond [(null? rest) (reverse setubirtta)]
              [else (let*-values ([(self rest++) (values (car rest) (cdr rest))]
                                  [(?attr) (xml-element-attribute-normalize self entities names tagname)])
                      (cond [(not ?attr) (attribute-filter-map rest++ setubirtta names)]
                            [else (attribute-filter-map rest++ (cons ?attr setubirtta)
                                                        (cons (xml:name-datum (car ?attr)) names))]))])))

    (define ?children : (U (Listof (U XML-Subdatum* XML-Element*)) exn:xml:loop)
      (xml-subelement-normalize tagname (caddr e) entities (assert (+ depth 1) index?)
                                recursion-stack))
    
    (cond [(list? ?children) (list tagname attributes ?children)]
          [else ?children])))

(define xml-element-attribute-normalize : (->* ((Pairof XML:Name XML:String) XML-Type-Entities (Listof Symbol))
                                               ((Option XML-Token))
                                               (Option (Pairof XML:Name XML:String)))
  (lambda [name=value entities attrnames [tagname #false]]
    (let ([name (car name=value)]
          [value (cdr name=value)])
      (cond [(memq (xml:name-datum name) attrnames) (make+exn:xml:unique name tagname) #false]
            [(not (xml:&string? value)) name=value]
            [else (let ([?value (xml-attr-entity-replace name value entities)])
                    (and (xml:string? ?value)
                         (cons name ?value)))]))))

(define xml-subelement-normalize : (-> XML:Name (Listof (U XML-Subdatum* XML-Element*)) XML-Type-Entities Index (Listof Symbol)
                                       (U (Listof (U XML-Subdatum* XML-Element*)) exn:xml:loop))
  (lambda [tagname children entities depth recursion-stack]
    (let normalize-subelement ([rest : (Listof (U XML-Subdatum* XML-Element*)) children]
                               [nerdlihc : (Listof (U XML-Subdatum* XML-Element*)) null]
                               [rstack : (Listof Symbol) recursion-stack])
      (cond [(null? rest) (reverse nerdlihc)]
            [else (let-values ([(self rest++) (values (car rest) (cdr rest))])
                    (cond [(list? self)
                           (let ([?elem (xml-element-normalize self entities depth rstack)])
                             (cond [(list? ?elem) (normalize-subelement rest++ (cons ?elem nerdlihc) rstack)]
                                   [else ?elem]))]
                          [(xml:reference? self)
                           (let ([selfname (xml:reference-datum self)])
                             (cond [(memq selfname rstack) (make+exn:xml:loop self tagname)]
                                   [else (let* ([fly-elements (xml-dtd-included tagname self entities depth)]
                                                [?elements (normalize-subelement fly-elements null (cons selfname rstack))])
                                           (cond [(list? ?elements) (normalize-subelement rest++ (append (reverse ?elements) nerdlihc) rstack)]
                                                 [(null? rstack) (normalize-subelement rest++ nerdlihc rstack)]
                                                 [else ?elements]))]))]
                          [(xml:char? self)
                           (let ([content (w3s-remake-token self xml:string (string (integer->char (xml:char-datum self))))])
                             (normalize-subelement rest++ (cons content nerdlihc) rstack))]
                          [else (normalize-subelement rest++ (cons self nerdlihc) rstack)]))]))))

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
                     [(eq? ch #\u0085) (attr-value-normalize idx++ leader srahc (cons #\space eulav))]
                     [(eq? ch #\u2028) (attr-value-normalize idx++ leader srahc (cons #\space eulav))]
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
  (lambda [e notations]
    (let* ([ntoken (xml-notation-name e)]
           [name (xml:name-datum ntoken)])
      (cond [(not (hash-has-key? notations name)) (hash-set notations name e)]
            [else (make+exn:xml:duplicate ntoken) notations]))))

(define xml-attributes-cons : (-> XML-Type-Attribute-List XML-Type-Attributes XML-Type-Attributes)
  (let ([empty-attributes ((inst make-immutable-hasheq Symbol XML-Attribute))])
    (lambda [as attributes]
      (let* ([etoken (xml-attribute-list-element as)]
             [ename (xml:name-datum etoken)])
        (hash-set attributes ename
                  (for/fold ([apool : (Immutable-HashTable Symbol XML-Attribute) (hash-ref attributes ename (λ [] empty-attributes))])
                            ([a (in-list (xml-attribute-list-body as))])
                    (let* ([atoken (xml-attribute-name a)]
                           [aname (xml:name-datum atoken)])
                      (cond [(not (hash-has-key? apool aname)) (hash-set apool aname a)]
                            [else (make+exn:xml:duplicate atoken etoken) apool]))))))))

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
