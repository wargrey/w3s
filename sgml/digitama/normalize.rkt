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
(define xml-normalize : (-> XML-DTD (Listof XML-DTD) (Listof XML-Content*) (Values XML-Type (Listof XML-Content*)))
  (lambda [int-dtd ext-dtds content]
    (define dtype : XML-Type (xml-dtd-expand int-dtd ext-dtds))
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
(define xml-dtd-expand : (-> XML-DTD (Listof XML-DTD) XML-Type)
  (lambda [int-dtd ext-dtds]
    (let expand-dtd ([rest : (Listof XML-Type-Declaration*) (xml-dtd-declarations int-dtd)]
                     [entities : XML-Type-Entities (make-immutable-hasheq)])
      (cond [(null? rest) (xml-type entities)]
            [else (let-values ([(self rest++) (values (car rest) (cdr rest))])
                    (cond [(xml-entity? self)
                           (cond [(not (xml-entity-value self)) (expand-dtd rest++ (xml-entity-cons self entities))]
                                 [else (expand-dtd rest++ (xml-entity-cons (xml-dtd-included-as-literal self entities) entities))])]
                          [(xml:pereference? self) (expand-dtd (append (xml-dtd-included-as-PE self entities) rest++) entities)]
                          [(pair? self) (expand-dtd (append (xml-dtd-expand-section (car self) (cdr self) entities) rest++) entities)]
                          [else (expand-dtd rest++ entities)]))]))))

(define xml-dtd-expand-section : (-> (U XML:Name XML:PEReference) (Listof XML-Type-Declaration*) XML-Type-Entities (Listof XML-Type-Declaration*))
  (lambda [condition body entities]
    (define sec-name : (U False Symbol String)
      (cond [(xml:name? condition) (xml:name-datum condition)]
            [else (let-values ([(?value ge?) (xml-entity-value-ref (xml:pereference-datum condition) entities)])
                    ?value)]))

    (cond [(or (eq? sec-name 'INCLUDE) (equal? sec-name "INCLUDE")) body]
          [(or (eq? sec-name 'IGNORE) (equal? sec-name "IGNORE")) null]
          [else (make+exn:xml:unrecognized condition) null])))

(define xml-dtd-included-as-literal : (-> XML-Entity XML-Type-Entities (Option XML-Entity))
  ;;; https://www.w3.org/TR/xml11/#inliteral
  ;;; https://www.w3.org/TR/xml11/#bypass
  (lambda [e entities]
    (define ?value (xml-entity-value e))
    
    (cond [(not (xml:&string? ?value)) e]
          [else (let ([?new-value (xml-dtd-entity-replacement-text ?value entities)])
                  (cond [(not ?new-value) (make+exn:xml:unrecognized ?value (xml-entity-name e)) #false]
                        [else (struct-copy xml-entity e [value ?new-value])]))])))

(define xml-dtd-included-as-PE : (-> XML:PEReference XML-Type-Entities (Listof XML-Type-Declaration*))
  ;;; https://www.w3.org/TR/xml11/#as-PE
  (lambda [pe entities]
    (define vtoken : (Option XML:String) (xml-entity-value-token-ref (xml:pereference-datum pe) entities))

    ;;; NOTE
    ; the `plain-value` is not surrounded by spaces since the source has already been tokenized, and
    ; the spaces would be inserted when writing the document object into a file.
    
    (cond [(not vtoken) (make+exn:xml:unrecognized pe) null]
          [else (let ([pe-body (read-xml-type-definition (xml:string-datum vtoken) (w3s-token-location-string vtoken))])
                  (xml-dtd-declarations pe-body))])))

(define xml-dtd-included : (-> XML:Name XML:Reference XML-Type-Entities Index (Listof (U XML-Subdatum* XML-Element*)))
  ;;; https://www.w3.org/TR/xml11/#included
  (lambda [tagname e entities depth]
    (define name : Symbol (xml:reference-datum e))
    (define prentity-value : (Option String) (xml-prentity-value-ref name))

    (cond [(and prentity-value) (list (w3s-remake-token e xml:string prentity-value))]
          [else (let ([tv (xml-entity-value-token-ref name entities)])
                  (cond [(not tv) (make+exn:xml:unrecognized e) null]
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
      (filter-map (λ [[name=value : (Pairof XML:Name XML:String)]]
                        (xml-element-attribute-normalize name=value entities))
                  (cadr e)))

    (define ?children : (U (Listof (U XML-Subdatum* XML-Element*)) exn:xml:loop)
      (xml-subelement-normalize tagname (caddr e) entities (assert (+ depth 1) index?)
                                recursion-stack))
    
    (cond [(list? ?children) (list tagname attributes ?children)]
          [else ?children])))

(define xml-element-attribute-normalize : (-> (Pairof XML:Name XML:String) XML-Type-Entities (Option (Pairof XML:Name XML:String)))
  (lambda [name=value entities]
    (let ([value (cdr name=value)])
      (cond [(not (xml:&string? value)) name=value]
            [else (let ([?value (xml-dtd-entity-replacement-text value entities)])
                    (cond [(not ?value) (make+exn:xml:unrecognized value (car name=value)) #false]
                          [else (cons (car name=value) ?value)]))]))))

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

(define xml-dtd-entity-replacement-text : (-> XML:&String XML-Type-Entities (Option XML:String))
  (let ([/dev/eout (open-output-string '/dev/eout)])
    (lambda [etoken entities]
      (define src : String (xml:string-datum etoken))
      (define size : Index (string-length src))
      (define false-idx : Nonnegative-Fixnum (+ size 1))
      
      (let dtd-entity-normalize ([idx : Nonnegative-Fixnum 0]
                                 [leader : (Option Char) #false]
                                 [srahc : (Listof Char) null]
                                 [ge-exists? : Boolean #false])
          (cond [(< idx size)
                 (let ([ch (unsafe-string-ref src idx)])
                   (define idx++ : Nonnegative-Fixnum (+ idx 1))
            
                   (if (not leader)
                       (cond [(or (eq? ch #\%) (eq? ch #\&)) (dtd-entity-normalize idx++ ch srahc ge-exists?)]
                             [else (write-char ch /dev/eout) (dtd-entity-normalize idx++ leader srahc ge-exists?)])
                       (cond [(not (eq? ch #\;)) (dtd-entity-normalize idx++ leader (cons ch srahc) ge-exists?)]
                             [(null? srahc) (dtd-entity-normalize false-idx leader srahc ge-exists?)]
                             [else (let ([estr (list->string (reverse srahc))])
                                     (define-values (replacement ge?)
                                       (cond [(eq? leader #\%) (xml-entity-value-ref (string->keyword estr) entities)]
                                             [(eq? (unsafe-string-ref estr 0) #\#) (values (xml-char-reference estr) #false)]
                                             [else (values estr #\&)]))
                                     
                                     (cond [(not (string? replacement)) (dtd-entity-normalize false-idx #false null ge-exists?)]
                                           [(not ge?) (write-string replacement /dev/eout) (dtd-entity-normalize idx++ #false null ge-exists?)]
                                           [else (write-char #\& /dev/eout)
                                                 (write-string replacement /dev/eout)
                                                 (write-char #\; /dev/eout)
                                                 (dtd-entity-normalize idx++ #false null #true)]))])))]
                [(= idx size)
                 (let ([new-value : String (bytes->string/utf-8 (get-output-bytes /dev/eout #true) #\uFFFD)])
                   (cond [(string=? new-value src) etoken]
                         [(not ge-exists?) (w3s-remake-token etoken xml:string new-value)]
                         [else (w3s-remake-token etoken xml:&string new-value)]))]
                
                [else (not (get-output-bytes /dev/eout #true))])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-entity-cons : (-> (Option XML-Entity) XML-Type-Entities XML-Type-Entities)
  (lambda [e entities]
    (cond [(not e) entities]
          [else (let* ([ntoken (xml-entity-name e)]
                       [name (if (xml:reference? ntoken) (xml:reference-datum ntoken) (xml:pereference-datum ntoken))])
                  (cond [(not (hash-has-key? entities name)) (hash-set entities name e)]
                        [else (make+exn:xml:duplicate ntoken) entities]))])))

(define xml-char-reference : (-> String (Option String))
  (lambda [estr]
    (define ?codepoint : (Option Complex)
      (if (regexp-match? #rx"^#x" estr)
          (string->number (substring estr 2) 16)
          (string->number (substring estr 1) 10)))

    (and (index? ?codepoint)
         (string (integer->char ?codepoint)))))

(define xml-entity-value-token-ref : (-> (U Symbol Keyword) XML-Type-Entities (Option XML:String))
  (lambda [name entities]
    (let ([e (hash-ref entities name (λ [] #false))])
      (and e
           (let ([?vtoken (xml-entity-value e)])
             (and ?vtoken ?vtoken))))))

(define xml-prentity-value-ref : (-> Symbol (Option String))
  ;;; https://www.w3.org/TR/xml11/#sec-predefined-ent
  (lambda [name]
    (cond [(eq? name &lt) "\x3c"]
          [(eq? name &gt) "\x3e"]
          [(eq? name &amp) "\x26"]
          [(eq? name &apos) "\x27"]
          [(eq? name &quot) "\x22"]
          [else #false])))

(define xml-entity-value-ref : (-> (U Symbol Keyword) XML-Type-Entities (Values (Option String) Boolean))
  (lambda [name entities]
    (let ([?xstr (xml-entity-value-token-ref name entities)])
      (cond [(not ?xstr) (values #false #false)]
            [(xml:&string? ?xstr) (values (xml:string-datum ?xstr) #true)]
            [else (values (xml:string-datum ?xstr) #false)]))))
