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
                    (cond [(list? self) (xml-content-normalize rest++ (cons (xml-element-normalize self entities 0) clear-content))]
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
                                 [else (expand-dtd rest++ (xml-entity-cons (xml-dtd-included-as-literal self entities #:GE-bypass? #true) entities))])]
                          [(xml:pereference? self) (expand-dtd (append (xml-dtd-included-as-PE self entities) rest++) entities)]
                          [(pair? self) (expand-dtd (append (xml-dtd-expand-section (car self) (cdr self) entities) rest++) entities)]
                          [else (expand-dtd rest++ entities)]))]))))

(define xml-dtd-expand-section : (-> (U XML:Name XML:PEReference) (Listof XML-Type-Declaration*) XML-Type-Entities (Listof XML-Type-Declaration*))
  (lambda [condition body entities]
    (define sec-name : (U False Symbol String)
      (cond [(xml:name? condition) (xml:name-datum condition)]
            [else (xml-pentity-value-ref condition entities)]))

    (cond [(or (eq? sec-name 'INCLUDE) (equal? sec-name "INCLUDE")) body]
          [(or (eq? sec-name 'IGNORE) (equal? sec-name "IGNORE")) null]
          [else (make+exn:xml:unrecognized condition) null])))

(define xml-dtd-included-as-literal : (-> XML-Entity XML-Type-Entities #:GE-bypass? Boolean (Option XML-Entity))
  ;;; https://www.w3.org/TR/xml11/#inliteral
  ;;; https://www.w3.org/TR/xml11/#bypass
  (lambda [e entities #:GE-bypass? ge-bypass?]
    (define ?value (xml-entity-value e))
    
    (cond [(not (xml:&string? ?value)) e]
          [else (let ([plain-value (xml-entity-replacement-text (xml:string-datum (xml-entity-value e)) entities #:GE-bypass? ge-bypass?)])
                  (cond [(not plain-value) (make+exn:xml:unrecognized ?value (xml-entity-name e)) #false]
                        [else (struct-copy xml-entity e
                                           [value (xml-remake-token ?value xml:string plain-value)])]))])))

(define xml-dtd-included-as-PE : (-> XML:PEReference XML-Type-Entities (Listof XML-Type-Declaration*))
  ;;; https://www.w3.org/TR/xml11/#as-PE
  (lambda [pe entities]
    (define vtoken : (Option XML:String) (xml-pentity-value-token-ref pe entities))

    ;;; NOTE
    ; the `plain-value` is not surrounded by spaces since the source has already been tokenized, and
    ; the spaces would be inserted when writing the document object into a file.
    
    (cond [(not vtoken) (make+exn:xml:unrecognized pe) null]
          [else (let ([pe-body (read-xml-type-definition (xml:string-datum vtoken) (xml-token-location-string vtoken))])
                  (xml-dtd-declarations pe-body))])))

(define xml-dtd-included : (-> XML:Name XML:Reference XML-Type-Entities Index (Listof (U XML-Subdatum* XML-Element*)))
  ;;; https://www.w3.org/TR/xml11/#included
  (lambda [tagname e entities depth]
    (define name : Symbol (xml:reference-datum e))
    (define prentity-value : (Option String) (xml-prentity-value-ref name))

    (cond [(and prentity-value) (list (xml-remake-token e xml:string prentity-value))]
          [else (let ([tv (xml-entity-value-token-ref name entities)])
                  (cond [(not tv) (make+exn:xml:unrecognized e) null]
                        [else (let*-values ([(tag-name) (symbol->immutable-string (xml:name-datum tagname))]
                                            [(source) (xml-token-location-string tv)]
                                            [(/dev/subin) (dtd-open-input-port (string-append (xml:string-datum tv) "</" tag-name ">") #true source)]
                                            [(tokens) (read-xml-content-tokens* /dev/subin source depth)]
                                            [(children rest) (xml-syntax-extract-subelement* tagname tokens)])
                                (cond [(and children (null? rest)) children]
                                      [else (make+exn:xml:malformed e tagname) null]))]))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-element-normalize : (-> XML-Element* XML-Type-Entities Index XML-Element*)
  (lambda [e entities depth]
    (define tagname : XML:Name (car e))
    
    (list tagname
          (filter-map (λ [[name=value : (Pairof XML:Name XML:String)]]
                        (xml-element-attribute-normalize name=value entities))
                      (cadr e))
          (xml-subelement-normalize tagname (caddr e) entities
                                    (assert (+ depth 1) index?)))))

(define xml-element-attribute-normalize : (-> (Pairof XML:Name XML:String) XML-Type-Entities (Option (Pairof XML:Name XML:String)))
  (lambda [name=value entities]
    (let ([value (cdr name=value)])
      (cond [(not (xml:&string? value)) name=value]
            [else (let ([?value (xml-entity-replacement-text (xml:string-datum value) entities #:GE-bypass? #false #:PE-unrecognize? #true)])
                    (cond [(not ?value) (make+exn:xml:unrecognized value (car name=value)) #false]
                          [else (cons (car name=value)
                                      (xml-remake-token value xml:string ?value))]))]))))

(define xml-subelement-normalize : (-> XML:Name (Listof (U XML-Subdatum* XML-Element*)) XML-Type-Entities Index (Listof (U XML-Subdatum* XML-Element*)))
  (lambda [tagname children entities depth]
    (let normalize-subelement ([rest : (Listof (U XML-Subdatum* XML-Element*)) children]
                               [nerdlihc : (Listof (U XML-Subdatum* XML-Element*)) null])
      (cond [(null? rest) (reverse nerdlihc)]
            [else (let-values ([(self rest++) (values (car rest) (cdr rest))])
                    (cond [(list? self) (normalize-subelement rest++ (cons (xml-element-normalize self entities depth) nerdlihc))]
                          [(xml:reference? self) (normalize-subelement (append (xml-dtd-included tagname self entities depth) rest++) nerdlihc)]
                          [(xml:char? self)
                           (let ([content (xml-remake-token self xml:string (string (integer->char (xml:char-datum self))))])
                             (normalize-subelement rest++ (cons content nerdlihc)))]
                          [else (normalize-subelement rest++ (cons self nerdlihc))]))]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-entity-replacement-text : (-> String XML-Type-Entities #:GE-bypass? Boolean [#:PE-unrecognize? Boolean] (Option String))
  ;;; https://www.w3.org/TR/xml11/#intern-replacement
  (let ([/dev/strout (open-output-string '/dev/entout)])
    (lambda [src entities #:GE-bypass? ge-bypass? #:PE-unrecognize? [pe? #false]]
      (define size : Index (string-length src))

      (define okay? : (Option Void)
        (let entity-normalize ([idx : Nonnegative-Fixnum 0]
                               [srahc : (Listof Char) null])
          (when (< idx size)
            (define ch : Char (unsafe-string-ref src idx))
            (define idx++ : Nonnegative-Fixnum (+ idx 1))
            
            (if (null? srahc)
                (cond [(or (eq? ch #\%) (eq? ch #\&)) (entity-normalize idx++ (list ch))]
                      [else (write-char ch /dev/strout) (entity-normalize idx++ srahc)])
                (cond [(not (eq? ch #\;)) (entity-normalize idx++ (cons ch srahc))]
                      [else (let ([entity (reverse srahc)])
                              (and (pair? entity)
                                   (let-values ([(leader nchars) (values (car entity) (cdr entity))])
                                     (and (pair? nchars)
                                          (let ([estr (list->string nchars)])
                                            (define-values (replacement hintsign)
                                              (cond [(eq? leader #\%)
                                                     (cond [(not pe?) (values estr #\%)]
                                                           [else (values (xml-entity-value-ref (string->keyword estr) entities)
                                                                         #false)])]
                                                    [(eq? (car nchars) #\#) (values (xml-char-reference estr) #false)]
                                                    [(not ge-bypass?) (values (xml-entity-value-ref (string->unreadable-symbol estr) entities) #false)]
                                                    [else (values estr #\&)]))

                                            (and (string? replacement)
                                                 (when (and hintsign) (write-char hintsign /dev/strout))
                                                 (write-string replacement /dev/strout)
                                                 (when (and hintsign) (write-char #\; /dev/strout))
                                                 (entity-normalize idx++ null)))))))])))))

      (let ([?value (get-output-bytes /dev/strout #true)])
        (and (void? okay?)
             (bytes->string/utf-8 ?value #\uFFFD))))))

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

(define xml-pentity-value-token-ref : (-> XML:PEReference XML-Type-Entities (Option XML:String))
  (lambda [pe entities]
    (define ?vtoken : (Option XML:String) (xml-entity-value-token-ref (xml:pereference-datum pe) entities))

    (cond [(xml:&string? ?vtoken)
           (let ([?str (xml-entity-replacement-text (xml:string-datum ?vtoken) entities #:GE-bypass? #true)])
             (and ?str (xml-remake-token ?vtoken xml:string ?str)))]
          [(xml:string? ?vtoken) ?vtoken]
          [else #false])))

(define xml-pentity-value-ref : (-> XML:PEReference XML-Type-Entities (Option String))
  (lambda [pe entities]
    (define ?vtoken : (Option XML:String) (xml-pentity-value-token-ref pe entities))

    (and ?vtoken (xml:string-datum ?vtoken))))

(define xml-prentity-value-ref : (-> Symbol (Option String))
  ;;; https://www.w3.org/TR/xml11/#sec-predefined-ent
  (lambda [name]
    (cond [(eq? name &lt) "\x3c"]
          [(eq? name &gt) "\x3e"]
          [(eq? name &amp) "\x26"]
          [(eq? name &apos) "\x27"]
          [(eq? name &quot) "\x22"]
          [else #false])))

(define xml-entity-value-ref : (-> (U Symbol Keyword) XML-Type-Entities (Option String))
  (lambda [name entities]
    (or (and (symbol? name) (xml-prentity-value-ref name))
        (let ([?xstr (xml-entity-value-token-ref name entities)])
          (and ?xstr (xml:string-datum ?xstr))))))
