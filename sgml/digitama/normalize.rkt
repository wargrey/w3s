#lang typed/racket/base

(provide (all-defined-out))

(require typed/racket/unsafe)

(require "grammar.rkt")
(require "dtd.rkt")

(require "digicore.rkt")

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
                    (cond [(list? self) (xml-content-normalize rest++ (cons (xml-element-normalize self entities) clear-content))]
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
                                 [else (expand-dtd rest++ (xml-entity-cons (xml-dtd-expand-entity self entities) entities))])]
                          [(xml:pereference? self) (expand-dtd (append (xml-dtd-expand-pentity self entities) rest++) entities)]
                          [(pair? self) (expand-dtd (append (xml-dtd-expand-section (car self) (cdr self) entities) rest++) entities)]
                          [else (expand-dtd rest++ entities)]))]))))

(define xml-dtd-expand-entity : (-> XML-Entity XML-Type-Entities (Option XML-Entity))
  (lambda [e entities]
    (define ?value (xml-entity-value e))
    
    (cond [(not (xml:&string? ?value)) e]
          [else (let ([plain-value (xml-entity-replacement-text (xml:string-datum (xml-entity-value e)) entities #:GE-bypass? #true)])
                  (cond [(not plain-value) (make+exn:xml:unrecognized ?value (xml-entity-name e)) #false]
                        [else (struct-copy xml-entity e
                                           [value (xml-remake-token ?value xml:string plain-value)])]))])))

(define xml-dtd-expand-pentity : (-> XML:PEReference XML-Type-Entities (Listof XML-Type-Declaration*))
  (lambda [pe entities]
    (define plain-value : (Option String) (xml-pentity-value-ref pe entities #true))

    (cond [(not plain-value) (make+exn:xml:unrecognized pe) null]
          [else (let ([pe-body (read-xml-type-definition plain-value)])
                  (xml-dtd-declarations pe-body))])))

(define xml-dtd-expand-section : (-> (U XML:Name XML:PEReference) (Listof XML-Type-Declaration*) XML-Type-Entities (Listof XML-Type-Declaration*))
  (lambda [condition body entities]
    (define sec-name : (U False Symbol String)
      (cond [(xml:name? condition) (xml:name-datum condition)]
            [else (xml-pentity-value-ref condition entities #false)]))

    (cond [(or (eq? sec-name 'INCLUDE) (equal? sec-name "INCLUDE")) body]
          [(or (eq? sec-name 'IGNORE) (equal? sec-name "IGNORE")) null]
          [else (make+exn:xml:unrecognized condition) null])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-element-normalize : (-> XML-Element* XML-Type-Entities XML-Element*)
  (lambda [e entities]
    (list (car e)
          (filter-map (λ [[name=value : (Pairof XML:Name XML:String)]]
                        (xml-element-attribute-normalize name=value entities))
                      (cadr e))
          (xml-subelement-normalize (caddr e) entities))))

(define xml-element-attribute-normalize : (-> (Pairof XML:Name XML:String) XML-Type-Entities (Option (Pairof XML:Name XML:String)))
  (lambda [name=value entities]
    (let ([value (cdr name=value)])
      (cond [(not (xml:&string? value)) name=value]
            [else (let ([?value (xml-entity-replacement-text (xml:string-datum value) entities #:GE-bypass? #false #:PE-unrecognize? #true)])
                    (cond [(not ?value) (make+exn:xml:unrecognized value (car name=value)) #false]
                          [else (cons (car name=value)
                                      (xml-remake-token value xml:string ?value))]))]))))

(define xml-subelement-normalize : (-> (Listof (U XML-Subdatum* XML-Element*)) XML-Type-Entities (Listof (U XML-Subdatum* XML-Element*)))
  (lambda [children entities]
    (let normalize-subelement ([rest : (Listof (U XML-Subdatum* XML-Element*)) children]
                               [nerdlihc : (Listof (U XML-Subdatum* XML-Element*)) null])
      (cond [(null? rest) (reverse nerdlihc)]
            [else (let-values ([(self rest++) (values (car rest) (cdr rest))])
                    (cond [(list? self) (normalize-subelement rest++ (cons (xml-element-normalize self entities) nerdlihc))]
                          [(xml:reference? self)
                           (let ([content (xml-remake-token self xml:string (string (integer->char (xml:char-datum self))))])
                             (normalize-subelement rest++ (cons content nerdlihc)))]
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
           (let ([?xstr (xml-entity-value e)])
             (and ?xstr ?xstr))))))

(define xml-pentity-value-ref : (-> XML:PEReference XML-Type-Entities Boolean (Option String))
  (lambda [pe entities ge-bypass?]
    (define vtoken : (Option XML:String) (xml-entity-value-token-ref (xml:pereference-datum pe) entities))

    (cond [(xml:&string? vtoken) (xml-entity-replacement-text (xml:string-datum vtoken) entities #:GE-bypass? ge-bypass?)]
          [(xml:string? vtoken) (xml:string-datum vtoken)]
          [else #false])))

(define xml-entity-value-ref : (-> (U Symbol Keyword) XML-Type-Entities (Option String))
  (lambda [name entities]
    (case name
      [(lt) "\x26\x3c"]
      [(gt) "\x3e"]
      [(amp) "\x26\x26"]
      [(apos) "\x27"]
      [(quot) "\x22"]
      [else (let ([?xstr (xml-entity-value-token-ref name entities)])
              (and ?xstr
                   (xml:string-datum ?xstr)))])))
