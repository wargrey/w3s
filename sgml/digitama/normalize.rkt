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
    (define dtd : XML-Type (xml-dtd-expand int-dtd ext-dtds))
    
    (values dtd content)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-dtd-expand : (-> XML-DTD (Listof XML-DTD) XML-Type)
  (lambda [int-dtd ext-dtds]
    (let expand ([rest : (Listof XML-Type-Declaration*) (xml-dtd-declarations int-dtd)]
                 [entities : XML-Type-Entities (make-immutable-hasheq)])
      (cond [(null? rest) (xml-type entities)]
            [else (let-values ([(self rest++) (values (car rest) (cdr rest))])
                    (cond [(xml-entity? self)
                           (cond [(not (xml-entity-value self)) (expand rest++ (xml-entity-cons self entities))]
                                 [else (expand rest++ (xml-entity-cons (xml-dtd-expand-entity self entities) entities))])]
                          [else (expand rest++ entities)]))]))))

(define xml-dtd-expand-entity : (-> XML-Entity XML-Type-Entities (Option XML-Entity))
  (lambda [e entities]
    (define ?value (xml-entity-value e))
    
    (cond [(not (xml:&string? ?value)) e]
          [else (let ([plain-value (xml-entity-replacement-text (xml-entity-value e) entities #:GE-bypass? #true)])
                  (cond [(not plain-value) (make+exn:xml:unrecognized ?value (xml-entity-name e)) #false]
                        [else (struct-copy xml-entity e
                                           [value (xml-remake-token ?value xml:string plain-value)])]))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-entity-replacement-text : (-> XML:&String XML-Type-Entities #:GE-bypass? Boolean (Option String))
  ;;; https://www.w3.org/TR/xml11/#intern-replacement
  (let ([/dev/strout (open-output-string '/dev/entout)])
    (lambda [ev entities #:GE-bypass? ge-bypass?]
      (define src : String (xml:string-datum ev))
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
                                            (define-values (replacement bypassed?)
                                              (cond [(eq? leader #\%) (values (xml-entity-value-ref (string->keyword estr) entities) #false)]
                                                    [(eq? (car nchars) #\#) (values (xml-char-reference estr) #false)]
                                                    [(not ge-bypass?) (values (xml-entity-value-ref (string->unreadable-symbol estr) entities) #false)]
                                                    [else (values estr #true)]))

                                            (and (string? replacement)
                                                 (when (and bypassed?) (write-char #\& /dev/strout))
                                                 (write-string replacement /dev/strout)
                                                 (when (and bypassed?) (write-char #\; /dev/strout))
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

(define xml-entity-value-ref : (-> (U Symbol Keyword) XML-Type-Entities (Option String))
  (lambda [name entities]
    (case name
      [(lt) "\x26\x3c"]
      [(gt) "\x3e"]
      [(amp) "\x26\x26"]
      [(apos) "\x27"]
      [(quot) "\x22"]
      [else (let ([e (hash-ref entities name (λ [] #false))])
              (and e
                   (let ([?xstr (xml-entity-value e)])
                     (and ?xstr
                          (xml:string-datum ?xstr)))))])))
