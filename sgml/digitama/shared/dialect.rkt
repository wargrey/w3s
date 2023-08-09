#lang typed/racket/base

(provide (all-defined-out))

(require racket/list)
(require racket/path)
(require racket/symbol)

(require digimon/syntax)
(require digimon/digitama/stdio)

(require "../tokenizer/characters.rkt")
(require "datatype.rkt")

(require (for-syntax syntax/parse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type XML-Source (U String Symbol (Pairof (U String Symbol) (Pairof Positive-Integer Natural))))
(define-type XML-Namespaces (Listof (Pairof Symbol String)))

(define-syntax (dom-attribute-list stx)
  (syntax-case stx [:]
    [(_ [field value #:~> datum->value] ...)
     (syntax/loc stx
       (for/list : (Listof (Pairof Symbol String))
         ([fname (in-list ((inst list Symbol) 'field ...))]
          [datum (in-list ((inst list (Option String)) (and value (datum->value value)) ...))]
          #:when datum)
         (cons fname datum)))]
    [(_ header [attr #:=> attr->xml] ...)
     (syntax/loc stx
       (for/fold ([attrs : (Listof (Pairof Symbol String)) header])
                 ([alist (in-list ((inst list (Option (Listof (Pairof Symbol String)))) (and attr (attr->xml attr)) ...))]
                  #:when alist)
         (append attrs alist)))]))

(define-syntax (define-xml-attribute/extract stx)
  (syntax-parse stx #:literals [:]
    [(_ attr : Attr #:-> super #:with define-xml-attribute-extract extract-attr attr->xexpr
        ([field : FieldType
                (~optional (~seq #:= defval ...) #:defaults ([(defval 1) null]))
                (~seq #:<-> xml->datum (~optional datum->xml #:defaults ([datum->xml #'xml:attr-datum->value])))] ...)
        (~optional (~seq #:report report-unknown report-range-exn)
                   #:defaults ([report-unknown #'#false] [report-range-exn #'#false]))
        options ...)
     (with-syntax* ([make-attr (format-id #'attr "make-~a" (syntax-e #'attr))]
                    [remake-attr (format-id #'attr "remake-~a" (syntax-e #'attr))]
                    [cascade-attr (format-id #'attr "cascade-~a" (syntax-e #'attr))]
                    [(field-ref ...) (make-identifiers #'attr #'(field ...))]
                    [([kw-args ...] [kw-reargs ...]) (make-keyword-arguments #'(field ...) #'(FieldType ...) #'([defval ...] ...))]
                    [mandatory-fields (for/list ([<field> (syntax->list #'(field ...))]
                                                 [<defvals> (syntax->list #'([defval ...] ...))]
                                                 #:when (null? (syntax-e <defvals>)))
                                        <field>)]

                    ;;; TODO: find a better strategy
                    ; #:inline is definitely bad for large structs, whereas
                    ; #:vector and #:hash are almost identical for large structs. 
                    [switch (let ([count (length (syntax->list #'(field ...)))]) (cond [(<= count 10) #'#:inline] [(<= count 20) #'#:vector] [else #'#:hash]))]
                    [AltReturnType (if (null? (syntax-e #'mandatory-fields)) #'False #'Attr)])
       (syntax/loc stx
         (begin (struct attr super ([field : FieldType] ...)
                  #:type-name Attr #:transparent
                  #:property prop:custom-write
                  (λ [[self : Attr] [/dev/stdout : Output-Port] [mode : (U Zero One Boolean)]]
                    (xml-attributes-custom-write 'attr /dev/stdout mode (list 'field ...) (list (field-ref self) ...)))
                  options ...)

                (define (make-attr kw-args ...) : Attr
                  (attr field ...))

                (define (remake-attr [self : Attr] kw-reargs ...) : Attr
                  (attr (if (void? field) (field-ref self) field) ...))

                (define-xml-attribute-extract extract-attr : Attr switch #:with report-unknown report-range-exn
                  (attr [field FieldType xml->datum defval ...] ...) 'mandatory-fields AltReturnType)

                (define (attr->xexpr [self : Attr]) : (Listof (Pairof Symbol String))
                  (dom-attribute-list [field (field-ref self) #:~> datum->xml] ...))

                (define (cascade-attr [parent : Attr] [child : Attr]) : Attr
                  (attr (or (field-ref child) (field-ref parent)) ... )))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-source->id-value : (-> XML-Source String)
  (lambda [src]
    (cond [(pair? src) (string-append (xml-source->id-value (car src)) ":" (number->string (cadr src)) ":" (number->string (cddr src)))]
          [(symbol? src) (xml-source->id-value (symbol->immutable-string src))]
          [else (or (and (path-string? src)
                         (let ([pathname (file-name-from-path src)])
                           (and pathname
                                (let ([name (path->string pathname)])
                                  (cond [(xml-name? name) name]
                                        [else (xml-name-fix name)])))))
                    (symbol->immutable-string (gensym 'id)))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-element-custom-write : (-> Symbol Output-Port (U Zero One Boolean) (Pairof Symbol (Listof Symbol)) (Pairof Any (Listof Any)) (Listof Any) Void)
  (lambda [id /dev/stdout mode fields all-data body-data]
    (define write-datum : (-> Any Output-Port Void) (stdio-select-writer mode))
    (define-values (line column pos) (port-next-location /dev/stdout))
    (define indent : String (list->string (cons #\newline (if (not column) null (make-list (+ column 2) #\space)))))
    (define-values (srtta atad)
      (for/fold ([attrs : (Listof Any) null]
                 [data : (Listof (Pairof Symbol Any)) null])
                ([datum (in-list (cdr all-data))]
                 [fname (in-list (cdr fields))]
                 #:unless (or (not datum) (null? datum)))
        (cond [(struct? datum) (values (cons datum attrs) data)]
              [else (values attrs (cons (cons fname datum) data))])))
      
    (display #\( /dev/stdout)
    (display id /dev/stdout)
    (display #\space /dev/stdout)
    (write-datum (car all-data) /dev/stdout)

    (when (pair? srtta)
      (for ([attr (in-list (reverse srtta))])
        (display indent /dev/stdout)
        (write-datum attr /dev/stdout)))

    (display indent /dev/stdout)
    (when (pair? atad)
      (write-datum (reverse atad) /dev/stdout)
      (display indent /dev/stdout))
    
    (write-datum body-data /dev/stdout)
    (write-char #\) /dev/stdout)
    (write-string indent /dev/stdout 0 (or column 1))
    (flush-output /dev/stdout)))

(define xml-attributes-custom-write : (-> Symbol Output-Port (U Zero One Boolean) (Listof Symbol) (Listof Any) Void)
  (lambda [id /dev/stdout mode fields all-data]
    (define write-datum : (-> Any Output-Port Void) (stdio-select-writer mode))
    (define data : (Listof (Pairof Symbol Any))
      (for/list ([datum (in-list all-data)]
                 [fname (in-list fields)]
                 #:unless (or (not datum) (null? datum)))
        (cons fname datum)))
    
    (display (if (eq? mode 0) "(" "#<") /dev/stdout)
    (display id /dev/stdout)
    (when (pair? data)
      (display #\space /dev/stdout)
      (write-datum data /dev/stdout))
    (write-char (if (eq? mode 0) #\) #\>) /dev/stdout)
    (flush-output /dev/stdout)))
