#lang typed/racket/base

;;; https://www.w3.org/TR/xml/#sec-documents

(provide (all-defined-out))

(require racket/path)
(require racket/string)

(require "dtd.rkt")
(require "doctype.rkt")
(require "grammar.rkt")
(require "normalize.rkt")

(require "digicore.rkt")
(require "stdin.rkt")

(require "tokenizer/port.rkt")
(require "tokenizer/grammar.rkt")
(require "tokenizer.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct xml-document
  ([prolog : XML-Prolog]
   [doctype : XML-DocType]
   [internal-dtd : XML-Plain-DTD]
   [elements : (Listof XML-Content)])
  #:transparent
  #:type-name XML-Document)

(struct (T) xml-opaque
  ([unbox : T])
  #:type-name XML-Opaqueof)

(struct xml-document*
  ([prolog : XML-Prolog]
   [doctype : XML-DocType*]
   [internal-dtd : XML-DTD]
   [external-dtd : (XML-Opaqueof (Option XML-DTD))]
   [type : (XML-Opaqueof (Option XML-Type))]
   [contents : (Listof XML-Content*)])
  #:transparent
  #:type-name XML-Document*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define read-xml-document : (-> SGML-StdIn XML-Document)
  (lambda [/dev/rawin]
    (define-values (/dev/xmlin version encoding standalone?) (xml-open-input-port /dev/rawin #false))
    (define tokens : (Listof XML-Datum) (read-xml-tokens /dev/xmlin))
    (define-values (doctype dtd grammars) (xml-syntax->content tokens))
    (define-values (maybe-name external) (xml-doctype-values doctype))
    
    (define name : (Option Symbol)
      (or maybe-name
          (let ([maybe-first-element (findf list? grammars)])
            (and (pair? maybe-first-element)
                 (car maybe-first-element)))))

    (xml-document (xml-prolog (sgml-port-name /dev/xmlin) version encoding standalone?)
                  (xml-doctype name external)
                  dtd grammars)))

(define make-xml-document* : (-> (U String Symbol) (Option Nonnegative-Flonum) (Option String) Boolean
                                 (Option XML:Name) XML-External-ID* (Option XML-DTD) (Option XML-DTD) (Option XML-Type)
                                 (Listof XML-Content*)
                                 XML-Document*)
  (lambda [source version encoding standalone? maybe-name external intdtd xxedtd type contents]
    (define doc-type : XML-DocType*
      (xml-doctype* (or maybe-name
                        (let ([maybe-first-element (findf list? contents)])
                          (and (pair? maybe-first-element)
                               (car maybe-first-element))))
                    external))
    
    (xml-document* (xml-prolog source version encoding standalone?) doc-type
                   (or intdtd (xml-make-type-definition source null)) (xml-opaque xxedtd) (xml-opaque type)
                   contents)))

(define read-xml-document* : (->* (SGML-StdIn)
                                  ((U False XML-DTD Open-Input-XML-XXE) (Option Open-Input-XML-XXE)
                                   #:ipe-topsize (Option Index) #:xxe-topsize (Option Index) #:xxe-timeout (Option Real)
                                   #:normalize? Boolean #:xml:lang String #:xml:space Symbol #:xml:space-filter (Option XML:Space-Filter))
                                  XML-Document*)
  (lambda [#:normalize? [normalize? #false] #:xml:lang [xml:lang ""] #:xml:space [xml:space 'default] #:xml:space-filter [xml:space-filter #false]
           #:ipe-topsize [ipe-topsize (default-xml-ipe-topsize)] #:xxe-topsize [xxe-topsize (default-xml-xxe-topsize)] #:xxe-timeout [timeout (default-xml-xxe-timeout)]
           /dev/rawin [ext-dtd xml-load-relative-system-entity] [open-xxe-port xml-load-relative-system-entity]]
    (define-values (/dev/xmlin version encoding standalone?) (xml-open-input-port /dev/rawin #true))
    (define source : (U Symbol String) (sgml-port-name /dev/xmlin))
    (define tokens : (Listof XML-Token) (read-xml-tokens* /dev/xmlin source))
    (define-values (doctype definitions grammars) (xml-syntax->content* tokens))
    (define-values (maybe-name external) (xml-doctype-values* doctype))
    
    (define doc-type : XML-DocType*
      (xml-doctype* (or maybe-name
                        (let ([maybe-first-element (findf list? grammars)])
                          (and (pair? maybe-first-element)
                               (car maybe-first-element))))
                    external))

    (define ?external-dtd : (Option XML-DTD) (xml-load-external-dtd ext-dtd maybe-name external xxe-topsize timeout))

    (define doc : XML-Document*
      (make-xml-document* source version encoding standalone?
                          maybe-name external
                          (xml-make-type-definition source definitions) ?external-dtd #false
                          grammars))

    (cond [(not normalize?) doc]
          [else (xml-document*-normalize #:ipe-topsize ipe-topsize #:xxe-topsize xxe-topsize #:xxe-timeout timeout
                                         #:xml:lang xml:lang #:xml:space xml:space #:xml:space-filter xml:space-filter
                                         #:external-dtd ext-dtd
                                         doc open-xxe-port)])))

(define xml-document*-normalize : (->* (XML-Document*)
                                       ((Option Open-Input-XML-XXE) #:external-dtd (U False XML-DTD Open-Input-XML-XXE)
                                        #:ipe-topsize (Option Index) #:xxe-topsize (Option Index) #:xxe-timeout (Option Real)
                                        #:xml:lang String #:xml:space Symbol #:xml:space-filter (Option XML:Space-Filter))
                                       XML-Document*)
  (lambda [#:external-dtd [alter-ext-dtd #false] #:xml:lang [xml:lang ""] #:xml:space [xml:space 'default] #:xml:space-filter [xml:space-filter #false]
           #:ipe-topsize [ipe-topsize (default-xml-ipe-topsize)] #:xxe-topsize [xxe-topsize (default-xml-xxe-topsize)] #:xxe-timeout [timeout (default-xml-xxe-timeout)]
           doc [open-xxe-port xml-load-relative-system-entity]]
    (define-values (type contents)
      (xml-normalize (xml-document*-internal-dtd doc)
                     (or (let ([dt (xml-document*-doctype doc)])
                           (xml-load-external-dtd alter-ext-dtd (xml-doctype*-name dt) (xml-doctype*-external dt) xxe-topsize timeout))
                         (xml-opaque-unbox (xml-document*-external-dtd doc)))
                     (xml-document*-contents doc)
                     xml:lang xml:space xml:space-filter ipe-topsize
                     (or open-xxe-port (if (xml-dtd? alter-ext-dtd) #false alter-ext-dtd))
                     xxe-topsize timeout))
    
    (xml-document* (xml-document*-prolog doc)
                   (xml-document*-doctype doc)
                   (xml-document*-internal-dtd doc) (xml-document*-external-dtd doc)
                   (xml-opaque type) contents)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-load-relative-system-entity : Open-Input-XML-XXE
  (lambda [rootdir public system topsize &alt-source]
    (and (path? rootdir)
         (string? system)
         (parameterize ([current-directory rootdir])
           (and (let ([extdtd (simple-form-path system)])
                  (and (file-exists? extdtd)
                       (string-prefix? (path->string extdtd) (path->string rootdir))
                       (cons (dtd-open-input-port extdtd) #true))))))))

(define xml-document*->document : (-> XML-Document* XML-Document)
  (lambda [doc.xml]
    (xml-document (xml-document*-prolog doc.xml)
                  (let* ([doctype (xml-document*-doctype doc.xml)]
                         [name (xml-doctype*-name doctype)]
                         [id (xml-doctype*-external doctype)])
                    (xml-doctype (and name (xml:name-datum name))
                                 (xml-external-id->datum id)))
                  (make-hasheq)
                  (map xml-content->datum (xml-document*-contents doc.xml)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-content->datum : (-> XML-Content* XML-Content)
  (lambda [g]
    (cond [(list? g) (xml-element->datum g)]
          [else (xml-pi->datum g)])))

(define xml-pi->datum : (-> XML-Processing-Instruction* XML-Processing-Instruction)
  (lambda [p]
    (mcons (xml:name-datum (mcar p))
           (let ([body (mcdr p)])
             (and body
                  (xml:string-datum body))))))

(define xml-element->datum : (-> XML-Element* XML-Element)
  (lambda [e]
    (list (xml:name-datum (car e))
          (map xml-pair->datum (cadr e))
          (map (λ [[child : (U XML-Subdatum* XML-Element*)]]
                 (cond [(list? child) (xml-element->datum child)]
                       [(xml:string? child) (xml:string-datum child)]
                       [(xml-cdata-token? child)
                        (cond [(xml:newline? child) (xml-new-line (xml:whitespace-datum child))]
                              [(xml:comment? child) (xml-comment (xml:whitespace-datum child))]
                              [else (xml-white-space (or (xml-cdata-token->datum child) ""))])]
                       [(xml-reference-token? child)
                        (cond [(xml:reference? child) (xml:reference-datum child)]
                              [(xml:char? child) (xml:char-datum child)]
                              [else '|&DEADC0DE;|])]
                       [else (xml-pi->datum child)]))
               (caddr e)))))

(define xml-external-id->datum : (-> XML-External-ID* XML-External-ID)
  (lambda [id]
    (cond [(not id) id]
          [(xml:string? id) (xml:string-datum id)]
          [else (let ([public (car id)]
                      [system (cdr id)])
                  (cons (and public (xml:string-datum public))
                        (and system (xml:string-datum system))))])))

(define xml-pair->datum : (-> XML-Element-Attribute* XML-Element-Attribute)
  (lambda [p]
    (cons (xml:name-datum (car p))
          (let ([v (cdr p)])
            (cond [(xml:string? v) (xml:string-datum v)]
                  [(xml:name? v) (xml:name-datum v)]
                  [else (map xml:name-datum v)])))))

(define xml-load-external-dtd : (-> (U False XML-DTD Open-Input-XML-XXE) (Option XML:Name) XML-External-ID*
                                    (Option Index) (Option Real)
                                    (Option XML-DTD))
  (lambda [ext-dtd name external topsize timeout]
    (cond [(xml-dtd? ext-dtd) ext-dtd]
          [(not ext-dtd) #false]
          [(not external) #false]
          [(pair? external) (xml-load-external-entity name (car external) (cdr external) ext-dtd topsize timeout read-xml-type-definition)]
          [else (xml-load-external-entity name #false external ext-dtd topsize timeout read-xml-type-definition)])))
