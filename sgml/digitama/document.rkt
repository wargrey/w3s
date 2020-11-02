#lang typed/racket/base

;;; https://www.w3.org/TR/xml/#sec-documents

(provide (all-defined-out))

(require racket/path)
(require racket/string)

(require "dtd.rkt")
(require "schema.rkt")
(require "validity.rkt")
(require "doctype.rkt")
(require "grammar.rkt")
(require "normalize.rkt")

(require "plain/grammar.rkt")

(require "digicore.rkt")
(require "stdin.rkt")

(require "tokenizer/port.rkt")
(require "tokenizer.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type XML-External-Doctype-Entity (U False XML-DTD))
(define-type XML-External-Schema (U False Open-Input-XML-XXE XML-Schema))

(struct xml-document
  ([prolog : XML-Prolog]
   [doctype : XML-DocType]
   [elements : (Listof XML-Content)])
  #:transparent
  #:type-name XML-Document)

(struct xml-document*
  ([prolog : XML-Prolog]
   [doctype : XML-DocType*]
   [internal-dtd : XML-DTD]
   [contents : (Listof XML-Content*)])
  #:transparent
  #:type-name XML-Document*)

(struct xml-document+schema xml-document*
  ([external-entity : XML-External-Doctype-Entity]
   [body : XML-Schema])
  #:type-name XML-Document+Schema)

(define xml-alternative-document-source : (Parameterof (U String Symbol False)) (make-parameter #false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define read-xml-document : (-> SGML-StdIn XML-Document)
  (lambda [/dev/rawin]
    (define-values (/dev/xmlin version encoding standalone?) (xml-open-input-port /dev/rawin #false))
    (define tokens : (Listof XML-Datum) (read-xml-tokens /dev/xmlin))
    (define-values (doctype grammars) (xml-syntax->content tokens))
    (define-values (maybe-name external) (xml-doctype-values doctype))
    
    (define name : (Option Symbol)
      (or maybe-name
          (let ([maybe-first-element (findf list? grammars)])
            (and (pair? maybe-first-element)
                 (car maybe-first-element)))))

    (xml-document (xml-prolog (sgml-port-name /dev/xmlin) version encoding standalone?)
                  (xml-doctype name external)
                  grammars)))

(define read-xml-document* : (-> SGML-StdIn XML-Document*)
  (lambda [/dev/rawin]
    (define-values (/dev/xmlin version encoding standalone?) (xml-open-input-port /dev/rawin #true))
    (define source : (U Symbol String) (sgml-port-name /dev/xmlin))
    (define tokens : (Listof XML-Token) (read-xml-tokens* /dev/xmlin (or (xml-alternative-document-source) source)))
    (define-values (doctype definitions grammars) (xml-syntax->content* tokens))
    (define-values (maybe-name external) (xml-doctype-values* doctype))
    
    (define doc-type : XML-DocType*
      (xml-doctype* (or maybe-name
                        (let ([maybe-first-element (findf list? grammars)])
                          (and (pair? maybe-first-element)
                               (car maybe-first-element))))
                    external))

    (xml-document* (xml-prolog source version encoding standalone?) doc-type
                   (xml-make-type-definition source definitions)
                   grammars)))

(define xml-document*-normalize : (->* (XML-Document*)
                                       (XML-External-Schema
                                        #:guard XML-DTD-Guard #:ignore-internal-dtd? Boolean
                                        #:xml:lang String #:xml:space Symbol #:xml:space-filter (Option XML:Space-Filter))
                                       XML-Document*)
  (lambda [doc [sch xml-load-relative-system-entity]
               #:guard [dtdg default-dtd-guard] #:ignore-internal-dtd? [ignore-intsubset? #false]
               #:xml:lang [xml:lang ""] #:xml:space [xml:space 'default] #:xml:space-filter [xml:space-filter #false]]
    (define standalone? : Boolean (xml-prolog-standalone? (xml-document*-prolog doc)))
    (define stop-if-xxe-not-loaded? : Boolean (not standalone?))
    
    (define external-dtd : (Option XML-DTD)
      (let ([dt (xml-document*-doctype doc)])
        (and (not (xml-schema? sch))
             (xml-load-external-dtd sch (xml-doctype*-name dt) (xml-doctype*-external dt)
                                    (xml-dtd-guard-xxe-guard dtdg)))))
    
    (define-values (schema contents)
      (if (xml-schema? sch)
          (xml-normalize/schema sch (xml-document*-contents doc)
                                xml:lang xml:space xml:space-filter
                                stop-if-xxe-not-loaded? dtdg)
          (let ([int-decls (if (not ignore-intsubset?) (xml-dtd-declarations (xml-document*-internal-dtd doc)) null)]
                [ext-dtd (and (not standalone?) external-dtd)])
            (xml-normalize int-decls ext-dtd (xml-document*-contents doc)
                           xml:lang xml:space xml:space-filter
                           stop-if-xxe-not-loaded? dtdg))))
    
    (xml-document+schema (xml-document*-prolog doc) (xml-document*-doctype doc) (xml-document*-internal-dtd doc)
                         contents external-dtd schema)))

(define xml-document*-valid? : (->* (XML-Document*)
                                    ((U False XML-Schema)
                                     #:external-schema XML-External-Schema #:guard XML-DTD-Guard #:ignore-internal-dtd? Boolean
                                     #:xml:lang String #:xml:space Symbol #:xml:space-filter (Option XML:Space-Filter))
                                    Boolean)
  (lambda [doc [sch #false]
               #:external-schema [extsch xml-load-relative-system-entity] #:guard [dtdg default-dtd-guard] #:ignore-internal-dtd? [ignore-intsubset? #false]
               #:xml:lang [xml:lang ""] #:xml:space [xml:space 'default] #:xml:space-filter [xml:space-filter #false]]
    (define standalone? : Boolean (xml-prolog-standalone? (xml-document*-prolog doc)))
    (define schema : XML-Schema
      (cond [(xml-schema? sch) sch]
            [(xml-document+schema? doc) (xml-document+schema-body doc)]
            [else (let ([ndoc (xml-document*-normalize doc extsch #:guard dtdg #:xml:lang xml:lang #:xml:space xml:space #:xml:space-filter xml:space-filter)])
                    (xml-document+schema-body (assert ndoc xml-document+schema?)))]))

    (xml-validate schema (xml-document*-contents doc) standalone?
                  (xml-dtd-guard-ipe-topsize dtdg))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-xml-dtd-guard : (->* () (XML-DTD-Guard #:open-input-xxe (U Open-Input-XML-XXE False Void)
                                                    #:ipe-topsize (U Index False Void) #:xxe-topsize (U Index False Void) #:xxe-timeout (U Real False Void))
                                  XML-DTD-Guard)
  (lambda [[src default-dtd-guard]
           #:open-input-xxe [open-inxxe (void)] #:ipe-topsize [ipe-topsize (void)] #:xxe-topsize [xxe-topsize (void)] #:xxe-timeout [timeout (void)]]
    (define xxeg : XML-XXE-Guard (xml-dtd-guard-xxe-guard src))
    
    (xml-dtd-guard (if (void? ipe-topsize) (xml-dtd-guard-ipe-topsize src) ipe-topsize)
                   (xml-xxe-guard (if (void? open-inxxe) (xml-xxe-guard-open-input-port xxeg) open-inxxe)
                                  (if (void? xxe-topsize) (xml-xxe-guard-topsize xxeg) xxe-topsize)
                                  (if (void? timeout) (xml-xxe-guard-timeout xxeg) timeout)))))

(define xml-document*->document : (-> XML-Document* XML-Document)
  (lambda [doc.xml]
    (xml-document (xml-document*-prolog doc.xml)
                  (let* ([doctype (xml-document*-doctype doc.xml)]
                         [name (xml-doctype*-name doctype)]
                         [id (xml-doctype*-external doctype)])
                    (xml-doctype (and name (xml:name-datum name))
                                 (xml-external-id->datum id)))
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

(define xml-load-external-dtd : (-> (Option Open-Input-XML-XXE) (Option XML:Name) XML-External-ID* XML-XXE-Guard (Option XML-DTD))
  (lambda [ext-dtd name external xxeg]
    (define topsize (xml-xxe-guard-topsize xxeg))
    (define timeout (xml-xxe-guard-timeout xxeg))
    
    (cond [(not ext-dtd) #false]
          [(not external) #false]
          [(pair? external) (xml-load-external-entity name (car external) (cdr external) ext-dtd topsize timeout read-xml-type-definition)]
          [else (xml-load-external-entity name #false external ext-dtd topsize timeout read-xml-type-definition)])))
