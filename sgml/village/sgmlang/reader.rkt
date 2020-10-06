#lang racket/base

(provide (all-defined-out))

(require racket/path)
(require racket/port)

(require syntax/strip-context)

(require sgml/digitama/dtd)
(require sgml/digitama/validity)
(require sgml/digitama/document)
(require sgml/digitama/normalize)

(require css/village/hashlang/w3s)

(require "expander.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define sgml-read
  (lambda [read-sgml-document /dev/xmlin]
    (read-sgml-document /dev/xmlin)))

(define sgml-doc-read-syntax
  (lambda [read-sgml-document DocType sgml px.sgml .ext src /dev/sgmlin [sgml-normalize #false]]
    (define-values (lang.sgml lang*.sgml) (sgml-lang-names src px.sgml .ext))

    (define-values (doc.sgml MB cpu real gc) (w3s-read-doc /dev/sgmlin read-sgml-document))
    (define-values (location version encoding standalone? root-name external-id sexp) (xml-document->sexp doc.sgml))
    (define-values (internal-dtd external-dtd type) (xml-dtd+type doc.sgml))

    (define-values (doc.sgml* MB* cpu* real* gc*)
      (cond [(not sgml-normalize) (values #false MB cpu real gc)]
            [else (w3s-read-doc doc.sgml sgml-normalize)]))

    (define-values (location* version* encoding* standalone?* root-name* external-id* sexp*)
      (cond [(not doc.sgml*) (values location version encoding standalone? root-name external-id sexp)]
            [else (xml-document->sexp doc.sgml*)]))

    (define-values (internal-dtd* external-dtd* type*)
      (cond [(not doc.sgml*) (values internal-dtd external-dtd type)]
            [else (xml-dtd+type doc.sgml*)]))

    (strip-context
     #`(module #,lang.sgml typed/racket/base
         (provide (all-from-out #,sgml) #,lang.sgml)

         (require #,sgml)
         (require css/village/hashlang/w3s)
         (require sgml/village/sgmlang/expander)
         
         (define #,lang.sgml : #,DocType
           (xml-syntax->document '#,location #,version #,encoding #,standalone? #,root-name #,external-id
                                 #,internal-dtd #,external-dtd #,type #,sexp))
         
         (module+ main
           #,lang.sgml
           (w3s-display-times '#,lang.sgml #,MB #,cpu #,real #,gc))

         (w3s-doc-process #:main+ #,(and doc.sgml* lang*.sgml) #,MB* #,cpu* #,real* #,gc*
                          #:body (define #,lang*.sgml : #,DocType
                                   (xml-syntax->document '#,location* #,version* #,encoding* #,standalone?* #,root-name* #,external-id*
                                                         #,internal-dtd* #,external-dtd* #,type* #,sexp*)))))))

(define sgml-type-read-syntax
  (lambda [read-sgml-type Type Expanded-Type sgml px.sgml .ext src /dev/sgmlin [sgml-expand #false]]
    (define-values (lang.sgml lang*.sgml) (sgml-lang-names src px.sgml .ext))

    (define-values (type.sgml MB cpu real gc) (w3s-read-doc /dev/sgmlin read-sgml-type))
    (define sexp (xml-dtd->sexp type.sgml))

    (define-values (doc.sgml* MB* cpu* real* gc*)
      (cond [(not sgml-expand) (values #false MB cpu real gc)]
            [else (w3s-read-doc type.sgml sgml-expand)]))

    (define sexp*
      (cond [(not doc.sgml*) sexp]
            [else (xml-type->sexp doc.sgml*)]))
    
    (strip-context
     #`(module #,lang.sgml typed/racket/base
         (provide (all-from-out #,sgml) #,lang.sgml)

         (require #,sgml)
         (require css/village/hashlang/w3s)
         (require sgml/village/sgmlang/expander)
         
         (define #,lang.sgml : #,Type (xml-syntax->dtd #:subset #,sexp))

         (module+ main
           #,lang.sgml
           (w3s-display-times '#,lang.sgml #,MB #,cpu #,real #,gc))

         (w3s-doc-process #:main+ #,(and doc.sgml* lang*.sgml) #,MB* #,cpu* #,real* #,gc*
                          #:body (define #,lang*.sgml : #,Expanded-Type (xml-syntax->type #,sexp*)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define sgml-lang-names
  (lambda [src px.sgml .ext]
    (define lang.sgml
      (cond [(path? src)
             (let* ([src.sgml (path-replace-extension (file-name-from-path src) #"")]
                    [path.sgml (if (regexp-match? px.sgml src.sgml) src.sgml (path-replace-extension src.sgml .ext))])
               (string->symbol (path->string path.sgml)))]
            [else '|this should not happen| (string->symbol (format "lang~a" .ext))]))

    (define lang*.sgml
      (string->symbol
       (format "~a*~a"
         (path-replace-extension (symbol->string lang.sgml) #"")
         .ext)))

    (values lang.sgml lang*.sgml)))

(define xml-dtd+type
  (lambda [doc.xml]
    (define ?ext (xml-opaque-unbox (xml-document*-external-dtd doc.xml)))
    (define ?type (xml-opaque-unbox (xml-document*-type doc.xml)))
    
    (values (xml-dtd->sexp (xml-document*-internal-dtd doc.xml))
            (and ?ext (xml-dtd->sexp ?ext))
            (and ?type (xml-type->sexp ?type)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-read
  (lambda [[/dev/xmlin (current-input-port)]]
    (sgml-read read-xml-document /dev/xmlin)))

(define xml-read-syntax
  (lambda [[src #false] [/dev/xmlin (current-input-port)]]
    (parameterize ([xml-alternative-document-source (path->string src)])
      (sgml-doc-read-syntax read-xml-document* 'XML-Document*
                            'sgml/xml #px"\\.xml$" ".xml" src /dev/xmlin
                            xml-document*-normalize))))

(define dtd-read
  (lambda [[/dev/xmlin (current-input-port)]]
    (sgml-read read-xml-document /dev/xmlin)))

(define dtd-read-syntax
  (lambda [[src #false] [/dev/dtdin (current-input-port)]]
    (parameterize ([xml-alternative-document-source (path->string src)])
      (sgml-type-read-syntax read-xml-type-definition 'XML-DTD 'XML-Type
                             'sgml/dtd #px"\\.dtd$" ".dtd" src /dev/dtdin
                             xml-dtd-expand))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (xml-info in mod line col pos)
  (lambda [key default]
    (case key
      [(drracket:default-filters) '(["XML Sources" "*.xml"])]
      [(drracket:default-extension) "xml"]
      [(drracket:indentation) (dynamic-require 'sgml/village/sgmlang/indentation 'xml-indentation)]
      [(color-lexer) (dynamic-require 'sgml/village/sgmlang/lexer 'xml-lexer)]
      [else default])))

(define (dtd-info in mod line col pos)
  (lambda [key default]
    (case key
      [(drracket:default-filters) '(["DTD Sources" "*.dtd"])]
      [(drracket:default-extension) "dtd"]
      [(drracket:indentation) (dynamic-require 'sgml/village/sgmlang/indentation 'xml-indentation)]
      [(color-lexer) (dynamic-require 'sgml/village/sgmlang/lexer 'dtd-lexer)]
      [else default])))
