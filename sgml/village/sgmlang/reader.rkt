#lang racket/base

(provide (all-defined-out))

(require racket/path)
(require racket/port)
(require racket/symbol)

(require syntax/strip-context)

(require sgml/digitama/document)
(require sgml/digitama/misc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define sgml-read
  (lambda [read-sgml-document /dev/xmlin]
    (read-sgml-document /dev/xmlin)))

(define sgml-doc-read-syntax
  (lambda [read-sgml-doc sgml px.sgml .ext src /dev/sgmlin [sgml-normalize #false]]
    (regexp-match #px"^\\s*" /dev/sgmlin) ; skip blanks before real css content

    ;;; large SGML files will cause expanding timeout,
    ;;; hence using bytes for faster reading inside the module at runtime
    
    (define-values (lang.sgml lang*.sgml) (sgml-lang-names src px.sgml .ext))
    (define-values (line column position) (port-next-location /dev/sgmlin))
    (define bytes-bag (port->bytes /dev/sgmlin))
    
    (strip-context
     #`(module #,lang.sgml typed/racket/base
         (provide (all-from-out #,sgml) #,lang.sgml)

         (require #,sgml)
         (require css/village/hashlang/w3s)

         (define-values (#,lang.sgml MB cpu real gc)
           (w3s-read-doc '#,src #,bytes-bag #,line #,column #,position
                         #,read-sgml-doc))

         (module+ main
           #,lang.sgml
           (w3s-display-times '#,lang.sgml MB cpu real gc))

         (w3s-doc-process #,sgml-normalize #,lang*.sgml MB* cpu* real* gc* #,lang.sgml)))))

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
         (path-replace-extension (symbol->immutable-string lang.sgml) #"")
         .ext)))

    (values lang.sgml lang*.sgml)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-read
  (lambda [[/dev/xmlin (current-input-port)]]
    (sgml-read read-xml-document /dev/xmlin)))

(define xml-read-syntax
  (lambda [[src #false] [/dev/xmlin (current-input-port)]]
    (parameterize ([xml-alternative-document-source (path->string src)])
      (sgml-doc-read-syntax 'read-xml-document* 'sgml/xml
                            #px"\\.t?xml$" ".xml" src /dev/xmlin
                            'xml-document*-normalize))))

(define dtd-read
  (lambda [[/dev/xmlin (current-input-port)]]
    (sgml-read read-xml-document /dev/xmlin)))

(define dtd-read-syntax
  (lambda [[src #false] [/dev/dtdin (current-input-port)]]
    (parameterize ([xml-alternative-document-source (path->string src)])
      (sgml-doc-read-syntax 'read-xml-type-definition 'sgml/dtd
                            #px"\\.t?dtd$" ".dtd" src /dev/dtdin
                            'xml-dtd-expand))))

(define rnc-read
  (lambda [[/dev/rncin (current-input-port)]]
    (sgml-read read-xml-document /dev/rncin)))

(define rnc-read-syntax
  (lambda [[src #false] [/dev/rncin (current-input-port)]]
    (parameterize ([xml-alternative-document-source (path->string src)])
      (sgml-doc-read-syntax 'read-rnc-grammar 'sgml/rnc
                            #px"\\.t?rnc$" ".rnc" src /dev/rncin))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (xml-info in mod line col pos)
  (lambda [key default]
    (case key
      [(drracket:default-filters) '(["XML Sources" "*.txml"])]
      [(drracket:default-extension) "txml"]
      ;[(drracket:indentation) (dynamic-require 'sgml/village/sgmlang/indentation 'xml-indentation)]
      [(color-lexer) (dynamic-require 'sgml/village/sgmlang/lexer 'xml-lexer)]
      [else default])))

(define (dtd-info in mod line col pos)
  (lambda [key default]
    (case key
      [(drracket:default-filters) '(["DTD Sources" "*.tdtd"])]
      [(drracket:default-extension) "tdtd"]
      ;[(drracket:indentation) (dynamic-require 'sgml/village/sgmlang/indentation 'dtd-indentation)]
      [(color-lexer) (dynamic-require 'sgml/village/sgmlang/lexer 'dtd-lexer)]
      [else default])))

(define (rnc-info in mod line col pos)
  (lambda [key default]
    (case key
      [(drracket:default-filters) '(["RNC Sources" "*.trnc"])]
      [(drracket:default-extension) "trnc"]
      ;[(drracket:indentation) (dynamic-require 'sgml/village/sgmlang/indentation 'rnc-indentation)]
      [(color-lexer) (dynamic-require 'sgml/village/sgmlang/lexer 'rnc-lexer)]
      [else default])))
