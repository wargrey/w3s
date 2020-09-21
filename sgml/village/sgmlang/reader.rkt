#lang racket/base

(provide (all-defined-out))

(require racket/path)
(require racket/port)

(require syntax/strip-context)

(require sgml/digitama/document)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define sgml-read
  (lambda [read-sgml-document /dev/xmlin]
    (read-sgml-document /dev/xmlin)))

(define sgml-read-syntax
  (lambda [read-sgml-document* sgml px.sgml .ext src /dev/sgmlin [sgml-normalize #false]]
    (regexp-match #px"^\\s*" /dev/sgmlin) ; skip blanks before real sgml content
    
    (define-values (line column position) (port-next-location /dev/sgmlin))
    (define bytes-bag (port->bytes /dev/sgmlin))
    
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

    (strip-context
     #`(module #,lang.sgml typed/racket/base
         (provide (all-from-out #,sgml) #,lang.sgml)

         (require #,sgml)
         (require css/village/hashlang/w3s)

         (define-values (#,lang.sgml MB cpu real gc)
           (w3s-read-doc '#,src #,bytes-bag #,line #,column #,position
                         #,read-sgml-document*)) 

         (module+ main
           #,lang.sgml
           (w3s-display-times '#,lang.sgml MB cpu real gc))

         (w3s-doc-process #,sgml-normalize #,lang*.sgml
                          MB* cpu* real* gc*
                          #,lang.sgml)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-read
  (lambda [[/dev/xmlin (current-input-port)]]
    (sgml-read read-xml-document /dev/xmlin)))

(define xml-read-syntax
  (lambda [[src #false] [/dev/xmlin (current-input-port)]]
    (sgml-read-syntax 'read-xml-document* 'sgml/xml #px"\\.xml$" ".xml" src /dev/xmlin
                      'xml-document*-normalize)))

(define dtd-read
  (lambda [[/dev/xmlin (current-input-port)]]
    (sgml-read read-xml-document /dev/xmlin)))

(define dtd-read-syntax
  (lambda [[src #false] [/dev/dtdin (current-input-port)]]
    (sgml-read-syntax 'read-xml-type-definition 'sgml/dtd #px"\\.dtd$" ".dtd" src /dev/dtdin
                      'xml-dtd-expand)))

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
