#lang racket/base

(provide (all-defined-out))

(require racket/path)
(require racket/port)
(require racket/pretty)
(require racket/list)

(require syntax/strip-context)

(require sgml/digitama/digicore)
(require sgml/digitama/document)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define sgml-read
  (lambda [read-sgml-document /dev/xmlin]
    (read-sgml-document /dev/xmlin)))

(define sgml-read-syntax
  (lambda [read-sgml-document* sgml px.sgml .ext src /dev/sgmlin]
    (regexp-match #px"^\\s*" /dev/sgmlin) ; skip blanks before real sgml content
    (define-values (line column position) (port-next-location /dev/sgmlin))
    (define bytes-bag (port->bytes /dev/sgmlin))
    
    (define lang.sgml
      (cond [(path? src)
             (let* ([src.sgml (path-replace-extension (file-name-from-path src) "")]
                    [path.sgml (if (regexp-match? px.sgml src.sgml) src.sgml (path-replace-extension src.sgml .ext))])
               (string->symbol (path->string path.sgml)))]
            [else '|this should not happen| (string->symbol (format "lang~a" .ext))]))

    (strip-context
     #`(module #,lang.sgml typed/racket/base
         (provide (all-from-out #,sgml) #,lang.sgml)

         (require #,sgml)

         (define-values (#,lang.sgml MB cpu real gc)
           (let ([/dev/rawin (open-input-bytes #,bytes-bag '#,src)]
                 [mem0 (current-memory-use)])
             (port-count-lines! /dev/rawin)
             (set-port-next-location! /dev/rawin #,line #,column #,position)
             (define-values (&lang.sgml cpu real gc) (time-apply #,read-sgml-document* (list /dev/rawin)))
             (values (car &lang.sgml) (/ (- (current-memory-use) mem0) 1024.0 1024.0) cpu real gc)))

         (module+ main
           (require racket/format)
           
           #,lang.sgml
           (displayln (format "[~a]memory: ~aMB cpu time: ~a real time: ~a gc time: ~a"
                        '#,lang.sgml (~r MB #:precision '(= 3)) cpu real gc)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-read
  (lambda [[/dev/xmlin (current-input-port)]]
    (sgml-read read-xml-document /dev/xmlin)))

(define xml-read-syntax
  (lambda [[src #false] [/dev/xmlin (current-input-port)]]
    (sgml-read-syntax 'read-xml-document* 'sgml/xml #px"\\.xml$" ".xml" src /dev/xmlin)))

(define dtd-read
  (lambda [[/dev/xmlin (current-input-port)]]
    (sgml-read read-xml-document /dev/xmlin)))

(define dtd-read-syntax
  (lambda [[src #false] [/dev/dtdin (current-input-port)]]
    (sgml-read-syntax 'read-xml-type-definition 'sgml/dtd #px"\\.dtd$" ".dtd" src /dev/dtdin)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (xml-info in mod line col pos)
  (lambda [key default]
    (case key
      [(drracket:default-filters) '(["XML Sources" "*.xml"])]
      [(drracket:default-extension) "xml"]
      [(drracket:indentation) (dynamic-require 'sgml/village/sgmlang/indentation 'xml-indentation)]
      [(color-lexer) (dynamic-require 'sgml/village/sgmlang/highlight 'xml-lexer)]
      [else default])))

(define (dtd-info in mod line col pos)
  (lambda [key default]
    (case key
      [(drracket:default-filters) '(["DTD Sources" "*.dtd"])]
      [(drracket:default-extension) "dtd"]
      [(drracket:indentation) (dynamic-require 'sgml/village/sgmlang/indentation 'xml-indentation)]
      [(color-lexer) (dynamic-require 'sgml/village/sgmlang/highlight 'xml-lexer)]
      [else default])))
