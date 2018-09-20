#lang racket/base

(provide (all-defined-out))

(require racket/path)
(require racket/port)
(require racket/pretty)
(require racket/list)

(require syntax/strip-context)

(require sgml/digitama/digicore)
(require sgml/digitama/document)

(define xml-read
  (lambda [[/dev/xmlin (current-input-port)]]
    (regexp-match #px"^\\s*" /dev/xmlin) ; skip blanks between `#lang` and contents
    (read-xml-document /dev/xmlin)))

(define xml-read-syntax
  (lambda [[src #false] [/dev/xmlin (current-input-port)]]
    (regexp-match #px"^\\s*" /dev/xmlin) ; skip blanks before real xml content
    (define-values (line column position) (port-next-location /dev/xmlin))
    (define bytes-bag (port->bytes /dev/xmlin))
    (define all-rules (read-xml-document bytes-bag))
    (define all-namespaces (XML-Document-namespaces all-rules))
    (define lang.xml
      (cond [(and (pair? all-namespaces) (not (eq? (caar all-namespaces) '||)))
             (string->symbol (string-append (symbol->string (caar all-namespaces)) ".xml"))]
            [(path? src)
             (define src.xml (path-replace-extension (file-name-from-path src) ""))
             (define path.xml (if (regexp-match? #px"\\.xml$" src.xml) src.xml (path-replace-extension src.xml ".xml")))
             (string->symbol (path->string path.xml))]
            [else '|this should not happen| 'lang.xml]))
    (strip-context
     #`(module #,lang.xml typed/racket/base
         (provide #,lang.xml)
         (provide (all-from-out sgml/xml))

         (require sgml/xml)

         ;;; NOTE
         ; Prefab structures can be handled at compile time, however reading the stylesheet is reasonably efficient,
         ; therefore do not waste time in struggling to optimize the reading process.
         (define-values (#,lang.xml MB cpu real gc)
           (let ([/dev/rawin (open-input-bytes #,bytes-bag '#,src)]
                 [mem0 (current-memory-use)])
             (port-count-lines! /dev/rawin)
             (set-port-next-location! /dev/rawin #,line #,column #,position)
             (define-values (&lang.xml cpu real gc) (time-apply read-xml-document (list /dev/rawin)))
             (values (car &lang.xml) (/ (- (current-memory-use) mem0) 1024.0 1024.0) cpu real gc)))

         (module+ main
           (require racket/pretty)
           (require racket/format)
           
           (pretty-print-columns 160)

           (define benchmark : String
             (format "[~a]memory: ~aMB cpu time: ~a real time: ~a gc time: ~a"
                     '#,lang.xml (~r MB #:precision '(= 3)) cpu real gc))
           
           (define drracket? : Boolean (regexp-match? #px"DrRacket$" (find-system-path 'run-file)))
           (if drracket? #,lang.xml (printf "~a~n~a~n" (pretty-format #,lang.xml) benchmark))
           (when drracket? (displayln benchmark)))))))

(define (xml-info in mod line col pos)
  (lambda [key default]
    (case key
      [(drracket:default-filters) '(["XML Sources" "*.xml"])]
      [(drracket:default-extension) "xml"]
      [(drracket:indentation) (dynamic-require 'sgml/village/xmllang/indentation 'xml-indentation)]
      [(color-lexer) (dynamic-require 'sgml/village/xmllang/highlight 'xml-lexer)]
      [else default])))
