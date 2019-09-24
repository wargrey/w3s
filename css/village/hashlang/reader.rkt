#lang racket/base

(provide (all-defined-out))

(require racket/path)
(require racket/port)
(require racket/pretty)
(require racket/list)

(require syntax/strip-context)

(require css/digitama/syntax/digicore)
(require css/digitama/syntax/grammar)

(define css-read
  (lambda [[/dev/cssin (current-input-port)]]
    (regexp-match #px"^\\s*" /dev/cssin) ; skip blanks between `#lang` and contents
    (read-css-stylesheet /dev/cssin)))
  
(define css-read-syntax
  (lambda [[src #false] [/dev/cssin (current-input-port)]]
    (regexp-match #px"^\\s*" /dev/cssin) ; skip blanks before real css content
    (define-values (line column position) (port-next-location /dev/cssin))
    (define bytes-bag (port->bytes /dev/cssin))
    (define all-rules (read-css-stylesheet bytes-bag))
    (define all-namespaces (css-stylesheet-namespaces all-rules))
    (define lang.css
      (cond [(and (pair? all-namespaces) (not (eq? (caar all-namespaces) '||)))
             (string->symbol (string-append (symbol->string (caar all-namespaces)) ".css"))]
            [(path? src)
             (define src.css (path-replace-extension (file-name-from-path src) ""))
             (define path.css (if (regexp-match? #px"\\.css$" src.css) src.css (path-replace-extension src.css ".css")))
             (string->symbol (path->string path.css))]
            [else '|this should not happen| 'lang.css]))
    (strip-context
     #`(module #,lang.css typed/racket/base
         (provide #,lang.css)
         (provide (all-from-out css/syntax))

         (require css/syntax)

         ;;; NOTE
         ; Prefab structures can be handled at compile time, however reading the stylesheet is reasonably efficient,
         ; therefore do not waste time in struggling to optimize the reading process.
         (define-values (#,lang.css MB cpu real gc)
           (let ([/dev/rawin (open-input-bytes #,bytes-bag '#,src)]
                 [mem0 (current-memory-use)])
             (port-count-lines! /dev/rawin)
             (set-port-next-location! /dev/rawin #,line #,column #,position)
             (define-values (&lang.css cpu real gc) (time-apply read-css-stylesheet (list /dev/rawin)))
             (values (car &lang.css) (/ (- (current-memory-use) mem0) 1024.0 1024.0) cpu real gc)))

         (module+ main
           (require racket/pretty)
           (require racket/format)
           
           (pretty-print-columns 160)

           (define benchmark : String
             (format "[~a]memory: ~aMB cpu time: ~a real time: ~a gc time: ~a"
                     '#,lang.css (~r MB #:precision '(= 3)) cpu real gc))
           
           (define drracket? : Boolean (regexp-match? #px"DrRacket$" (find-system-path 'run-file)))
           (if drracket? #,lang.css (printf "~a~n~a~n" (pretty-format #,lang.css) benchmark))
           (when drracket? (displayln benchmark)))))))

(define (css-info in mod line col pos)
  (lambda [key default]
    (case key
      [(drracket:default-filters) '(["CSS Sources" "*.css"])]
      [(drracket:default-extension) "css"]
      [(drracket:indentation) (dynamic-require 'css/village/hashlang/indentation 'css-indentation)]
      [(color-lexer) (dynamic-require 'css/village/hashlang/highlight 'css-lexer)]
      [else default])))
