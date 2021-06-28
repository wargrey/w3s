#lang racket/base

(provide (all-defined-out))

(require racket/path)
(require racket/port)
(require racket/symbol)

(require syntax/strip-context)

(require css/digitama/syntax/digicore)
(require css/digitama/syntax/grammar)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
             (string->symbol (string-append (symbol->immutable-string (caar all-namespaces)) ".css"))]
            [(path? src)
             (define src.css (path-replace-extension (file-name-from-path src) ""))
             (define path.css (if (regexp-match? #px"\\.css$" src.css) src.css (path-replace-extension src.css ".css")))
             (string->symbol (path->string path.css))]
            [else '|this should not happen| 'lang.css]))
    
    (strip-context
     #`(module #,lang.css typed/racket/base
         (provide (all-from-out css/syntax))
         (provide #,lang.css)
         
         (require css/syntax)
         (require css/village/hashlang/w3s)

         ;;; NOTE
         ; Prefab structures can be handled at compile time, however reading the stylesheet is reasonably efficient,
         ; therefore do not waste time in struggling to optimize the reading process.

         (define-values (#,lang.css MB cpu real gc)
           (w3s-read-doc '#,src #,bytes-bag #,line #,column #,position
                         read-css-stylesheet))

         (module+ main
           #,lang.css
           (w3s-display-times '#,lang.css MB cpu real gc))))))

(define (css-info in mod line col pos)
  (lambda [key default]
    (case key
      [(drracket:default-filters) '(["CSS Sources" "*.css"])]
      [(drracket:default-extension) "css"]
      [(drracket:indentation) (dynamic-require 'css/village/hashlang/indentation 'css-indentation)]
      [(color-lexer) (dynamic-require 'css/village/hashlang/lexer 'css-lexer)]
      [else default])))
