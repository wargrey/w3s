#lang typed/racket/base

(require sgml/xml)

(require racket/runtime-path)

(define-runtime-path normalize.txml "normalize.txml")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define sax-display-prolog : (XML-Prolog-Handler Void)
  (lambda [pname version encoding standalone? etype datum]
    (if (eq? etype 'open)
        (printf "<?xml version=\"~a\" encoding=\"~a\" standalone=\"~a\" ?>~n"
                version (if (not encoding) "UTF-8" encoding) (if standalone? 'yes 'no))
        (printf "<!-- END OF ~a -->~n" pname))))

(define sax-display-doctype : (XML-Doctype-Handler Void)
  (lambda [?name public system datum]
    (cond [(not ?name) (sax-stop-with datum)]
          [(and public system) (printf "<!DOCTYPE ~a PUBLIC \"~a\" \"~a\">~n" ?name public system)]
          [(and system) (printf "<!DOCTYPE ~a SYSTEM \"~a\">~n" ?name system)]
          [else (printf "<!DOCTYPE ~a>~n" ?name)])))

(define sax-display-pi : (XML-PI-Handler Void)
  (lambda [?element target body datum]
    (cond [(not body) (printf "<?~a?>~n" target)]
          [else (printf "<!~a ~a>~n" target body)])))

(define sax-display-comment : (XML-Comment-Handler Void)
  (lambda [?element comment datum]
    (printf "<!--~a-->~n" comment)))

(define sax-display-element : (XML-Element-Handler Void)
  (lambda [name depth attrs empty? datum]
    (define indent (make-string (* depth 4) #\space))

    (cond [(not attrs) (printf "~a</~a>~n" indent name)]
          [else (printf "~a<~a" indent name)
                (for ([attr (in-list attrs)])
                  (printf " ~a=\"~a\"" (car attr) (cdr attr)))
                (if (not empty?) (printf ">") (printf "/>~n"))])))

(define sax-display-pcdata : (XML-PCData-Handler Void)
  (lambda [element pcdata cdata? datum]
    (when (and cdata?) (printf "<![CDATA["))
    (display pcdata)
    (when (and cdata?) (printf "]]>"))))

(define sax-display-whitespace : (XML-Space-Handler Void)
  (lambda [element space newline? datum]
    (display space)))

(define sax-display-entity : (XML-GEReference-Handler Void)
  (lambda [entity ?default-char datum]
    (when (char? ?default-char)
      (display ?default-char))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (define sax-handler
    ((inst make-xml-event-handler Void)
     #:prolog sax-display-prolog #:doctype sax-display-doctype #:pi sax-display-pi
     #:element sax-display-element #:pcdata sax-display-pcdata #:space sax-display-whitespace
     #:gereference sax-display-entity #:comment sax-display-comment))

  (read-xml-datum normalize.txml sax-handler))
