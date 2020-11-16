#lang typed/racket/base

(require sgml/sax)

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
    (printf "<!--~a-->" comment)))

(define sax-display-element : (XML-Element-Handler Void)
  (lambda [name depth attrs empty? datum]
    (define indent (make-string (* depth 4) #\space))

    (cond [(not attrs) (printf "~a</~a>~n" indent name)]
          [else (printf "~a<~a" indent name)
                (for ([attr (in-list attrs)])
                  (printf " ~a=\"~a\"" (car attr) (cdr attr)))
                (if (not empty?) (printf ">~n") (printf "/>~n"))])))

(define sax-display-pcdata : (XML-PCData-Handler Void)
  (lambda [element depth pcdata cdata? datum]
    (define indention (make-string (* (+ depth 1) 4) #\space))
    
    (cond [(and cdata?) (printf "<![CDATA[~a[[>" pcdata)]
          [else (printf "~a~a~n" indention pcdata)])))

(define sax-display-entity : (XML-GEReference-Handler Void)
  (lambda [entity ?default-char datum]
    (when (char? ?default-char)
      (display ?default-char))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (require "normalize.txml")
  
  (define sax-handler
    ((inst make-xml-event-handler Void)
     #:prolog sax-display-prolog #:doctype sax-display-doctype #:pi sax-display-pi
     #:element sax-display-element #:pcdata sax-display-pcdata
     #:gereference sax-display-entity #:comment sax-display-comment))

  (load-xml-datum (assert (xml-doc-location normalize.xml) string?) sax-handler))
