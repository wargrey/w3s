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
          [(and public system) (printf "<!DOCTYPE ~a PUBLIC ~a ~a>~n" ?name public system)]
          [(and system) (printf "<!DOCTYPE ~a SYSTEM ~a>~n" ?name system)]
          [else (printf "<!DOCTYPE ~a ~a ~a>~n" ?name public system)])))

(define sax-display-pi : (XML-PI-Handler Void)
  (lambda [?element target body datum]
    (cond [(not body) (printf "<?~a?>~n" target)]
          [else (printf "<!~a ~a>~n" target body)])))

(define sax-display-comment : (XML-Comment-Handler Void)
  (lambda [?element comment datum]
    (printf "<!--~a-->~n" comment)))

(define sax-display-element : (XML-Element-Handler Void)
  (lambda [name depth event datum]
    (define indent (make-string (* depth 4) #\space))
    
    (case event
      [(open) (printf "~a<~a" indent name)]
      [(close) (printf "~a</~a>~n" indent name)]
      [(close-empty) (printf "/>~n" name)]
      [(close-tag) (printf ">")])))

(define sax-display-attribute : (XML-Attribute-Handler Void)
  (lambda [element name value datum]
    (printf " ~a=\"~a\"" name value)))

(define sax-display-attrilist : (XML-AttriList-Handler Void)
  (lambda [element attributes datum]
    (when (pair? attributes)
      (display #\space))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (define sax-handler
    ((inst make-xml-event-handler Void)
     #:prolog sax-display-prolog
     #:doctype sax-display-doctype
     #:pi sax-display-pi
     #:element sax-display-element
     #:attribute sax-display-attribute
     #:attrilist sax-display-attrilist
     #:comment sax-display-comment))

  (read-xml-datum normalize.txml sax-handler))
