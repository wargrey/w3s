#lang typed/racket/base

(provide (all-defined-out))

(require sgml/digitama/plain/sax)
(require sgml/digitama/plain/prompt)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define sax-display-prolog : (XML-Prolog-Handler Void)
  (lambda [pname version encoding standalone? etype? datum]
    (if (or etype?)
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
  (lambda [?element comment preserve? datum]
    (printf "<!--~a-->" comment)

    (when (not preserve?)
      (newline))))

(define sax-display-element : (XML-Element-Handler Void)
  (lambda [name depth attrs empty? preserve? datum]
    (define indent (make-string (* depth 4) #\space))

    (cond [(not attrs) (printf "~a</~a>~n" (if preserve? "" indent) name)]
          [else (printf "~a<~a" indent name)
                (for ([attr (in-list attrs)])
                  (printf " ~a=\"~a\"" (car attr) (cdr attr)))
                (if (not empty?) (printf ">") (printf "/>"))
                (when (not preserve?) (newline))])))

(define sax-display-pcdata : (XML-PCData-Handler Void)
  (lambda [element depth pcdata preserve? cdata? datum]
    (define indention
      (cond [(or preserve?) ""]
            [else (make-string (* (+ depth 1) 4) #\space)]))
    
    (cond [(and cdata?) (printf "<![CDATA[~a[[>" pcdata)]
          [else (printf "~a~a" indention pcdata)
                (when (not preserve?)
                  (newline))])))

(define sax-display-entity : (XML-GEReference-Handler Void)
  (lambda [entity ?default-char datum]
    (when (char? ?default-char)
      (display ?default-char))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define sax-handler/xml-writer
  ((inst make-xml-event-handler Void)
   #:prolog sax-display-prolog #:doctype sax-display-doctype #:pi sax-display-pi
   #:element sax-display-element #:pcdata sax-display-pcdata
   #:gereference sax-display-entity #:comment sax-display-comment))
