#lang typed/racket/base

(provide (all-defined-out))

(require "../../sax.rkt")
(require "../../prompt.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (T) sax-display-prolog : (XML-Prolog-Handler T)
  (lambda [pname version encoding standalone? etype? datum]
    (if (or etype?)
        (printf "<?xml version=\"~a\" encoding=\"~a\" standalone=\"~a\" ?>~n"
                version (if (not encoding) "UTF-8" encoding) (if standalone? 'yes 'no))
        (printf "<!-- END OF ~a -->~n" pname))
    datum))

(define #:forall (T) sax-display-doctype : (XML-Doctype-Handler T)
  (lambda [?name public system datum]
    (cond [(not ?name) (sax-stop-with datum)]
          [(and public system) (printf "<!DOCTYPE ~a PUBLIC \"~a\" \"~a\">~n" ?name public system)]
          [(and system) (printf "<!DOCTYPE ~a SYSTEM \"~a\">~n" ?name system)]
          [else (printf "<!DOCTYPE ~a>~n" ?name)])
    datum))

(define #:forall (T) sax-display-pi : (XML-PI-Handler T)
  (lambda [xpath target body datum]
    (cond [(not body) (printf "<?~a?>~n" target)]
          [else (printf "<!~a ~a>~n" target body)])
    datum))

(define #:forall (T) sax-display-comment : (XML-Comment-Handler T)
  (lambda [xpath comment preserve? datum]
    (printf "<!--~a-->" comment)
    (when (not preserve?)
      (newline))
    datum))

(define #:forall (T) sax-display-element : (XML-Element-Handler T)
  (lambda [name xpath attrs empty? preserve? datum]
    (define indent (make-string (* (length xpath) 4) #\space))

    (cond [(not attrs) (printf "~a</~a>~n" (if preserve? "" indent) name)]
          [else (printf "~a<~a" indent name)
                (for ([attr (in-list attrs)])
                  (printf " ~a=\"~a\"" (car attr) (cdr attr)))
                (if (not empty?) (printf ">") (printf "/>"))
                (when (not preserve?) (newline))])
    datum))

(define #:forall (T) sax-display-pcdata : (XML-PCData-Handler T)
  (lambda [element xpath pcdata preserve? cdata? datum]
    (define indention
      (cond [(or preserve?) ""]
            [else (make-string (* (+ (length xpath) 1) 4) #\space)]))
    
    (cond [(and cdata?) (printf "<![CDATA[~a[[>" pcdata)]
          [else (printf "~a~a" indention pcdata)
                (when (not preserve?)
                  (newline))])
    datum))

(define #:forall (T) sax-display-entity : (XML-GEReference-Handler T)
  (lambda [entity ?default-char datum]
    (when (char? ?default-char)
      (display ?default-char))
    datum))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (T) sax-xml-writer : (-> (XML-Event-Handlerof T))
  (lambda []
    ((inst make-xml-event-handler T)
     #:prolog sax-display-prolog #:doctype sax-display-doctype #:pi sax-display-pi
     #:element sax-display-element #:pcdata sax-display-pcdata
     #:gereference sax-display-entity #:comment sax-display-comment)))

(define sax-handler/xml-writer ((inst sax-xml-writer Void)))
