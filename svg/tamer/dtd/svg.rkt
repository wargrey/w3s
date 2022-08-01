#lang typed/racket/base

(require typed/net/http-client)

(require sgml/xml)
(require sgml/sax)
(require sgml/xexpr)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type SAX-Toclist-Statue (Pairof (Listof String) (Option Symbol)))

(define (read-w3c-url [uri : String]) : Input-Port
  (define-values (status headers /dev/w3cin) (http-sendrecv "www.w3.org" uri))
  (displayln (cons uri status))
  /dev/w3cin)

(define (svgdoc-toc-list) : (Listof String)
  (define /dev/w3cin (read-w3c-url "https://www.w3.org/TR/SVG11/Overview.html"))

  (define sax-seek-tocline : (XML-Element-Handler SAX-Toclist-Statue)
    (lambda [name depth attrs empty? preserve? datum]
      (define indent (make-string (* depth 4) #\space))
      
      (or (and attrs
               (case name
                 [(li) (let ([class (assoc 'class attrs)])
                         (and class
                              (equal? (cdr class) "tocline1")
                              (cons (car datum) 'green)))]
                 [(a) (and (eq? (cdr datum) 'green)
                           (let ([href (assoc 'href attrs)])
                             (and href
                                  (string? (cdr href))
                                  (cons (cons (cdr href) (car datum)) #false))))]
                 [else #false]))
          datum)))

  (define toc-handler : (XML-Event-Handlerof SAX-Toclist-Statue)
    ((inst make-xml-event-handler SAX-Toclist-Statue)
     #:element sax-seek-tocline))

  (define datum : SAX-Toclist-Statue (read-xml-datum /dev/w3cin toc-handler (cons null #false)))
  (reverse (car datum)))

(module+ main
  (svgdoc-toc-list))
