#lang typed/racket/base

(provide (all-defined-out))

(require sgml/xml)

(require "../digitama/stdin.rkt")
(require "../digitama/digicore.rkt")
(require "../digitama/normalize.rkt")

(require "../digitama/tokenizer.rkt")
(require "../digitama/tokenizer/port.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define tamer-xml:space : (-> Symbol [#:xml:space-filter (Option XML:Space-Filter)] [#:xml:lang String] Char * XML-Syntax-Any)
  (lambda [#:xml:space-filter [filter #false] #:xml:lang [xml:lang ""] xml:space . src]
    (define ws (tamer-src->token (format "xml:space-~a" xml:space) filter xml:lang src))

    (cond [(not (xml:whitespace? ws)) ws]
          [(eq? xml:space 'preserve) (xml:space=preserve* '|| ws filter xml:lang)]
          [else (tamer-xml:space-default ws filter '|| xml:lang)])))

(define tamer-svg:space : (-> Symbol [#:xml:lang String] Char * XML-Syntax-Any)
  (lambda [#:xml:lang [xml:lang ""] xml:space . src]
    (define ws (tamer-src->token (format "svg:space-~a" xml:space) svg:space-filter xml:lang src))

    (cond [(not (xml:whitespace? ws)) ws]
          [(eq? xml:space 'preserve) (xml:space=preserve* '|| ws svg:space-filter xml:lang)]
          [else (tamer-xml:space-default ws svg:space-filter '|| xml:lang)])))

(define tamer-src->token : (-> (U Symbol String) (Option XML:Space-Filter) String (Listof Char) XML-Syntax-Any)
  (lambda [portname filter xml:lang src]
    (define-values (/dev/xmlin version encoding standalone?)
      (xml-open-input-port
       (open-input-string (string-append "<space>"
                                         (list->string src)
                                         "</space>"))
       #true))

    (let*-values ([(< status) (xml-consume-token* /dev/xmlin portname (cons xml-consume-token:* 1))]
                  [(space status) (xml-consume-token* /dev/xmlin portname status)]
                  [(> status) (xml-consume-token* /dev/xmlin portname status)]
                  [(ws status) (xml-consume-token* /dev/xmlin portname status)])
      ws)))

(define tamer-xml:space-default : (-> XML:WhiteSpace (Option XML:Space-Filter) Symbol String XML-Syntax-Any)
  (lambda [ws filter tag xml:lang]
    (assert (car (xml-child-cons* (list ws) (list ws) filter tag xml:lang))
            xml:whitespace?)))


(module+ main
  (require "normalize.txml")
  
  (tamer-xml:space 'preserve
                   #\return #\newline
                   #\newline
                   #\return)
  
  (tamer-svg:space 'default
                   #\return #\newline
                   #\newline
                   #\return)

  (tamer-svg:space 'preserve
                   #\return #\newline
                   #\newline
                   #\return)

  normalize.xml

  ; NOTE: Plain APIs does not support DTD, so don't be surprised entity references are not expanded properly.
  (xml-document-normalize #:xml:space 'default #:xml:space-filter svg:space-filter
                          (read-xml-document (assert (xml-doc-location normalize.xml) string?))))
