#lang typed/racket/base

(provide (all-defined-out))

(require sgml/xml)

(require "../digitama/stdin.rkt")
(require "../digitama/digicore.rkt")
(require "../digitama/normalize.rkt")

(require "../digitama/tokenizer.rkt")
(require "../digitama/tokenizer/port.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define tamer-xml:space : (-> [#:xml:space-filter (Option XML:Space-Filter)] Char * XML-Syntax-Any)
  (lambda [#:xml:space-filter [filter #false] . src]
    (define-values (/dev/xmlin version encoding standalone?)
      (xml-open-input-port (open-input-string (list->string src)) #true))

    (define-values (ws -)
      (xml-consume-token* /dev/xmlin 'tamer-xml:space
                          (cons xml-consume-token:* 1)))
    
    (cond [(not (xml:whitespace? ws)) ws]
          [else (xml-whitespace-preserve-filter ws filter)])))


(module+ main
  (tamer-xml:space #\return #\newline
                   #\newline
                   #\return))
