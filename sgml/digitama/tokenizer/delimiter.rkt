#lang typed/racket/base

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define <? : Symbol (string->uninterned-symbol "<?"))
(define ?> : Symbol (string->uninterned-symbol "?>"))
(define <! : Symbol (string->uninterned-symbol "<!"))
(define <!& : Symbol (string->uninterned-symbol "<!["))
(define <!&CDATA& : Symbol (string->uninterned-symbol "<![CDATA["))
(define $$> : Symbol (string->uninterned-symbol "]]>"))
(define /> : Symbol (string->uninterned-symbol "/>"))

(define /= : Symbol (string->uninterned-symbol "|="))
(define &= : Symbol (string->uninterned-symbol "&="))

;;; WARNING: `#\>` is the close delimiter for Decls and EndTags
;;    but these are not open and close delimiters 
(define stag> : Symbol (string->uninterned-symbol ">"))
(define </ : Symbol (string->uninterned-symbol "</"))
(define csec& : Symbol (string->uninterned-symbol "["))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-delim-symbol? : (-> Symbol Boolean)
  (lambda [datum]
    (not (or (symbol-interned? datum)
             (symbol-unreadable? datum)))))
