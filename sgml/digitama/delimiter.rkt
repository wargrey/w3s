#lang typed/racket/base

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define <_ : Symbol (string->uninterned-symbol "<"))
(define _> : Symbol (string->uninterned-symbol ">"))
(define <? : Symbol (string->uninterned-symbol "<?"))
(define ?> : Symbol (string->uninterned-symbol "?>"))
(define <! : Symbol (string->uninterned-symbol "<!"))
(define </ : Symbol (string->uninterned-symbol "</"))
(define /> : Symbol (string->uninterned-symbol "/>"))
(define := : Symbol (string->uninterned-symbol "="))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-delim-symbol? : (-> Symbol Boolean)
  (lambda [datum]
    (not (or (symbol-interned? datum)
             (symbol-unreadable? datum)))))
