#lang typed/racket/base

(provide (all-defined-out))
(provide (struct-out XML-DTD) read-xml-type-definition)
(provide (struct-out XML-Type) XML-Type-Entities)
(provide xml-dtd-expand default-dtd-entity-expansion-upsize)

(require "digitama/dtd.rkt")
(require "digitama/normalize.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-dtd-entity-expand : (->* (XML-DTD)
                                     ((Option XML-Type-Entities) Boolean #:entity-expansion-upsize (Option Index))
                                     XML-Type-Entities)
  (lambda [dtd [int-entities #false] [merge? #true] #:entity-expansion-upsize [expansion-upsize (default-dtd-entity-expansion-upsize)]]
    (define-values (entities _)
      (xml-dtd-entity-expand/partition dtd int-entities merge?
                                       expansion-upsize))

    entities))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module reader racket/base
  (provide (except-out (all-from-out racket/base) read read-syntax))

  (provide (rename-out [dtd-read read]))
  (provide (rename-out [dtd-read-syntax read-syntax]))
  (provide (rename-out [dtd-info get-info]))
  
  (require sgml/village/sgmlang/reader))
