#lang typed/racket/base

(provide (all-defined-out))
(provide (struct-out XML-DTD) read-xml-type-definition)
(provide (struct-out XML-Type) XML-Type-Entities)
(provide Open-Input-XML-XXE xml-dtd-expand)
(provide default-xml-ipe-topsize default-xml-xxe-topsize default-xml-xxe-timeout)

(require "digitama/dtd.rkt")
(require "digitama/normalize.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-dtd-entity-expand : (->* (XML-DTD)
                                     (#:open-xxe-input-port (Option Open-Input-XML-XXE)
                                      #:ipe-topsize (Option Index) #:xxe-topsize (Option Index) #:xxe-timeout (Option Real)
                                      (Option XML-Type-Entities) Boolean)
                                     XML-Type-Entities)
  (lambda [#:open-xxe-input-port [open-port #false]
           #:ipe-topsize [ipe-topsize (default-xml-ipe-topsize)] #:xxe-topsize [xxe-topsize (default-xml-xxe-topsize)] #:xxe-timeout [timeout (default-xml-xxe-timeout)]
           dtd [int-entities #false] [merge? #true]]
    (define-values (entities _)
      (xml-dtd-entity-expand/partition dtd int-entities merge? ipe-topsize
                                       (and open-port (vector open-port xxe-topsize timeout))))

    entities))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module reader racket/base
  (provide (except-out (all-from-out racket/base) read read-syntax))

  (provide (rename-out [dtd-read read]))
  (provide (rename-out [dtd-read-syntax read-syntax]))
  (provide (rename-out [dtd-info get-info]))
  
  (require sgml/village/sgmlang/reader))
