#lang typed/racket/base

(require sgml/digitama/schema)

(require "../village/svglang/svg11.tdtd")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (svg-attribute-list [tag-name : Symbol]) : (Listof Symbol)
  (hash-keys
   (hash-ref (xml-schema-attributes svg11*.dtd) tag-name
             (inst make-hasheq Symbol XSch-Attribute))))

(define (svg-children [tag-name : Symbol]) : (U False Keyword (Listof Symbol) (Pairof Schema-Element-Children Char))
  (define e (hash-ref (xml-schema-elements svg11*.dtd) tag-name (λ [] #false)))

  (cond [(xsch-mixed-element? e) (xsch-mixed-element-children e)]
        [(xsch-element+children? e) (xsch-element+children-content e)]
        [(xsch-element? e) '#:empty]
        [else #false]))

(for ([(e elems) (in-hash (xml-schema-elements svg11*.dtd))])
  (displayln (cons e (svg-children e))))
