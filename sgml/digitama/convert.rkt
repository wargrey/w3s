#lang typed/racket/base

(provide (all-defined-out) prop:convertible)

(require typed/file/convertible)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-element-write : (All (s) (->* (s (-> s (Listof (Pairof Symbol String)))) (Output-Port) Void))
  (lambda [self flatten-attributes [/dev/svgout (current-output-port)]]
    (write-char #\< /dev/svgout)
    (write (object-name self) /dev/svgout)

    (for ([attr (in-list (flatten-attributes self))])
      (write-char #\space /dev/svgout)
      (write (car attr) /dev/svgout)
      (write-char #\= /dev/svgout)
      (write-char #\" /dev/svgout)
      (write-string (cdr attr) /dev/svgout)
      (write-char #\" /dev/svgout))

    (write-char #\/ /dev/svgout)
    (write-char #\> /dev/svgout)))

(define xml-element->bytes : (All (s) (-> s (-> s (Listof (Pairof Symbol String))) Bytes))
  (lambda [self flatten-attributes]
    (define /dev/svgout (open-output-bytes '/dev/svgout))

    (xml-element-write self flatten-attributes /dev/svgout)
    (get-output-bytes /dev/svgout #true)))
