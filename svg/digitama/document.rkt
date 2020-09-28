#lang typed/racket/base

(provide (all-defined-out))

(require sgml/xml)

;(require "../village/hashlang/svg11.dtd")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define svg-open-external-dtd-input-port : Open-Input-XML-XXE
  (lambda [rootdir public system]
    (open-input-file (collection-file-path "svg11.dtd" "svg" "village" "hashlang"))))

(define read-svg-document* : (-> SGML-StdIn XML-Document*)
  (lambda [/dev/rawin]
    (read-xml-document* /dev/rawin svg-open-external-dtd-input-port)))
