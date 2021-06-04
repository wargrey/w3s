#lang typed/racket/base

(require sgml/xml)

(require racket/pretty)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (pretty-print-columns 160)
  (global-port-print-handler pretty-print)

  (for/list : (Listof Any) ([file.xml (in-vector (current-command-line-arguments))])
    (xml-document-normalize (read-xml-document file.xml))))
