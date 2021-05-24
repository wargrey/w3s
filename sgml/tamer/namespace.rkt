#lang typed/racket/base

(require sgml/xml)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(for ([file.xml (in-vector (current-command-line-arguments))])
  (displayln file.xml)
  
  (define doc.xml (read-xml-document file.xml))
  (for ([ns (in-list (xml-doc-namespaces doc.xml))])
    (printf "[(~a)  \"~a\"]~n" (car ns) (cdr ns))))
