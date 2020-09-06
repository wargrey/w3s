#lang typed/racket/base

(require sgml/xml)
(require sgml/dtd)

(require "../../digitama/dtd.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define cmdv (current-command-line-arguments))

(collect-garbage)
(collect-garbage)
(collect-garbage)

(define cmd.xml : XML-Document* (time (read-xml-document* (if (> (vector-length cmdv) 0) (string->path (vector-ref cmdv 0)) ""))))

(collect-garbage)
(collect-garbage)
(collect-garbage)

(void (time (xml-document*-normalize cmd.xml)))
