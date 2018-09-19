#lang racket/base

(provide xml-indentation)

(require racket/class)

(require sgml/digitama/digicore)
(require sgml/digitama/tokenizer)

(define xml-indentation ;: (-> (Instance Racket:Text<%>) Natural (Option Natural))
  (lambda [editor line]
    #false))
