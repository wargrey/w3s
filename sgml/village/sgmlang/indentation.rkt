#lang racket/base

(provide xml-indentation)
(provide (rename-out [xml-indentation dtd-indentation]
                     [xml-indentation rnc-indentation]))

(require racket/class)

(require sgml/digitama/digicore)
(require sgml/digitama/tokenizer)

(define xml-indentation ;: (-> (Instance Racket:Text<%>) Natural (Option Natural))
  (lambda [editor line]
    #false))
