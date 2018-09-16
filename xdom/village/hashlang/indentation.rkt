#lang racket/base

(provide css-indentation)

(require racket/class)

(require xdom/digitama/digicore)
(require xdom/digitama/tokenizer)

(define css-indentation ;: (-> (Instance Racket:Text<%>) Natural (Option Natural))
  (lambda [editor line]
    #false))
