#lang racket/base

(provide css-indentation)

(require racket/class)

(require css/digitama/syntax/digicore)
(require css/digitama/syntax/tokenizer)

(define css-indentation ;: (-> (Instance Racket:Text<%>) Natural (Option Natural))
  (lambda [editor line]
    #false))
