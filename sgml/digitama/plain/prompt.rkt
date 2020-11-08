#lang typed/racket/base

(provide (all-defined-out))

(require typed/racket/unsafe)

(unsafe-require/typed
 racket/base
 [call-with-continuation-prompt (All (a b) (-> (-> a) (Prompt-Tagof Any Any) (Option (-> (-> a) b)) (U a b)))]
 [abort-current-continuation (All (a) (-> (Prompt-Tagof Any Any) a Nothing))]
 [default-continuation-prompt-tag (-> (Prompt-Tagof Any Any))])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NOTE: (Prompt-Tagof Any (-> a ... a b)) cannot be type-checked meanwhile since it involves 'chaperone/sc'

(define default-sax-prompt : (Parameterof (Prompt-Tagof Any Any)) (make-parameter (default-continuation-prompt-tag)))

(define sax-start : (All (a b) (-> (Option Symbol) (-> a) a))
  (lambda [tagname do-task]
    (define current-prompt : (Prompt-Tagof Any Any)
      (cond [(not tagname) (default-continuation-prompt-tag)]
            [else (make-continuation-prompt-tag tagname)]))

    (parameterize ([default-sax-prompt current-prompt])
      (call-with-continuation-prompt do-task current-prompt #false))))

(define sax-stop-with : (All (seed) (-> seed Nothing))
  (lambda [datum]
    (abort-current-continuation (default-sax-prompt)
                                (λ [] datum))))
