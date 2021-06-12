#lang typed/racket/base

(provide (all-defined-out))

(require bitmap/digitama/misc)

(require/provide "base.rkt" "sugar.rkt")

(module reader racket/base
  (provide (except-out (all-from-out racket/base) read read-syntax))

  (provide (rename-out [css-read read]))
  (provide (rename-out [css-read-syntax read-syntax]))
  (provide (rename-out [css-info get-info]))
  
  (require css/village/hashlang/reader))
