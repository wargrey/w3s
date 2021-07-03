#lang typed/racket/base

;;; https://drafts.csswg.org/css-text-decor

(provide (all-defined-out))

(require racket/list)

(require "syntax/digicore.rkt")
(require "syntax/misc.rkt")

(require "../recognizer.rkt")
(require "color.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define css-text-decor-line-options : (Listof Symbol) '(underline overline line-through blink))
(define css-text-decor-skip-options : (Listof Symbol) '(objects spaces ink edges box-decoration))

(define css-text-decor-style-option : (Listof Symbol) '(solid double dotted dashed wavy))

(define css-fold-decoration-line : CSS-Longhand-Update
  (lambda [_ old-options option]
    (if (list? old-options) (cons option old-options) (list option))))
  
(define <:text-decoration:> : CSS-Shorthand+Parser
  ;;; https://drafts.csswg.org/css-text-decor/#text-decoration-color-property
  (cons (CSS<*> (CSS<+> #:any
                        (CSS:<^> (<css-keyword> 'none) 'text-decoration-line)
                        (CSS:<^> (<css-keyword> css-text-decor-line-options) 'text-decoration-line css-fold-decoration-line)
                        (CSS:<^> (<css-keyword> css-text-decor-style-option) 'text-decoration-style)
                        (CSS:<^> (<css-color>) 'text-decoration-color)))
        '(text-decoration-line text-decoration-color text-decoration-style)))

(define css->text-decor-lines : (CSS->Racket (Listof Symbol))
  (lambda [[_ : Symbol] [value : Any]]
    (cond [(list? value) (reverse (remove-duplicates (reverse (filter symbol? value))))]
          [else null])))
