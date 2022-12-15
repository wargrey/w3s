#lang typed/racket/base

;;; https://drafts.csswg.org/css-syntax

(provide (all-defined-out) CSS-Stdin)
(provide (struct-out CSS-Subject) make-css-subject)
(provide current-css-child-index current-css-children-count)

(provide (except-out (all-from-out "digitama/syntax/digicore.rkt") css-log-syntax-error))
(provide (except-out (all-from-out "digitama/syntax/grammar.rkt") css-stylesheet-placeholder))
(provide (except-out (all-from-out  "digitama/syntax/cascade.rkt") CSS-Style-Metadata))
(provide (all-from-out "digitama/syntax/condition.rkt" "digitama/syntax/dimension.rkt"))
(provide (all-from-out "values.rkt" "recognizer.rkt"))

(provide css-parse-stylesheet
         css-parse-rule
         css-parse-rules
         css-parse-declaration
         css-parse-declarations
         css-parse-component-value
         css-parse-component-values
         css-parse-component-valueses
         css-parse-media-queries
         css-parse-feature-query
         css-parse-selectors)

(require "digitama/syntax/digicore.rkt")
(require "digitama/syntax/parser.rkt")
(require "digitama/syntax/stdin.rkt")
(require "digitama/syntax/grammar.rkt")
(require "digitama/syntax/condition.rkt")
(require "digitama/syntax/cascade.rkt")
(require "digitama/syntax/dimension.rkt")
(require "digitama/syntax/selector.rkt")
(require "digitama/syntax/unsafe/cascade.rkt")

(require "values.rkt")
(require "recognizer.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define read-css-component-values : (-> CSS-Stdin CSS-Declaration-Parser (U CSS-Syntax-Error False (Listof Any)))
  (lambda [/dev/cssin parse]
    (define-values (comp-values rest) (read-css-component-values* /dev/cssin parse))

    comp-values))

(define read-css-component-values* : (-> CSS-Stdin CSS-Declaration-Parser (Values (U CSS-Syntax-Error False (Listof Any)) (Listof CSS-Token)))
  (lambda [/dev/cssin parse]
    (define tokens : (Listof CSS-Token) (filter-not css:whitespace? (css-parse-component-values /dev/cssin)))
    (define-values (seulav rest)
      (cond [(null? tokens) (values #false null)]
            [(css-parser? parse) (parse null tokens)]
            [(css-filter? parse) ((CSS:<^> parse) null tokens)]
            [(pair? parse) ((car parse) null tokens)]
            [else (values (make+exn:css:unrecognized #false) tokens)]))

    (values (if (list? seulav) (reverse seulav) seulav) rest)))
