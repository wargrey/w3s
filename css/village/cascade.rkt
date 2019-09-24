#lang typed/racket/base

;;; This module provides some nonstandard cascade APIs

(provide (all-defined-out))

(require "../digitama/syntax/digicore.rkt")
(require "../digitama/syntax/selector.rkt")
(require "../digitama/syntax/cascade.rkt")
(require "../digitama/syntax/variables.rkt")
(require "../digitama/syntax/condition.rkt")
(require "../digitama/syntax/grammar.rkt")
(require "../digitama/syntax/misc.rkt")

(define css-cascade*
  : (All (Preference Env)
         (case-> [-> (Listof+ CSS-Stylesheet) (Listof+ CSS-Subject) CSS-Declaration-Parsers
                     (CSS-Cascaded-Value-Filter Preference) (Option CSS-Values)
                     (Values (Listof Preference) (Listof CSS-Values))]
                 [-> (Listof+ CSS-Stylesheet) (Listof+ CSS-Subject) CSS-Declaration-Parsers
                     (CSS-Cascaded-Value+Filter Preference Env) (Option CSS-Values) Env
                     (Values (Listof Preference) (Listof CSS-Values))]))
  (let ()
    (define do-cascade* : (All (Preference) (-> (Listof+ CSS-Stylesheet) (Listof+ CSS-Subject) CSS-Declaration-Parsers
                                                (Option CSS-Values) (-> CSS-Values Preference)
                                                (Values (Listof Preference) (Listof CSS-Values))))
      (lambda [stylesheets stcejbus desc-parsers inherited-values do-value-filter]
        (hash-clear! !importants) ; TODO: if it is placed correctly, perhaps a specification for custom cascading process is required.
        (define-values (rotpircsed seulav)
          (let cascade-stylesheets* : (values (Listof Preference) (Listof CSS-Values))
            ([batch : (Listof CSS-Stylesheet) stylesheets]
             [all-rotpircsed : (Listof Preference) null]
             [all-seulav : (Listof CSS-Values) null])
            (for/fold ([descriptors++ : (Listof Preference) all-rotpircsed] [values++ : (Listof CSS-Values) all-seulav])
                      ([this-sheet (in-list batch)])
              (define-values (sub-rotpircsed sub-seulav)
                (cascade-stylesheets* (css-select-children this-sheet desc-parsers) descriptors++ values++))
              (define this-values : (Listof CSS-Values)
                (css-cascade-rules* (css-stylesheet-grammars this-sheet) stcejbus desc-parsers
                                    (css-cascade-viewport (default-css-media-features)
                                                          (css-stylesheet-viewports this-sheet))))
              (for/fold ([this-rotpircsed : (Listof Preference) sub-rotpircsed]
                         [this-seulav : (Listof CSS-Values) sub-seulav])
                        ([declared-values : CSS-Values (in-list this-values)])
                (css-resolve-variables declared-values inherited-values)
                (values (cons (do-value-filter declared-values) this-rotpircsed)
                        (cons declared-values this-seulav))))))
        (values (reverse rotpircsed) (reverse seulav))))
    (case-lambda
      [(stylesheets stcejbus desc-parsers value-filter inherited-values)
       (do-cascade* stylesheets stcejbus desc-parsers inherited-values
                    (λ [[declared-values : CSS-Values]] (value-filter declared-values inherited-values)))]
      [(stylesheets stcejbus desc-parsers value-filter inherited-values env)
       (do-cascade* stylesheets stcejbus desc-parsers inherited-values
                    (λ [[declared-values : CSS-Values]] (value-filter declared-values inherited-values env)))])))

(define css-cascade-rules* : (->* ((Listof CSS-Grammar-Rule) (Listof+ CSS-Subject) CSS-Declaration-Parsers)
                                  (CSS-Media-Features) (Listof CSS-Values))
  ;;; https://drafts.csswg.org/css-cascade/#filtering
  ;;; https://drafts.csswg.org/css-cascade/#cascading
  (lambda [rules stcejbus desc-parsers [top-descriptors (default-css-media-features)]]
    (call-with-css-viewport-from-media #:descriptors top-descriptors
      (define-values (ordered-srcs single?) (css-select-styles rules stcejbus desc-parsers top-descriptors))
      (if (and single?)
          (for/list : (Listof CSS-Values) ([src (in-list ordered-srcs)])
            (css-cascade-declarations desc-parsers (in-value (vector-ref src 1)) css:ident-datum))
          (for/list : (Listof CSS-Values) ([src (in-list ordered-srcs)])
            (define alter-descriptors : CSS-Media-Features (vector-ref src 2))
            (if (eq? alter-descriptors top-descriptors)
                (css-cascade-declarations desc-parsers (in-value (vector-ref src 1)) css:ident-datum)
                (call-with-css-viewport-from-media #:descriptors alter-descriptors
                  (css-cascade-declarations desc-parsers (in-value (vector-ref src 1)) css:ident-datum))))))))
