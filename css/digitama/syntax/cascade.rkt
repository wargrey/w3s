#lang typed/racket/base

;;; https://drafts.csswg.org/css-cascade
;;; https://drafts.csswg.org/css-values

(provide (except-out (all-defined-out) desc-more-important? do-filter do-parse))

(require racket/sequence)

(require "misc.rkt")
(require "digicore.rkt")
(require "selector.rkt")
(require "variables.rkt")
(require "condition.rkt")
(require "grammar.rkt")

(require "../device-adapt.rkt")
(require "../../recognizer.rkt")
(require "unsafe/cascade.rkt")

(require (for-syntax racket/base))
(require (for-syntax syntax/parse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type CSS-Style-Metadata (Vector Natural (Listof CSS-Declaration) CSS-Media-Features))

(define-syntax (call-with-css-viewport-from-media stx)
  (syntax-parse stx
    [(_ (~optional (~seq #:descriptors ?env)) sexp ...)
     (with-syntax ([env (or (attribute ?env) #'(default-css-media-features))])
       (syntax/loc stx
         (let ([w (hash-ref env 'width (λ [] #false))]
               [h (hash-ref env 'height (λ [] #false))])
           (when (and (real? w) (positive? w)) (css-vw (real->double-flonum w)))
           (when (and (real? h) (positive? h)) (css-vh (real->double-flonum h)))
           sexp ...)))]))

(define css-warn-unknown-property? : (Parameterof Boolean) (make-parameter #true))
(define css-warn-unknown-feature? : (Parameterof Boolean) (make-parameter #false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define css-variable-cascade : (-> (ListofU+ CSS-Stylesheet) (ListofU+ CSS-Subject) (Option CSS-Values) CSS-Values)
  (lambda [stylesheets stcejbus inherited-values]
    (define-values (_ variables) (css-cascade stylesheets stcejbus void void inherited-values))

    variables))

(define css-cascade-children
  : (All (s) (->* ((ListofU+ CSS-Stylesheet) (ListofU+ CSS-Subject) CSS-Declaration-Parsers (CSS-Cascaded-Value-Filter s) (Option CSS-Values) Positive-Index)
                  (Positive-Index Positive-Index)
                  (Listof s)))
  (lambda [stylesheets stcejbus desc-parsers value-filter inherited-values count [start-idx 1] [total (+ (- start-idx 1) count)]]
    (parameterize ([current-css-children-count (assert total index?)])
      (define end-idx : Positive-Index (assert (+ (- start-idx 1) count) index?))
      (let cascade-style ([idx : Positive-Fixnum start-idx]
                          [rendlihc : (Listof s) null])
        (cond [(> idx end-idx) (reverse rendlihc)]
              [else (let-values ([(style _) (parameterize ([current-css-child-index idx])
                                              (css-cascade stylesheets stcejbus desc-parsers value-filter inherited-values))])
                      (cascade-style (add1 idx) (cons style rendlihc)))])))))

(define css-cascade
  : (All (Preference Env)
         (case-> [-> (ListofU+ CSS-Stylesheet) (ListofU+ CSS-Subject) CSS-Declaration-Parsers
                     (CSS-Cascaded-Value-Filter Preference) (Option CSS-Values)
                     (Values Preference CSS-Values)]
                 [-> (ListofU+ CSS-Stylesheet) (ListofU+ CSS-Subject) CSS-Declaration-Parsers
                     (CSS-Cascaded-Value+Filter Preference Env) (Option CSS-Values) Env
                     (Values Preference CSS-Values)]))
  ;;; https://drafts.csswg.org/css-cascade/#filtering
  ;;; https://drafts.csswg.org/css-cascade/#cascading
  ;;; https://drafts.csswg.org/css-cascade/#at-import
  (let ()
    (define do-cascade : (-> (ListofU+ CSS-Stylesheet) (ListofU+ CSS-Subject) CSS-Declaration-Parsers (Option CSS-Values) CSS-Values)
      (lambda [stylesheets stcejbus desc-parsers inherited-values]
        (define declared-values : CSS-Values (make-css-values))
        (hash-clear! !importants)
        (let cascade-stylesheets ([batch : (U CSS-Stylesheet (Listof CSS-Stylesheet)) stylesheets])
          (for ([this-sheet (if (list? batch) (in-list batch) (in-value batch))])
            (cascade-stylesheets (css-select-children this-sheet desc-parsers))
            (css-cascade-rules (css-stylesheet-grammars this-sheet) stcejbus desc-parsers declared-values
                               (css-cascade-viewport (default-css-media-features) (css-stylesheet-viewports this-sheet)))))
        (css-resolve-variables declared-values inherited-values)
        declared-values))
    (case-lambda
      [(stylesheets stcejbus desc-parsers value-filter inherited-values)
       (define declared-values : CSS-Values (do-cascade stylesheets stcejbus desc-parsers inherited-values))
       (values (value-filter declared-values inherited-values) declared-values)]
      [(stylesheets stcejbus desc-parsers value-filter inherited-values env)
       (define declared-values : CSS-Values (do-cascade stylesheets stcejbus desc-parsers inherited-values))
       (values (value-filter declared-values inherited-values env) declared-values)])))

(define css-cascade-rules : (->* ((Listof CSS-Grammar-Rule) (ListofU+ CSS-Subject) CSS-Declaration-Parsers)
                                 (CSS-Values CSS-Media-Features) CSS-Values)
  ;;; https://drafts.csswg.org/css-cascade/#filtering
  ;;; https://drafts.csswg.org/css-cascade/#cascading
  (lambda [rules stcejbus desc-parsers [descbase (make-css-values)] [top-descriptors (default-css-media-features)]]
    (call-with-css-viewport-from-media #:descriptors top-descriptors
      (define-values (ordered-srcs single?) (css-select-styles rules stcejbus desc-parsers top-descriptors))
      (if (and single?)
          (let ([source-ref (λ [[src : CSS-Style-Metadata]] : (Listof CSS-Declaration) (vector-ref src 1))])
            (css-cascade-declarations desc-parsers (sequence-map source-ref (in-list ordered-srcs)) css:ident-norm descbase))
          (for ([src (in-list ordered-srcs)])
            (define alter-descriptors : CSS-Media-Features (vector-ref src 2))
            (if (eq? alter-descriptors top-descriptors)
                (css-cascade-declarations desc-parsers (in-value (vector-ref src 1)) css:ident-norm descbase)
                (call-with-css-viewport-from-media #:descriptors alter-descriptors
                  (css-cascade-declarations desc-parsers (in-value (vector-ref src 1)) css:ident-norm descbase))))))
    descbase))

(define css-cascade-declarations : (->* (CSS-Declaration-Parsers (Sequenceof (Listof CSS-Declaration)))
                                        ((-> CSS:Ident Symbol) CSS-Values) CSS-Values)
  ;;; https://drafts.csswg.org/css-cascade/#shorthand
  ;;; https://drafts.csswg.org/css-cascade/#importance
  ;;; https://drafts.csswg.org/css-variables/#syntax
  ;;; https://drafts.csswg.org/css-variables/#using-variables
  (let ()
    (define desc-set! : (-> CSS-Values Symbol Boolean (-> Any) Void)
      (lambda [descbase desc-name important? declared-value]
        (when important? (hash-set! !importants desc-name #true))
        (when (eq? desc-name 'all)
          (for ([desc-key (in-list (remq* (default-css-all-exceptions) (hash-keys descbase)))])
            (when (desc-more-important? desc-key important?)
              (hash-set! descbase desc-key declared-value))))
        (hash-set! descbase desc-name declared-value)))
    (define parse-long : (-> CSS-Values CSS-Shorthand+Parser CSS:Ident (Listof+ CSS-Token) Boolean Boolean Void)
      (lambda [descbase info <desc-name> declared-values important? lazy?]
        (define css-parse : CSS-Shorthand-Parser (CSS<+> (car info) (CSS:<^> (<css-wide-keywords>) (cdr info))))
        (cond [(and lazy?)
               (define pending-thunk : (-> (Option CSS-Shorthand-Datum))
                 (λ [] (let ([flat-values (css-variable-substitute <desc-name> declared-values (css-varbase-ref descbase) null)])
                         (and (css-pair? flat-values)
                              (do-parse <desc-name> css-parse flat-values css-longhand #false)))))
               (define &pending-longhand : (Boxof (-> (Option CSS-Shorthand-Datum))) (box pending-thunk))
               (for ([name (in-list (cdr info))] #:when (desc-more-important? name important?))
                 (desc-set! descbase name important?
                            (λ [] (let ([longhand ((unbox &pending-longhand))])
                                     (box-cas! &pending-longhand pending-thunk (λ [] longhand))
                                     (let ([desc-value (if (hash? longhand) (hash-ref longhand name (λ [] css:initial)) css:unset)])
                                       (hash-set! descbase name (λ [] desc-value))
                                       desc-value)))))]
              [(do-parse <desc-name> css-parse declared-values css-longhand #false)
               => (λ [longhand] (for ([p (in-list longhand)])
                                  (desc-set! descbase (car p) important? (λ [] (cdr p)))))])))
    (define parse-desc : (-> CSS-Values (CSS-Parser (Listof Any)) CSS:Ident (Listof+ CSS-Token) Symbol Boolean Boolean Void)
      (lambda [descbase parser <desc-name> declared-values desc-name important? lazy?]
        (define css-parse : (CSS-Parser (Listof Any)) (CSS<+> parser (CSS:<^> (<css-wide-keywords>))))
        (cond [(and lazy?)
               (desc-set! descbase desc-name important?
                          (λ [] (let* ([flat (css-variable-substitute <desc-name> declared-values (css-varbase-ref descbase) null)]
                                       [desc-value (cond [(not (css-pair? flat)) css:unset]
                                                         [else ((inst do-parse (Listof Any) CSS-Wide-Keyword)
                                                                <desc-name> css-parse flat null css:unset reverse)])])
                                  (hash-set! descbase desc-name (λ [] desc-value)) ; self replace
                                  desc-value)))]
              [((inst do-parse (Listof Any) False) <desc-name> css-parse declared-values null #false reverse)
               => (λ [desc-value] (desc-set! descbase desc-name important? (λ [] desc-value)))])))
    (define filter-desc : (-> CSS-Values (CSS:Filter Any) CSS:Ident (Listof+ CSS-Token) Symbol Boolean Boolean Void)
      (lambda [descbase raw-filter <desc-name> declared-values desc-name important? lazy?]
        (cond [(and lazy?)
               (desc-set! descbase desc-name important?
                          (λ [] (let* ([flat (css-variable-substitute <desc-name> declared-values (css-varbase-ref descbase) null)]
                                       [desc-value (if (css-pair? flat) (do-filter <desc-name> raw-filter flat css:unset) css:unset)])
                                  (hash-set! descbase desc-name (λ [] desc-value)) ; self replace
                                  desc-value)))]
              [(do-filter <desc-name> raw-filter declared-values #false)
               => (λ [desc-value] (desc-set! descbase desc-name important? (λ [] desc-value)))])))
    (lambda [desc-parsers sequences [css:ident->datum css:ident-norm] [descbase (make-css-values)]]
      (for* ([properties sequences]
             [property (in-list properties)])
        (define <desc-name> : CSS:Ident (css-declaration-name property))
        (define desc-name : Symbol (css:ident->datum <desc-name>))
        (cond [(symbol-unreadable? desc-name) (varbase-set! descbase desc-name property)]
              [else (let ([important? : Boolean (css-declaration-important? property)])
                      (when (desc-more-important? desc-name important?)
                        (define declared-values : (Listof+ CSS-Token) (css-declaration-values property))
                        (define lazy? : Boolean (css-declaration-lazy? property))
                        (define info : CSS-Declaration-Parser
                          (desc-parsers desc-name (λ [] (void (make+exn:css:deprecated <desc-name>)))))
                        (cond [(css-filter? info) (filter-desc descbase info <desc-name> declared-values desc-name important? lazy?)]
                              [(css-parser? info) (parse-desc descbase info <desc-name> declared-values desc-name important? lazy?)]
                              [(pair? info) (parse-long descbase info <desc-name> declared-values important? lazy?)]
                              [(css-warn-unknown-property?) (make+exn:css:unrecognized <desc-name>)])))]))
      descbase)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define css-cascade-viewport : (->* (CSS-Media-Features (Vectorof (Listof CSS-Declaration))) (CSS-Declaration-Parsers CSS-Viewport-Filter)
                                    CSS-Media-Features)
  ;;; https://drafts.csswg.org/css-device-adapt/#atviewport-rule
  (lambda [init-viewport descriptors [viewport-parser (default-css-viewport-parsers)] [viewport-filter (default-css-viewport-filter)]]
    (cond [(zero? (vector-length descriptors)) init-viewport]
          [else (call-with-css-viewport-from-media #:descriptors init-viewport
                  (viewport-filter (css-cascade-declarations viewport-parser (in-vector descriptors) css:ident-norm)
                                   #false init-viewport))])))

(define css-select-styles : (->* ((Listof CSS-Grammar-Rule) (ListofU+ CSS-Subject) CSS-Declaration-Parsers) (CSS-Media-Features)
                                 (Values (Listof CSS-Style-Metadata) Boolean))
  ;;; https://drafts.csswg.org/selectors/#subject-of-a-selector
  ;;; https://drafts.csswg.org/selectors/#data-model
  (lambda [rules stcejbus decl-parsers [top-descriptors (default-css-media-features)]]
    (define-values (selected-styles single-query?)
      (let cascade-rules : (Values (Listof CSS-Style-Metadata) Boolean)
        ([descriptors : CSS-Media-Features top-descriptors]
         [grammars : (Listof CSS-Grammar-Rule) rules]
         [stylebase : (Listof CSS-Style-Metadata) null]
         [single? : Boolean #true])
        (for/fold ([styles stylebase] [single-query? single?])
                  ([rule (in-list grammars)])
          (cond [(css-style-rule? rule)
                 (define selectors : (Listof+ CSS-Complex-Selector) (css-style-rule-selectors rule))
                 (define specificity : Natural
                   (for/fold ([max-specificity : Natural 0]) ([selector (in-list selectors)])
                     (define matched-specificity : Natural (or (css-selector-match selector stcejbus) 0))
                     (fxmax matched-specificity max-specificity)))
                 (cond [(zero? specificity) (values styles single-query?)]
                       [else (let ([sm : CSS-Style-Metadata (vector specificity (css-style-rule-properties rule) descriptors)])
                               (values (cons sm styles) single-query?))])]
                [(and (css-@media-rule? rule) (css-@media-okay? (css-@media-rule-queries rule) descriptors))
                 (cascade-rules (cond [(null? (css-@media-rule-viewports rule)) descriptors]
                                      [else (css-cascade-viewport descriptors (css-@media-rule-viewports rule))])
                                (css-@media-rule-grammars rule)
                                styles
                                (and (null? (css-@media-rule-viewports rule)) single-query?))]
                [(and (css-@supports-rule? rule)
                      (css-@supports-okay? (css-@supports-rule-query rule)
                                           decl-parsers
                                           (default-css-feature-support?)))
                 (cascade-rules descriptors (css-@supports-rule-grammars rule) styles single-query?)]
                [else #|other `css-@rule`s|# (values styles single-query?)]))))
    (values (sort (reverse selected-styles) #| `reverse` guarantees orginal order |#
                  (λ [[sm1 : CSS-Style-Metadata] [sm2 : CSS-Style-Metadata]]
                    (fx< (vector-ref sm1 0) (vector-ref sm2 0))))
            single-query?)))

(define css-select-children : (-> CSS-Stylesheet CSS-Declaration-Parsers (Listof CSS-Stylesheet))
  (lambda [parent decl-parsers]
    (define pool : CSS-Stylesheet-Pool (css-stylesheet-pool parent))
    (define (okay? [child : CSS-@Import-Rule]) : Boolean
      (define query : (Option CSS-Feature-Query) (css-@import-rule-supports child))
      (and (implies query (css-@supports-okay? query decl-parsers (default-css-feature-support?)))
           (css-@media-okay? (css-@import-rule-media-list child) (default-css-media-features))))
    (for/list : (Listof CSS-Stylesheet) ([child (in-list (css-stylesheet-imports parent))] #:when (okay? child))
      (hash-ref pool (css-@import-rule-identity child)))))

(define css-@supports-okay? : (-> CSS-Feature-Query CSS-Declaration-Parsers CSS-@Supports-Okay? Boolean)
  ;;; https://drafts.csswg.org/css-conditional/#at-supports
  (lambda [query decl-parsers support?]
    (define (okay? [query : (U CSS-Media-Query CSS-Feature-Query)]) : Boolean
      (and (css-declaration? query)
           (let* ([<desc-name> : CSS:Ident (css-declaration-name query)]
                  [decl-values : (Listof+ CSS-Token) (css-declaration-values query)])
             (define info (decl-parsers (css:ident-norm <desc-name>) void))
             (define desc-values ;;; TODO: what if variables exist in @supports query?
               (cond [(css-filter? info) (do-filter <desc-name> info decl-values #false)]
                     [(css-parser? info) ((inst do-parse (Listof Any) False) <desc-name> info decl-values null #false reverse)]
                     [(pair? info) (do-parse <desc-name> (car info) decl-values css-longhand #false)]
                     [(css-warn-unknown-feature?) (make+exn:css:unrecognized <desc-name>)]))
             (and desc-values (support? (css:ident-norm <desc-name>) desc-values)))))
    (css-condition-okay? query okay?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define !importants : (HashTable Symbol Boolean) ((inst make-hasheq Symbol Boolean)))

(define desc-more-important? : (-> Symbol Boolean Boolean)
  (lambda [desc-name important?]
    (or important? (not (hash-has-key? !importants desc-name)))))

(define do-filter : (All (a b) (-> CSS:Ident (CSS:Filter a) (Listof+ CSS-Token) b (U a b CSS-Wide-Keyword)))
  (lambda [<desc-name> css-filter declared-values unset]
    (define single-value? : Boolean (null? (cdr declared-values)))
    (define maybe-value : (CSS-Option a) (and single-value? (css-filter (car declared-values))))
    (cond [(not single-value?) (make+exn:css:overconsumption (cdr declared-values) <desc-name>) unset]
          [(nor (not maybe-value) (exn:css? maybe-value)) maybe-value]
          [else (let ([maybe-wide ((<css-wide-keywords>) (car declared-values))])
                  (cond [(css-wide-keyword? maybe-wide) maybe-wide]
                        [(exn:css? maybe-value) (css-log-syntax-error maybe-value <desc-name>) unset]
                        [(exn:css? maybe-wide) (css-log-syntax-error maybe-wide <desc-name>) unset]
                        [else (make+exn:css:type (car declared-values) <desc-name>) unset]))])))

(define do-parse : (All (a b) (->* (CSS:Ident (CSS-Parser a) (Listof+ CSS-Token) a b) ((-> a a)) (U a b)))
  (lambda [<desc-name> parse declared-values initial unset [normalize values]]
    (define-values (maybe-value tail) (parse initial declared-values))
    (cond [(false? maybe-value) ((if (null? tail) make+exn:css:missing-value make+exn:css:type) tail <desc-name>) unset]
          [(exn:css? maybe-value) (css-log-syntax-error maybe-value <desc-name>) unset]
          [(pair? tail) (make+exn:css:overconsumption tail <desc-name>) unset]
          [else (normalize maybe-value)])))
