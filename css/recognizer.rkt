#lang typed/racket

;;; Parser Combinators and Syntax Sugars of dealing with declarations are designed for client applications
;;; WARNING: Notations are not following the CSS Specification(https://drafts.csswg.org/css-values/#component-combinators)

(provide (all-defined-out))
(provide nonnegative-flonum? nonnegative-fixnum? positive-flonum? positive-fixnum? positive-index? positive-byte?)

(require racket/keyword)

(require digimon/number)

(require "digitama/syntax/misc.rkt")
(require "digitama/syntax/digicore.rkt")
(require "digitama/syntax/dimension.rkt")

(require (for-syntax racket/syntax))
(require (for-syntax syntax/parse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (define-css-disjoint-filter stx)
  (syntax-case stx [:]
    [(_ compound-filter #:-> RangeType #:with [[dom : DomType defval ...] ...] atom-filters ...)
     (syntax/loc stx (define (compound-filter [dom : DomType defval ...] ...) : (CSS:Filter RangeType) (CSS:<+> atom-filters ...)))]
    [(_ compound-filter #:-> RangeType atom-filters ...)
     (syntax/loc stx (define (compound-filter) : (CSS:Filter RangeType) (CSS:<+> atom-filters ...)))]))
  
(define-syntax (define-css-atomic-filter stx)
  (syntax-case stx [:]
    [(_ atom-filter #:-> RangeType #:with [[token : css:token?] [dom : DomType defval ...] ...] atom-body ... #:where [defaux ...])
     (syntax/loc stx
       (define (atom-filter [dom : DomType defval ...] ...) : (CSS:Filter RangeType) defaux ...
         (λ [[token : CSS-Syntax-Any]] : (CSS-Option RangeType)
           (cond [(css:token? token) atom-body ...]
                 [else #false]))))]
    [(defilter atom-filter #:-> RangeType #:with [[token : css:token?] [dom : DomType defval ...] ...] atom-body ...)
     (syntax/loc stx (defilter atom-filter #:-> RangeType #:with [[token : css:token?] [dom : DomType defval ...] ...] atom-body ... #:where []))]))

(define-syntax (define-css-function-filter stx)
  (define (parse-pattern <constructor> <matches>)
    (define-values (snerttap stnemugra)
      (for/fold ([snrettap null] [stnemugra null])
                ([<pattern> (in-list <matches>)])
        (syntax-case <pattern> [: ? _ quote]
          [(field ? type?) (values (cons #'(? type? field) snrettap) (cons #'field stnemugra))]
          [(field ? type? ...) (values (cons #'(? (λ [v] (or (type? v) ...)) field) snrettap) (cons #'field stnemugra))]
          [(field : Type) (values (cons #'(? (make-predicate Type) field) snrettap) (cons #'field stnemugra))]
          [(field : Type ...) (values (cons #'(? (make-predicate (U Type ...)) field) snrettap) (cons #'field stnemugra))]
          ;[,field (values (cons #'field snrettap) (cons #'field stnemugra))]
          [_ (let ([? (datum->syntax #'_ (gensym))]) (values (cons ? snrettap) (cons ? stnemugra)))]
          [field (values snrettap (cons #'field stnemugra))])))
    (list (list* #'list #'_ (reverse snerttap)) (cons <constructor> (reverse stnemugra))))
  (syntax-parse stx
    [(self func-filter #:-> RangeType
           [(fname aliases ...) #:=> [transforms ...] (~optional #:<+>) fparser ...] ...
           (~optional (~seq #:where [defaux ...])))
     (with-syntax ([defines (if (attribute defaux) #'(begin defaux ...) #'(void))]
                   [((([pattern ...] [transform ...]) ...) ...)
                    (for/list ([<function> (in-list (syntax->list #'(fname ...)))]
                               [<transforms> (in-list (syntax->list #'([transforms ...] ...)))])
                      (define transforms (syntax-e <transforms>))
                      (when (null? transforms) (raise-syntax-error (syntax-e #'self) "empty value transformation" <function>))
                      (for/list ([<transform> (in-list transforms)])
                        (define transform (syntax-e <transform>))
                        (cond [(pair? transform) (parse-pattern (car transform) (cdr transform))]
                              [(null? transform) (raise-syntax-error (syntax-e #'self) "missing value constructor" <transform>)]
                              [else (let ([? (datum->syntax <transform> (gensym))])
                                      (list (list #'? #'list? ?) (list #'values ?)))])))])
       (syntax/loc stx
         (define (func-filter) : (CSS:Filter RangeType) defines
           (define do-parse : (-> Symbol (CSS-Parser (Listof Any)) (Listof CSS-Token) (U (Listof Any) CSS-Syntax-Error))
             (lambda [func-name func-parse func-argl]
               (define-values (fargs --tokens) (func-parse (list func-name) func-argl))
               (cond [(exn:css? fargs) fargs]
                     [(false? fargs) (make-exn:css:type --tokens)]
                     [(pair? --tokens) (make-exn:css:overconsumption --tokens)]
                     [else (reverse fargs)])))
           (λ [[token : CSS-Syntax-Any]] : (CSS-Option RangeType)
             (and (css:function? token)
                  (let ([argl : (Listof CSS-Token) (css:function-arguments token)])
                    (case (css:function-norm token)
                      [(fname aliases ...)
                       (match (do-parse 'fname (CSS<+> fparser ...) argl)
                         [(pattern ...) (transform ...)] ...
                         [(? exn? e) e]
                         [_ (make-exn:css:arity token)])]
                      ...
                      [else (make-exn:css:range token)])))))))]))

(define-syntax (define-css-prefab-filter stx)
  (syntax-parse stx
    [(_ <id> #:-> RangeType #:format fmt:str [css default-racket] ... [otherwise last-one])
     (with-syntax ([(current ... current-last)
                    (for/list ([<p> (in-list (syntax->list #'(css ... otherwise)))])
                      (format-id <p> (syntax-e #'fmt) (syntax-e <p>)))])
       (syntax/loc stx
         (begin (define-css-parameter current : RangeType #:= default-racket) ...
                (define-css-parameter current-last : RangeType #:= last-one)
                (define-css-disjoint-filter <id> #:-> RangeType
                  (CSS:<~> (<css:ident-norm> (list 'css ... 'otherwise))
                           (λ [[id : Symbol]] : RangeType
                             (case id [(css) (current)] ... [else (current-last)])))))))]))

(define-syntax (CSS<?> stx)
  (syntax-case stx [else]
    [(_) #'values]
    [(_ [else <else> ...]) (syntax/loc stx (CSS<&> <else> ...))]
    [(_ [<if> <then> ...]) (syntax/loc stx (css:if <if> (CSS<&> <then> ...) #false))]
    [(_ [<if> <then> ...] [else <else> ...]) (syntax/loc stx (css:if <if> (CSS<&> <then> ...) (CSS<&> <else> ...)))]
    [(_ [<if> <then> ...] ... [else <else> ...]) (syntax/loc stx (css:if (list (cons <if> (CSS<&> <then> ...)) ...) (CSS<&> <else> ...)))]
    [(_ [<if> <then> ...] ...) (syntax/loc stx (css:if (list (cons <if> (CSS<&> <then> ...)) ...) #false))]))
  
(define-syntax (CSS:<+> stx)
  (syntax-case stx []
    [(_ css-filter) #'css-filter]
    [(_ css-filter css-filter2) (syntax/loc stx (css:disjoin css-filter css-filter2))]
    [(_ css-filter css-filter2 css-filters ...) (syntax/loc stx (css:disjoin css-filter css-filter2 (CSS:<+> css-filters ...)))]))
  
(define-syntax (CSS:<~> stx)
  (syntax-case stx []
    [(_ css-filter f) (syntax/loc stx (css:compose css-filter f))]
    [(_ css-filter f g) (syntax/loc stx (css:compose (css:compose css-filter g) f))]
    [(_ css-filter f g h ...) (syntax/loc stx (css:compose css-filter (compose f g h ...)))]))

(define CSS:<=> : (All (a b) (-> (CSS:Filter a) b (CSS:Filter b)))
  (lambda [css-filter const]
    (λ [[token : CSS-Syntax-Any]]
      (define datum : (CSS-Option a) (css-filter token))
      (if (exn:css? datum) datum (and datum const)))))

(define CSS:<?> : (All (a b c) (case-> [(CSS:Filter a) (-> (U CSS-Syntax-Any (Listof CSS-Token)) CSS-Syntax-Error) -> (CSS:Filter a)]
                                       [(CSS:Filter a) b (-> CSS-Syntax-Error c) -> (CSS:Filter (U a b c))]))
  (case-lambda
    [(css:filter make-exn)
     (λ [[token : CSS-Syntax-Any]]
       (define datum : (CSS-Option a) (css:filter token))
       (cond [(or (false? datum) (exn:css? datum)) (make-exn token)]
             [else datum]))]
    [(css:filter false-value fexn-value)
     (λ [[token : CSS-Syntax-Any]]
       (define datum : (CSS-Option a) (css:filter token))
       (cond [(false? datum) false-value]
             [(exn:css? datum) (or (fexn-value datum) datum)]
             [else datum]))]))

(define CSS:<$> : (-> (CSS:Filter Any) Symbol (-> (HashTable Symbol Any) Any) CSS-Shorthand-Parser)
  (lambda [css:filter tag fdatum]
    (λ [[data : (HashTable Symbol Any)] [tokens : (Listof CSS-Token)]]
      (cond [(pair? tokens) (let ([css-parser (CSS:<^> css:filter tag)]) (css-parser data tokens))]
            [else (values (hash-set data tag (fdatum data)) null)]))))

(define CSS:<^> : (All (a) (case-> [(U (CSS:Filter Any) (Listof+ (CSS:Filter Any))) -> (CSS-Parser (Listof Any))]
                                   [(CSS:Filter a) (->* (a) ((HashTable Symbol Any)) (HashTable Symbol Any)) -> CSS-Shorthand-Parser]
                                   [(CSS:Filter Any) (U Symbol (Listof+ Symbol)) -> CSS-Shorthand-Parser]
                                   [(CSS:Filter Any) (U Symbol (Listof+ Symbol)) CSS-Longhand-Update -> CSS-Shorthand-Parser]))
  (case-lambda
    [(atom-filter)
     (if (pair? atom-filter)
         (apply CSS:<&> atom-filter)
         (CSS:<&> atom-filter))]
    [(atom-filter tag)
     (if (procedure? tag)
         (λ [[data : (HashTable Symbol Any)] [tokens : (Listof CSS-Token)]]
           (define-values (token --tokens) (css-car/cdr tokens))
           (define datum (atom-filter token))
           (cond [(or (false? datum) (exn:css? datum)) (values datum tokens)]
                 [else (values (tag datum data) --tokens)]))
         (λ [[data : (HashTable Symbol Any)] [tokens : (Listof CSS-Token)]]
           (define-values (token --tokens) (css-car/cdr tokens))
           (define datum : (CSS-Option Any) (atom-filter token))
           (cond [(or (false? datum) (exn:css? datum)) (values datum tokens)]
                 [(symbol? tag) (values (hash-set data tag datum) --tokens)]
                 [else (values (hash-set++ data tag datum) --tokens)])))]
    [(atom-filter tag updater)
     (λ [[data : (HashTable Symbol Any)] [tokens : (Listof CSS-Token)]]
       (define-values (token --tokens) (css-car/cdr tokens))
       (define datum : (CSS-Option Any) (atom-filter token))
       (cond [(or (false? datum) (exn:css? datum)) (values datum tokens)]
             [(symbol? tag) (values (hash-update data tag (λ [[v : Any]] (updater tag v datum)) (thunk #false)) --tokens)]
             [else (values (hash-update++ data tag datum updater) --tokens)]))]))

(define CSS:<&> : (-> (CSS:Filter Any) * (CSS-Parser (Listof Any)))
  (case-lambda
    [() values]
    [(atom-filter)
     (λ [[data : (Listof Any)] [tokens : (Listof CSS-Token)]]
       (define-values (head tail) (css-car/cdr tokens))
       (define datum : (CSS-Option Any) (atom-filter head))
       (cond [(or (false? datum) (exn:css? datum)) (values datum tokens)]
             [else (values (cons datum data) tail)]))]
    [atom-filters
     (λ [[data : (Listof Any)] [tokens : (Listof CSS-Token)]]
       (let datum-fold ([data++ : (Listof Any) data]
                        [tokens-- : (Listof CSS-Token) tokens]
                        [filters : (Listof (CSS:Filter Any)) atom-filters])
         (cond [(null? filters) (values data++ tokens--)]
               [else (let ([css-filter (car filters)])
                       (define-values (token --tokens) (css-car/cdr tokens--))
                       (define datum : (CSS-Option Any) (css-filter token))
                       (cond [(or (false? datum) (exn:css? datum)) (values datum tokens--)]
                             [else (datum-fold (cons datum data++) --tokens (cdr filters))]))])))]))

(define CSS:<&&> : (-> (CSS:Filter Any) * (CSS-Parser (Listof Any)))
  ;;; https://drafts.csswg.org/css-values/#comb-all
  (case-lambda
    [() values]
    [(atom-filter) (CSS:<&> atom-filter)]
    [atom-filters
     (λ [[data : (Listof Any)] [tokens : (Listof CSS-Token)]]
       (let datum-fold ([data++ : (Listof Any) data]
                        [tokens-- : (Listof CSS-Token) tokens]
                        [filters : (Listof (CSS:Filter Any)) atom-filters])
         (cond [(null? filters) (values data++ tokens--)]
               [else (let ([css-filter (car filters)])
                       (define-values (token --tokens) (css-car/cdr tokens--))
                       (define datum : (CSS-Option Any) (css-filter token))
                       (cond [(nor (false? datum) (exn:css? datum)) (datum-fold (cons datum data++) --tokens (cdr filters))]
                             [else (let no-order-fold ([rest : (Listof (CSS:Filter Any)) (cdr filters)]
                                                       [sresol : (Listof (CSS:Filter Any)) null])
                                     (cond [(null? rest) (values datum tokens--)]
                                           [else (let ([sub-filter (car rest)])
                                                   (define subdatum : (CSS-Option Any) (sub-filter token))
                                                   (cond [(or (false? subdatum) (exn:css? subdatum)) (no-order-fold (cdr rest) (cons sub-filter sresol))]
                                                         [else (datum-fold (cons subdatum data++) --tokens
                                                                           (cons css-filter
                                                                                 (append (reverse sresol)
                                                                                         (cdr rest))))]))]))]))])))]))

(define CSS:<*> : (All (a) (->* ((CSS:Filter Any)) ((U (CSS-Multiplier Index) '+ '? '*)) (CSS-Parser (Listof Any))))
  ;;; https://drafts.csswg.org/css-values/#mult-zero-plus
  ;;; https://drafts.csswg.org/css-values/#comb-any
  (lambda [atom-filter [multiplier '*]]
    (define-values (least most) (css:multiplier-range multiplier 0))
    (cond [(zero? most) values]
          [else (λ [[data : (Listof Any)] [tokens : (Listof CSS-Token)]]
                  (let seq ([data++ : (Listof Any) data]
                            [tokens-- : (Listof CSS-Token) tokens]
                            [n+1 : Natural 1])
                    (define-values (token --tokens) (css-car/cdr tokens--))
                    (define datum : (CSS-Option Any) (atom-filter token))
                    (cond [(or (false? datum) (exn:css? datum)) (values (if (< least n+1) data++ datum) tokens--)]
                          [(= n+1 most) (values (cons datum data++) --tokens)] ; (= n +inf.0) also does not make much sense
                          [else (seq (cons datum data++) --tokens (add1 n+1))])))])))

(define CSS:<#> : (->* ((CSS:Filter Any)) ((U (CSS-Multiplier Positive-Index) '+)) (CSS-Parser (Listof Any)))
  ;;; https://drafts.csswg.org/css-values/#mult-comma
  (lambda [atom-filter [multiplier '+]]
    (define-values (least most) (css:multiplier-range multiplier 1))
    (λ [[data : (Listof Any)] [tokens : (Listof CSS-Token)]]
      (let seq ([data++ : (Listof Any) data]
                [tokens-- : (Listof CSS-Token) tokens]
                [n+1 : Natural 1])
        (define-values (token tail) (css-car/cdr tokens--))
        (define datum : (CSS-Option Any) (atom-filter token))
        (cond [(or (false? datum) (exn:css? datum)) (values (if (< least n+1) data++ datum) tail)]
              [(= n+1 most) (values (cons datum data++) tail)]
              [else (let-values ([(?comma --tokens) (css-car/cdr tail)])
                      (cond [(eof-object? ?comma) (seq (cons datum data++) --tokens (add1 n+1))] ; to check least boundry
                            [(not (css:comma? ?comma)) (values (make-exn:css:missing-comma ?comma) --tokens)]
                            [(null? --tokens) (values (make-exn:css:missing-value ?comma) --tokens)]
                            [else (seq (cons datum data++) --tokens (add1 n+1))]))])))))

(define CSS:<!> : (->* ((CSS:Filter Any)) ((U (CSS-Multiplier Positive-Index) '+)) (CSS-Parser (Listof Any)))
  ;;; (WARNING: this is *not*) https://drafts.csswg.org/css-values/#mult-req
  (lambda [atom-filter [multiplier '+]]
    (define-values (least most) (css:multiplier-range multiplier 1))
    (λ [[data : (Listof Any)] [tokens : (Listof CSS-Token)]]
      (let seq ([sub++ : (Listof Any) null]
                [tokens-- : (Listof CSS-Token) tokens]
                [n+1 : Natural 1])
        (define-values (token --tokens) (css-car/cdr tokens--))
        (define datum : (CSS-Option Any) (atom-filter token))
        (if (or (false? datum) (exn:css? datum))
            (cond [(< least n+1) (values (cons (reverse sub++) data) tokens--)]
                  [else (values datum --tokens)])
            (cond [(= n+1 most) (values (cons (reverse (cons datum sub++)) data) --tokens)]
                  [else (seq (cons datum sub++) --tokens (add1 n+1))]))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define CSS<^> : (case-> [(CSS-Parser (Listof Any)) (U Symbol (Listof+ Symbol)) -> CSS-Shorthand-Parser]
                         [(CSS-Parser (Listof Any)) (U Symbol (Listof+ Symbol)) CSS-Longhand-Update -> CSS-Shorthand-Parser])
  (case-lambda
    [(atom-parser tag)
     (λ [[data : (HashTable Symbol Any)] [tokens : (Listof CSS-Token)]]
       (define-values (subdata --tokens) (atom-parser null tokens))
       (cond [(or (false? subdata) (exn:css? subdata)) (values subdata --tokens)]
             [(symbol? tag) (values (hash-set data tag (reverse subdata)) --tokens)]
             [else (values (hash-set++ data tag (reverse subdata)) --tokens)]))]
    [(atom-parser tag updater)
     (λ [[data : (HashTable Symbol Any)] [tokens : (Listof CSS-Token)]]
       (define-values (subdata --tokens) (atom-parser null tokens))
       (cond [(or (false? subdata) (exn:css? subdata)) (values subdata --tokens)]
             [(list? tag) (values (hash-update++ data tag (reverse subdata) updater) --tokens)]
             [else (values (hash-update data tag (λ [[v : Any]] (updater tag v (reverse subdata))) (thunk #false)) --tokens)]))]))

(define CSS<$> : (case-> [(CSS-Parser (Listof Any)) -> (CSS-Parser (Listof Any))]
                         [(CSS-Parser (Listof Any)) Any -> (CSS-Parser (Listof Any))]
                         [(CSS-Parser (Listof Any)) Symbol (-> (HashTable Symbol Any) Any) -> CSS-Shorthand-Parser])
  (case-lambda
    [(css-parser)
     (λ [[data : (Listof Any)] [tokens : (Listof CSS-Token)]]
       (cond [(pair? tokens) (css-parser data tokens)]
             [else (values data null)]))]
    [(css-parser eof-value)
     (λ [[data : (Listof Any)] [tokens : (Listof CSS-Token)]]
       (cond [(pair? tokens) (css-parser data tokens)]
             [else (values (cons eof-value data) null)]))]
    [(css:filter tag fdatum)
     (λ [[data : (HashTable Symbol Any)] [tokens : (Listof CSS-Token)]]
       (cond [(pair? tokens) (let ([css-parser (CSS<^> css:filter tag)]) (css-parser data tokens))]
             [else (values (hash-set data tag (fdatum data)) null)]))]))

(define CSS<~> : (All (a) (-> (CSS-Parser a) (-> a a) (CSS-Parser a)))
  (lambda [css-parser data=>data]
    (λ [[data : a] [tokens : (Listof CSS-Token)]]
      (define-values (++data --tokens) (css-parser data tokens))
      (cond [(or (exn:css? ++data) (false? ++data)) (values ++data --tokens)]
            [else (values (data=>data ++data) --tokens)]))))

(define CSS<_> : (All (a) (-> (CSS-Parser a) (CSS-Parser a)))
  (lambda [css-parser]
    (λ [[data : a] [tokens : (Listof CSS-Token)]]
      (define-values (++data --tokens) (css-parser data tokens))
      (cond [(or (exn:css? ++data) (false? ++data)) (values ++data --tokens)]
            [else (values data --tokens)]))))

(define CSS<+> : (All (a) (-> (CSS-Parser a) (CSS-Parser a) * (CSS-Parser a)))
  ;;; https://drafts.csswg.org/css-values/#comb-one
  (case-lambda
    [(head-branch) head-branch]
    [(head-branch . tail-branches)
     (λ [[data : a] [tokens : (Listof CSS-Token)]]
       (let switch ([head-parser : (CSS-Parser a) head-branch]
                    [tail-parsers : (Listof (CSS-Parser a)) tail-branches])
         (define-values (++data --tokens) (head-parser data tokens))
         (cond [(nor (false? ++data) (exn:css? ++data)) (values ++data --tokens)]
               [(pair? tail-parsers) (switch (car tail-parsers) (cdr tail-parsers))]
               [else (values ++data --tokens)])))]))

(define CSS<&> : (All (a) (-> (CSS-Parser a) * (CSS-Parser a)))
  ;;; https://drafts.csswg.org/css-values-4/#component-combinators [juxtaposing components]
  (case-lambda
    [() values]
    [(css-parser) css-parser]
    [css-parsers
     (λ [[data : a] [tokens : (Listof CSS-Token)]]
       (let datum-fold ([data++ : a data]
                        [tokens-- : (Listof CSS-Token) tokens]
                        [parsers : (Listof (CSS-Parser a)) css-parsers])
         (cond [(null? parsers) (values data++ tokens--)]
               [else (let ([css-parser (car parsers)])
                       (define-values (++data --tokens) (css-parser data++ tokens--))
                       (cond [(nor (false? ++data) (exn:css? ++data)) (datum-fold ++data --tokens (cdr parsers))]
                             [else (values ++data --tokens)]))])))]))

(define CSS<&&> : (All (a) (-> (CSS-Parser a) * (CSS-Parser a)))
  ;;; https://drafts.csswg.org/css-values/#comb-all
  (case-lambda
    [() values]
    [(css-parser) css-parser]
    [css-parsers
     (λ [[data : a] [tokens : (Listof CSS-Token)]]
       (let datum-fold ([data++ : a data]
                        [tokens-- : (Listof CSS-Token) tokens]
                        [parsers : (Listof (CSS-Parser a)) css-parsers])
         (cond [(null? parsers) (values data++ tokens--)]
               [else (let ([css-parser (car parsers)])
                       (define-values (++data --tokens) (css-parser data++ tokens--))
                       (cond [(nor (false? ++data) (exn:css? ++data)) (datum-fold ++data --tokens (cdr parsers))]
                             [else (let no-order-fold ([rest : (Listof (CSS-Parser a)) (cdr parsers)]
                                                       [sresol : (Listof (CSS-Parser a)) null])
                                     (cond [(null? rest) (values ++data --tokens)]
                                           [else (let ([sub-parser (car rest)])
                                                   (define-values (++subdata --subtokens) (sub-parser data++ tokens--))
                                                   (cond [(or (false? ++subdata) (exn:css? ++subdata)) (no-order-fold (cdr rest) (cons sub-parser sresol))]
                                                         [else (datum-fold ++subdata --subtokens
                                                                           (cons css-parser
                                                                                 (append (reverse sresol)
                                                                                         (cdr rest))))]))]))]))])))]))
  
(define CSS<*> : (All (a) (->* ((CSS-Parser a)) ((U (CSS-Multiplier Index) '+ '? '*)) (CSS-Parser a)))
  ;;; https://drafts.csswg.org/css-values/#mult-zero-plus
  (lambda [css-parser [multiplier '*]]
    (define-values (least most) (css:multiplier-range multiplier 0))
    (cond [(zero? most) values]
          [else (λ [[data : a] [tokens : (Listof CSS-Token)]]
                  (let seq ([data++ : a data]
                            [tokens-- : (Listof CSS-Token) tokens]
                            [n+1 : Natural 1])
                    (define-values (++data --tokens) (css-parser data++ tokens--))
                    (cond [(or (false? ++data) (exn:css? ++data)) (if (< least n+1) (values data++ tokens--) (values ++data --tokens))]
                          [(= n+1 most) (values ++data --tokens)] ; (= n +inf.0) also does not make much sense
                          [else (seq ++data --tokens (add1 n+1))])))])))

(define CSS<#> : (All (a) (->* ((CSS-Parser a)) ((U (CSS-Multiplier Positive-Index) '+)) (CSS-Parser a)))
  ;;; https://drafts.csswg.org/css-values/#mult-comma
  (lambda [css-parser [multiplier '+]]
    (define-values (least most) (css:multiplier-range multiplier 1))
    (λ [[data : a] [tokens : (Listof CSS-Token)]]
      (let seq ([data++ : a data]
                [tokens-- : (Listof CSS-Token) tokens]
                [n+1 : Natural 1])
        (define-values (++data tail) (css-parser data++ tokens--))
        (cond [(or (false? ++data) (exn:css? ++data)) (if (< least n+1) (values data++ tokens--) (values ++data tail))]
              [(= n+1 most) (values ++data tail)]
              [else (let-values ([(?comma --tokens) (css-car/cdr tail)])
                      (cond [(eof-object? ?comma) (seq ++data --tokens (add1 n+1))] ; to check least boundry
                            [(not (css:comma? ?comma)) (values (make-exn:css:missing-comma ?comma) --tokens)]
                            [(null? --tokens) (values (make-exn:css:missing-value ?comma) --tokens)]
                            [else (seq ++data --tokens (add1 n+1))]))])))))

(define CSS<!> : (->* ((CSS-Parser (Listof Any))) ((U (CSS-Multiplier Positive-Index) '+)) (CSS-Parser (Listof Any)))
  ;;; (WARNING: this is *not*) https://drafts.csswg.org/css-values/#mult-req
  (lambda [css-parser [multiplier '+]]
    (define-values (least most) (css:multiplier-range multiplier 1))
    (λ [[data : (Listof Any)] [tokens : (Listof CSS-Token)]]
      (let seq ([subdata++ : (Listof Any) null]
                [tokens-- : (Listof CSS-Token) tokens]
                [n+1 : Natural 1])
        (define-values (subdata --tokens) (css-parser subdata++ tokens--))
        (if (or (false? subdata) (exn:css? subdata))
            (cond [(< least n+1) (values (cons (reverse subdata++) data) tokens--)]
                  [else (values subdata --tokens)])
            (cond [(= n+1 most) (values (cons (reverse subdata) data) --tokens)]
                  [else (seq subdata --tokens (add1 n+1))]))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define css:disjoin : (All (a b c) (case-> [(CSS:Filter a) (CSS:Filter b) -> (CSS:Filter (U a b))]
                                           [(CSS:Filter a) (CSS:Filter b) (CSS:Filter c) -> (CSS:Filter (U a b c))]))
  (case-lambda
    [(css-filter1 css-filter2)
     (λ [[token : CSS-Syntax-Any]]
       (define datum : (CSS-Option a) (css-filter1 token))
       (cond [(nor (false? datum) (exn:css? datum)) datum]
             [else (css-filter2 token)]))]
    [(css-filter1 css-filter2 css-filter3)
     (λ [[token : CSS-Syntax-Any]]
       (define datum : (CSS-Option a) (css-filter1 token))
       (cond [(nor (false? datum) (exn:css? datum)) datum]
             [else (let ([datum2 (css-filter2 token)])
                     (cond [(nor (false? datum2) (exn:css? datum2)) datum2]
                           [else (css-filter3 token)]))]))]))
  
(define css:compose : (All (a b) (-> (CSS:Filter a) (-> a b) (CSS:Filter b)))
  (lambda [css-filter css->racket]
    (λ [[token : CSS-Syntax-Any]]
      (define datum : (CSS-Option a) (css-filter token))
      (if (exn:css? datum) datum (and datum (css->racket datum))))))

(define css:if : (All (a) (case-> [(Listof+ (Pairof (CSS:Filter Any) (CSS-Parser a))) (Option (CSS-Parser a)) -> (CSS-Parser a)]
                                  [(CSS:Filter Any) (CSS-Parser a) (Option (CSS-Parser a)) -> (CSS-Parser a)]))
  (case-lambda
    [(cond-parsers else-parser)
     (λ [[data : a] [tokens : (Listof CSS-Token)]]
       (let else-if ([branch (car cond-parsers)]
                     [branches-- (cdr cond-parsers)])
         (define-values (if:filter then-parser) (css-car/cdr branch))
         (define-values (token --tokens) (css-car/cdr tokens))
         (define if:datum : (CSS-Option Any) (if:filter token))
         (cond [(nor (false? if:datum) (exn:css? if:datum)) (then-parser data --tokens)]
               [(pair? branches--) (else-if (car branches--) (cdr branches--))]
               [(false? else-parser) (values if:datum tokens)]
               [else (else-parser data tokens)])))]
    [(if:filter then-parser else-parser)
     (λ [[data : a] [tokens : (Listof CSS-Token)]]
       (define-values (token --tokens) (css-car/cdr tokens))
       (define if:datum : (CSS-Option Any) (if:filter token))
       (cond [(nor (false? if:datum) (exn:css? if:datum)) (then-parser data --tokens)]
             [(false? else-parser) (values if:datum tokens)]
             [else (else-parser data tokens)]))]))

(define css:multiplier-range : (-> (U (CSS-Multiplier Index) '+ '? '*) Index (Values Natural (U Natural +inf.0)))
  (lambda [multiplier least]
    (case multiplier
      [(?) (values 0 1)]
      [(*) (values 0 +inf.0)]
      [(+) (values 1 +inf.0)]
      [else (cond [(index? multiplier) (values (max multiplier least) (max multiplier least))]
                  [else (let ([n (let ([n (car multiplier)]) (if (index? n) (max n least) least))]) 
                          (define m : (U Index Symbol Null) (cdr multiplier))
                          (cond [(index? m) (values n (max n m))]
                                [(symbol? m) (values n +inf.0)]
                                [else (values n n)]))])])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct css-unitless ([value : Flonum]) #:type-name CSS-Unitless) ; for properties whoes computed values are not their used value
(struct css+unitless css-unitless ([value : Nonnegative-Flonum]) #:type-name CSS+Unitless)

(struct css-% ([value : Flonum]) #:type-name CSS-% #:transparent)
(struct css+% css-% ([value : Nonnegative-Flonum]) #:type-name CSS+% #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-css-disjoint-filter <css-boolean> #:-> (U Zero One)
  (CSS:<=> (<css:integer> = 0) 0)
  (CSS:<=> (<css:integer> = 1) 1))

(define-css-disjoint-filter <css-keyword> #:-> Symbol
  #:with [[options : (U (-> Symbol Boolean) (Listof Symbol) Symbol)]]
  (<css:ident-norm> options))

(define-css-disjoint-filter <css-keyword/cs> #:-> Symbol
  #:with [[options : (U (-> Symbol Boolean) (Listof Symbol) Symbol)]]
  (<css:ident> options))

(define <css-hexadecimal> : (All (a) (case-> [(-> Any Boolean : #:+ a) -> (CSS:Filter a)]
                                             [(-> Natural Natural Boolean) Natural -> (CSS:Filter Natural)]
                                             [Natural (-> Natural Natural Boolean) Natural -> (CSS:Filter Natural)]
                                             [(Listof a) -> (CSS:Filter a)]
                                             [-> (CSS:Filter Natural)]))
  (case-lambda
    [()
     (λ [[t : CSS-Syntax-Any]]
       (cond [(not (css:hash? t)) (make-exn:css:type t)]
             [else (let ([n (string->number (keyword->immutable-string (css:hash-datum t)))])
                     (cond [(natural? n) n]
                           [else (make-exn:css:range t)]))]))]
    [(op n)
     (λ [[t : CSS-Syntax-Any]]
       (cond [(not (css:hash? t)) (make-exn:css:type t)]
             [else (let ([N (string->number (keyword->immutable-string (css:hash-datum t)))])
                     (cond [(and (natural? N) (op N n)) N]
                           [else (make-exn:css:range t)]))]))]
    [(l op r)
     (λ [[t : CSS-Syntax-Any]]
       (cond [(not (css:hash? t)) (make-exn:css:type t)]
             [else (let ([N (string->number (keyword->immutable-string (css:hash-datum t)))])
                     (cond [(and (natural? N) (op l N) (op N r)) N]
                           [else (make-exn:css:range t)]))]))]
    [(range?)
     (λ [[t : CSS-Syntax-Any]]
       (cond [(not (css:hash? t)) (make-exn:css:type t)]
             [else (let ([N (string->number (keyword->immutable-string (css:hash-datum t)))])
                     (or (cond [(procedure? range?) (and (range? N) N)]
                               [(list? range?) (let ([ns (member N range?)]) (and ns (car ns)))]
                               [else (and (equal? N range?) N)])
                         (make-exn:css:range t)))]))]))

(define-css-disjoint-filter <css-natural> #:-> Natural
  #:with [[nonzero : (Option '#:nonzero) #false]]
  (cond [nonzero (<css:integer> exact-positive-integer?)]
        [else    (<css:integer> exact-nonnegative-integer?)]))

(define-css-disjoint-filter <css+real> #:-> (U Natural Nonnegative-Flonum)
  #:with [[nonzero : (Option '#:nonzero) #false]]
  (cond [nonzero (CSS:<+> (<css:flonum> positive-flonum?) (<css:integer> exact-positive-integer?))]
        [else    (CSS:<+> (<css:flonum> nonnegative-flonum?) (<css:integer> exact-nonnegative-integer?))]))

(define-css-disjoint-filter <css+%real> #:-> (U Natural Nonnegative-Inexact-Real)
  #:with [[nonzero : (Option '#:nonzero) #false]]
  (cond [nonzero (CSS:<+> (<css:percentage> positive-flonum?)
                          (<css:flonum> positive-flonum?)
                          (<css:integer> exact-positive-integer?))]
        [else    (CSS:<+> (<css:percentage> nonnegative-flonum?)
                          (<css:flonum> nonnegative-flonum?)
                          (<css:integer> exact-nonnegative-integer?))]))

(define-css-disjoint-filter <css-flunit> #:-> Nonnegative-Flonum
  (CSS:<~> (<css:flonum> 0.0 fl<= 1.0) flabs)
  (CSS:<=> (<css:integer> = 0) 0.0)
  (CSS:<=> (<css:integer> = 1) 1.0))

(define-css-disjoint-filter <css-%flunit> #:-> Nonnegative-Flonum
  (CSS:<~> (<css:percentage> 0.0 <= 1.0) flabs)
  (CSS:<~> (<css:flonum> 0.0 fl<= 1.0) flabs)
  (CSS:<=> (<css:integer> = 0) 0.0)
  (CSS:<=> (<css:integer> = 1) 1.0))

(define-css-disjoint-filter <css-angle> #:-> Flonum
  (CSS:<~> (<css:integer>) exact->inexact)
  (<css:flonum>)
  (<css:angle>))

(define <:css-keywords:> : (->* ((Listof Symbol)) (Symbol) (CSS-Parser (Listof Any)))
  (lambda [options [none 'none]]
    (CSS<+> (CSS:<^> (CSS:<=> (<css-keyword> none) null))
            (CSS<*> (CSS:<^> (<css-keyword> options)) '+))))

(define <:css-strings:> : (->* () ((U (CSS-Multiplier Index) '+ '? '*) (-> String Boolean)) (CSS-Parser (Listof Any)))
  (lambda [[multipliers '+] [string-filter (λ _ #true)]]
    (CSS<*> (CSS:<^> (<css:string> string-filter)) multipliers)))

(define (<css-comma>) : (CSS:Filter Char) (CSS:<?> (<css:delim> #\,) make-exn:css:missing-comma))
(define (<css-slash>) : (CSS:Filter Char) (CSS:<?> (<css:delim> #\/) make-exn:css:missing-slash))

(define (<css+unitless>) : (CSS:Filter CSS+Unitless) (CSS:<~> (<css+real>) (λ [[v : Nonnegative-Real]] (let ([flv (real->double-flonum v)]) (css+unitless flv flv)))))
(define (<css-percentage>) : (CSS:Filter CSS-%) (CSS:<~> (<css:percentage>) (λ [[v : Flonum]] (css-% v))))
(define (<css+percentage>) : (CSS:Filter CSS+%) (CSS:<~> (<css:percentage> nonnegative-flonum?) (λ [[v : Nonnegative-Flonum]] (css+% v v))))

(define css-comma-parser : (All (a) (-> (CSS-Parser a) (CSS-Parser a)))
  (lambda [atom-parser]
    (CSS<?> [(<css-comma>) atom-parser]
            [else          atom-parser])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-css-disjoint-filter <css-size> #:-> (U Nonnegative-Inexact-Real CSS:Length:Font)
  (<css+length:font>)
  (CSS:<~> (<css+%real>) exact->inexact))

(define css-make-pair-parser
  : (case-> [(Listof+ (Pairof (CSS:Filter Any) (Listof+ Symbol))) -> CSS-Shorthand+Parser]
            [(Listof+ (CSS:Filter Any)) (Listof+ Symbol) -> CSS-Shorthand+Parser]
            [(CSS:Filter Any) Symbol Symbol -> CSS-Shorthand+Parser]
            [(CSS:Filter Any) Symbol Symbol Symbol Symbol -> CSS-Shorthand+Parser])
  (case-lambda
    [(filters)
     (define-values (parsers names)
       (for/fold ([parsers : (Listof+ CSS-Shorthand-Parser) (list (CSS:<^> (caar filters) (cdar filters)))]
                  [names : (Listof+ Symbol) (cdar filters)])
                 ([fltr+name : (Pairof (CSS:Filter Any) (Listof+ Symbol)) (cdr filters)])
         (values (cons (CSS:<^> (car fltr+name) (cdr fltr+name)) parsers)
                 (append names (cdr fltr+name)))))
     (cons (CSS<*> (apply CSS<+> (car parsers) (cdr parsers)) '+) names)]
    [(filters names)
     (cons (CSS<*> (apply CSS<+> (CSS:<^> (car filters) (car names))
                          (map CSS:<^> (cdr filters) (cdr names))) '+)
           names)]
    [(filter vertical horizontal)
     (cons (CSS<&> (CSS:<^> filter vertical)
                   (CSS:<$> filter horizontal (λ [longhand] (hash-ref longhand vertical))))
           (list vertical horizontal))]
    [(filter top right bottom left)
     (cons (CSS<&> (CSS:<^> filter top)
                   (CSS:<$> filter right (λ [longhand] (hash-ref longhand top)))
                   (CSS:<$> filter bottom (λ [longhand] (hash-ref longhand top)))
                   (CSS:<$> filter left (λ [longhand] (hash-ref longhand right))))
           (list top right bottom left))]))

(define make-css->size : (All (a) (-> a #:100% Nonnegative-Flonum (CSS->Racket (U a Nonnegative-Flonum))))
  (lambda [defval #:100% fl%]
    (λ [property datum]
      (cond [(nonnegative-flonum? datum) datum]
            [(css+%? datum) (fl* (css+%-value datum) fl%)]
            [(css:length? datum) (css:length->scalar datum #false)]
            [else defval]))))

(define make-css->pixels : (All (a b) (-> (-> Any Boolean : #:+ a) b #:100% Nonnegative-Real [#:size->pixels (-> Real Integer)]
                                          (CSS->Racket (U a b))))
  (lambda [pixels? defval #:100% fl% #:size->pixels [-> exact-round]]
    (λ [property datum]
      (define size : Integer
        (cond [(flonum? datum) (-> datum)]
              [(css-%? datum) (-> (* (css-%-value datum) fl%))]
              [(css:length? datum) (-> (css:length->scalar datum #false))]
              [else -1]))
      (if (pixels? size) size defval))))
