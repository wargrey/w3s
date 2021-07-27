#lang typed/racket

;;; Parser Combinators and Syntax Sugars of dealing with declarations are designed for client applications
;;; WARNING: Notations might not follow the CSS Specification(https://drafts.csswg.org/css-values/#component-combinators)

;;; TODO: If we can make the type annotation more precisely, and if it is worth doing.

(provide (all-defined-out))
(provide nonnegative-flonum? nonnegative-fixnum? positive-flonum? positive-fixnum? positive-index? positive-byte?)

(require racket/keyword)

(require digimon/number)
(require digimon/predicate)

(require bitmap/digitama/unsafe/image)

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
  (syntax-parse stx #:datum-literals [:]
    [(_ atom-filter #:-> RangeType #:with [[token : token?] [dom : DomType defval ...] ...]
        (~optional (~seq #:on-error make-exn) #:defaults ([make-exn #'#false]))
        atom-body ...
        #:where [defaux ...])
     (syntax/loc stx
       (define (atom-filter [dom : DomType defval ...] ...) : (CSS:Filter RangeType) defaux ...
         (λ [[token : CSS-Syntax-Any]] : (CSS-Option RangeType)
           (cond [(token? token) atom-body ...]
                 [else (and make-exn (make-exn token))]))))]
    [(defilter atom-filter #:-> RangeType #:with [[token : token?] [dom : DomType defval ...] ...] atom-body ...)
     (syntax/loc stx (defilter atom-filter #:-> RangeType #:with [[token : token?] [dom : DomType defval ...] ...] atom-body ... #:where []))]))

(define-syntax (define-css-function-filter stx)
  (define (parse-pattern <constructor> <matches>)
    (define-values (snerttap stnemugra)
      (for/fold ([snrettap null] [stnemugra null])
                ([<pattern> (in-list <matches>)])
        (syntax-case <pattern> [: ? ?* _ * quote]
          [(field ? type?) (values (cons #'(? type? field) snrettap) (cons #'field stnemugra))]
          [(field ? type? ...) (values (cons #'(? (λ [v] (or (type? v) ...)) field) snrettap) (cons #'field stnemugra))]
          [(field : Type) (values (cons #'(? (make-predicate Type) field) snrettap) (cons #'field stnemugra))]
          [(field : Type ...) (values (cons #'(? (make-predicate (U Type ...)) field) snrettap) (cons #'field stnemugra))]
          ;[,field (values (cons #'field snrettap) (cons #'field stnemugra))] ; this pattern seems useless yet troublesome
          [_ (let ([? (datum->syntax #'_ (gensym))]) (values (cons ? snrettap) (cons ? stnemugra)))]
          [* (values (cons #'[... ...] snrettap) stnemugra)] ; this would make sense if typed racket could understand it 
          [field (values snrettap (cons #'field stnemugra))])))
    (list (list* #'list #;'func-name (reverse snerttap)) (cons <constructor> (reverse stnemugra)) (length stnemugra)))
  (syntax-parse stx #:datum-literals [:]
    [(self func-filter #:-> RangeType #:with [[dom : DomType defval ...] ...]
           [(fname aliases ...) #:=> [transforms ...] (~optional #:<+>) fparser ...] ...
           (~optional (~seq #:where [defaux ...])))
     (with-syntax ([defines (if (attribute defaux) #'(begin defaux ...) #'(void))]
                   [((([pattern ...] [transform ...] argc) ...) ...)
                    (for/list ([<function> (in-list (syntax->list #'(fname ...)))]
                               [<transforms> (in-list (syntax->list #'([transforms ...] ...)))])
                      (define transforms (syntax-e <transforms>))
                      (when (null? transforms) (raise-syntax-error (syntax-e #'self) "empty value transformation" <function>))
                      (for/list ([<transform> (in-list transforms)])
                        (define transform (syntax-e <transform>))
                        (cond [(pair? transform) (parse-pattern (car transform) (cdr transform))]
                              [(null? transform) (raise-syntax-error (syntax-e #'self) "missing value constructor" <transform>)]
                              [else (let ([? (datum->syntax <transform> (gensym))])
                                      (list (list #'? #'list? ?) (list #'values ?) 1))])))])
       (syntax/loc stx
         (define (func-filter [dom : DomType defval ...] ...) : (CSS:Filter RangeType) defines
           (define do-parse : (All (a) (-> Symbol (CSS-Parser (Listof a)) (Listof CSS-Token) (U (Listof Any) CSS-Syntax-Error)))
             (lambda [func-name func-parse func-argl]
               (define-values (fargs --tokens) (func-parse null func-argl))
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
                      [else (make-exn:css:range token)])))))))]
    [(self func-filter #:-> RangeType rest ...)
     (syntax/loc stx (define-css-function-filter func-filter #:-> RangeType #:with [] rest ...))]))

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
  (syntax-parse stx #:datum-literals [else]
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

(define-syntax (CSS<+> stx)
  (syntax-case stx []
    [(_) #'values]
    [(_ #:any css-parser css-parsers ...) (syntax/loc stx (css-disjoin (list css-parser css-parsers ...)))]
    [(_ css-parser) #'css-parser]
    [(_ css-parser css-parser2) (syntax/loc stx (css-disjoin css-parser css-parser2))]
    [(_ css-parser css-parser2 css-parsers ...) (syntax/loc stx (css-disjoin css-parser css-parser2 (CSS<+> css-parsers ...)))]))
  
(define-syntax (CSS:<~> stx)
  (syntax-case stx []
    [(_ css-filter f) (syntax/loc stx (css:compose css-filter f))]
    [(_ css-filter f g) (syntax/loc stx (css:compose (css:compose css-filter g) f))]
    [(_ css-filter f g h ...) (syntax/loc stx (css:compose css-filter (compose f g h ...)))]))

(define-syntax (CSS<&> stx)
  (syntax-case stx []
    [(_) #'values]
    [(_ #:any css-parser css-parsers ...) (syntax/loc stx (css-juxtapose (list css-parser css-parsers ...)))]
    [(_ css-parser) #'css-parser]
    [(_ css-parser css-parser2) (syntax/loc stx (css-juxtapose css-parser css-parser2))]
    [(_ css-parser css-parser2 css-parsers ...) (syntax/loc stx (css-juxtapose css-parser css-parser2 (CSS<&> css-parsers ...)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define CSS:<=> : (All (a) (-> (CSS:Filter Any) a (CSS:Filter a)))
  (lambda [css-filter const]
    (λ [[token : CSS-Syntax-Any]]
      (define datum : (CSS-Option Any) (css-filter token))
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
     ;;; NOTICE
     ; this filter is intentionally designed for tolerating a failed match,
     ; the input token therefore would be consumed unconditionally.
     (λ [[token : CSS-Syntax-Any]]
       (define datum : (CSS-Option a) (css:filter token))
       (cond [(false? datum) false-value]
             [(not (exn:css? datum)) datum]
             [(not fexn-value) false-value]
             [else (fexn-value datum)]))]))

(define CSS:<$> : (-> (CSS:Filter Any) Symbol (-> CSS-Shorthand-Datum Any) CSS-Shorthand-Parser)
  (lambda [css:filter tag fdatum]
    (λ [[data : CSS-Shorthand-Datum] [tokens : (Listof CSS-Token)]]
      (cond [(pair? tokens) (let ([css-parser (CSS:<^> css:filter tag)]) (css-parser data tokens))]
            [else (values (css-shorthand-set data tag (fdatum data)) null)]))))

(define CSS:<^> : (All (a) (case-> [(U (CSS:Filter a) (Listof+ (CSS:Filter a))) -> (CSS-Parser (Listof a))]
                                   [(CSS:Filter a) (->* (a) (CSS-Shorthand-Datum) CSS-Shorthand-Datum) -> CSS-Shorthand-Parser]
                                   [(CSS:Filter Any) (U Symbol (Listof+ Symbol)) -> CSS-Shorthand-Parser]
                                   [(CSS:Filter Any) (U Symbol (Listof+ Symbol)) CSS-Longhand-Update -> CSS-Shorthand-Parser]))
  (case-lambda
    [(atom-filter)
     (if (pair? atom-filter)
         (apply CSS:<&> atom-filter)
         (CSS:<&> atom-filter))]
    [(atom-filter tag)
     (if (procedure? tag)
         (λ [[data : CSS-Shorthand-Datum] [tokens : (Listof CSS-Token)]]
           (define-values (token --tokens) (css-car/cdr tokens))
           (define datum (atom-filter token))
           (cond [(or (false? datum) (exn:css? datum)) (values datum tokens)]
                 [else (values (tag datum data) --tokens)]))
         (λ [[data : CSS-Shorthand-Datum] [tokens : (Listof CSS-Token)]]
           (define-values (token --tokens) (css-car/cdr tokens))
           (define datum : (CSS-Option Any) (atom-filter token))
           (cond [(or (false? datum) (exn:css? datum)) (values datum tokens)]
                 [(symbol? tag) (values (css-shorthand-set data tag datum) --tokens)]
                 [else (values (css-shorthand-set* data tag datum) --tokens)])))]
    [(atom-filter tag updater)
     (λ [[data : CSS-Shorthand-Datum] [tokens : (Listof CSS-Token)]]
       (define-values (token --tokens) (css-car/cdr tokens))
       (define datum : (CSS-Option Any) (atom-filter token))
       (cond [(or (false? datum) (exn:css? datum)) (values datum tokens)]
             [(symbol? tag) (values (css-shorthand-set data tag datum updater) --tokens)]
             [else (values (css-shorthand-set* data tag datum updater) --tokens)]))]))

(define CSS:<++> : (All (a) (-> (CSS:Filter a) * (CSS-Parser (Listof a))))
  ;;; https://drafts.csswg.org/css-values/#comb-any
  (case-lambda
    [() values]
    [(atom-filter) (CSS:<&> #| yes, it's `CSS:<&>` |# atom-filter)]
    [atom-filters
     (λ [[data : (Listof a)] [tokens : (Listof CSS-Token)]]
       (let comb-any ([data++ : (Listof a) data]
                      [tokens-- : (Listof CSS-Token) tokens]
                      [filters : (Listof (CSS:Filter a)) atom-filters])
         (cond [(pair? filters)
                (let ([css-filter (car filters)])
                  (define-values (token --tokens) (css-car/cdr tokens--))
                  (define datum : (CSS-Option a) (css-filter token))
                  (cond [(nor (false? datum) (exn:css? datum)) (comb-any (cons datum data++) --tokens (cdr filters))]
                        [else (let no-order-combine ([rest : (Listof (CSS:Filter a)) (cdr filters)]
                                                     [sresol : (Listof (CSS:Filter a)) (list css-filter)])
                                (cond [(pair? rest)
                                       (let ([sub-filter (car rest)])
                                         (define subdatum : (CSS-Option a) (sub-filter token))
                                         (cond [(or (false? subdatum) (exn:css? subdatum)) (no-order-combine (cdr rest) (cons sub-filter sresol))]
                                               [else (comb-any (cons subdatum data++) --tokens (append (reverse sresol) (cdr rest)))]))]
                                      [(eq? data++ data) (values datum --tokens)] ; there should be at least one match
                                      [else (values data++ tokens--)]))]))]
               [(eq? data++ data) (values #false tokens)] ; there should be at least one match
               [else (values data++ tokens--)])))]))

(define CSS:<&> : (All (a) (-> (CSS:Filter a) * (CSS-Parser (Listof a))))
  ;;; https://drafts.csswg.org/css-values-4/#component-combinators [juxtaposing components]
  (case-lambda
    [() values]
    [(atom-filter)
     (λ [[data : (Listof a)] [tokens : (Listof CSS-Token)]]
       (define-values (head tail) (css-car/cdr tokens))
       (define datum : (CSS-Option a) (atom-filter head))
       (cond [(or (false? datum) (exn:css? datum)) (values datum tokens)]
             [else (values (cons datum data) tail)]))]
    [atom-filters
     (λ [[data : (Listof a)] [tokens : (Listof CSS-Token)]]
       (let juxtapose ([data++ : (Listof a) data]
                       [tokens-- : (Listof CSS-Token) tokens]
                       [filters : (Listof (CSS:Filter a)) atom-filters])
         (cond [(null? filters) (values data++ tokens--)]
               [else (let ([css-filter (car filters)])
                       (define-values (token --tokens) (css-car/cdr tokens--))
                       (define datum : (CSS-Option a) (css-filter token))
                       (cond [(or (false? datum) (exn:css? datum)) (values datum tokens--)]
                             [else (juxtapose (cons datum data++) --tokens (cdr filters))]))])))]))

(define CSS:<&&> : (All (a) (-> (CSS:Filter a) * (CSS-Parser (Listof a))))
  ;;; https://drafts.csswg.org/css-values/#comb-all
  (case-lambda
    [() values]
    [(atom-filter) (CSS:<&> atom-filter)]
    [atom-filters
     (λ [[data : (Listof a)] [tokens : (Listof CSS-Token)]]
       (let comb-all ([data++ : (Listof a) data]
                      [tokens-- : (Listof CSS-Token) tokens]
                      [filters : (Listof (CSS:Filter a)) atom-filters])
         (cond [(null? filters) (values data++ tokens--)]
               [else (let ([css-filter (car filters)])
                       (define-values (token --tokens) (css-car/cdr tokens--))
                       (define datum : (CSS-Option a) (css-filter token))
                       (cond [(nor (false? datum) (exn:css? datum)) (comb-all (cons datum data++) --tokens (cdr filters))]
                             [else (let no-order-fold ([rest : (Listof (CSS:Filter a)) (cdr filters)]
                                                       [sresol : (Listof (CSS:Filter a)) (list css-filter)])
                                     (cond [(null? rest) (values datum tokens--)]
                                           [else (let ([sub-filter (car rest)])
                                                   (define subdatum : (CSS-Option a) (sub-filter token))
                                                   (cond [(or (false? subdatum) (exn:css? subdatum)) (no-order-fold (cdr rest) (cons sub-filter sresol))]
                                                         [else (comb-all (cons subdatum data++) --tokens (append (reverse sresol) (cdr rest)))]))]))]))])))]))

(define CSS:<*> : (All (a) (->* ((CSS:Filter a)) ((U (CSS-Multiplier Index) '+ '? '*)) (CSS-Parser (Listof a))))
  ;;; https://drafts.csswg.org/css-values/#mult-zero-plus
  (lambda [atom-filter [multiplier '*]]
    (define-values (least most) (css:multiplier-range multiplier 0))
    (cond [(zero? most) values]
          [else (λ [[data : (Listof a)] [tokens : (Listof CSS-Token)]]
                  (let mult-0+ ([data++ : (Listof a) data]
                                [tokens-- : (Listof CSS-Token) tokens]
                                [n+1 : Natural 1])
                    (define-values (token --tokens) (css-car/cdr tokens--))
                    (define datum : (CSS-Option a) (atom-filter token))
                    (cond [(or (false? datum) (exn:css? datum)) (values (if (< least n+1) data++ datum) tokens--)]
                          [(= n+1 most) (values (cons datum data++) --tokens)] ; (= n +inf.0) also does not make much sense
                          [else (mult-0+ (cons datum data++) --tokens (add1 n+1))])))])))

(define CSS:<#> : (All (a) (->* ((CSS:Filter a)) ((U (CSS-Multiplier Positive-Index) '+) #:tolerate-leading-comma? Boolean) (CSS-Parser (Listof a))))
  ;;; https://drafts.csswg.org/css-values/#mult-comma
  ; It seems that, the omissible comma is said from the perspective of user instead of parser
  (lambda [atom-filter [multiplier '+] #:tolerate-leading-comma? [allow-omissible-leading-comma? #true]]
    (define-values (least most) (css:multiplier-range multiplier 1))
    (λ [[data : (Listof a)] [tokens : (Listof CSS-Token)]]
      (let mult-comma ([data++ : (Listof a) data]
                       [tokens-- : (Listof CSS-Token) tokens]
                       [n+1 : Natural 1]
                       [omissible? : Boolean allow-omissible-leading-comma?])
        (define-values (token tail) (css-car/cdr tokens--))
        (define datum : (CSS-Option a) (atom-filter token))
        (cond [(or (false? datum) (exn:css? datum))
               (if (and omissible? (css:comma? token) (pair? data))
                   (mult-comma data++ tail n+1 #false) ; preceding omittable argument is present, so sensibly ignore the first leading comma
                   (values (if (< least n+1) data++ datum) tokens--))]
              [(= n+1 most) (values (cons datum data++) tail)]
              [else (let-values ([(?comma --tokens) (css-car/cdr tail)])
                      (cond [(not ?comma) (mult-comma (cons datum data++) --tokens (add1 n+1) #false)] ; to check the least boundry
                            [(not (css:comma? ?comma)) (values (make-exn:css:missing-comma ?comma) tail)]
                            [(null? --tokens) (values (make-exn:css:missing-value ?comma) --tokens)]
                            [else (mult-comma (cons datum data++) --tokens (add1 n+1) #false)]))])))))

(define CSS:<!> : (All (a) (->* ((CSS:Filter a)) ((U (CSS-Multiplier Positive-Index) '+)) (CSS-Parser (Listof Any))))
  ;;; (WARNING: this is *not*) https://drafts.csswg.org/css-values/#mult-req
  (lambda [atom-filter [multiplier '+]]
    (define-values (least most) (css:multiplier-range multiplier 1))
    (λ [[data : (Listof Any)] [tokens : (Listof CSS-Token)]]
      (let not-mult-req ([sub++ : (Listof a) null]
                         [tokens-- : (Listof CSS-Token) tokens]
                         [n+1 : Natural 1])
        (define-values (token --tokens) (css-car/cdr tokens--))
        (define datum : (CSS-Option a) (atom-filter token))
        (if (or (false? datum) (exn:css? datum))
            (cond [(< least n+1) (values (cons (reverse sub++) data) tokens--)]
                  [else (values datum --tokens)])
            (cond [(= n+1 most) (values (cons (reverse (cons datum sub++)) data) --tokens)]
                  [else (not-mult-req (cons datum sub++) --tokens (add1 n+1))]))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define CSS<^> : (All (a) (case-> [(CSS-Parser (Listof a)) -> (CSS-Parser (Listof Any))]
                                  [(CSS-Parser (Listof Any)) (U Symbol (Listof+ Symbol)) -> CSS-Shorthand-Parser]
                                  [(CSS-Parser (Listof Any)) (U Symbol (Listof+ Symbol)) CSS-Longhand-Update -> CSS-Shorthand-Parser]))
  (case-lambda
    [(atom-parser) ; equivalent to casting `(CSS-Parser (Listof a))` into `(CSS-Parser (Listof Any))` for `CSS-Declaration-Parser`
     (λ [[data : (Listof Any)] [tokens : (Listof CSS-Token)]]
       (define-values (datum --tokens) (atom-parser null tokens))
       (cond [(or (false? datum) (exn:css? datum)) (values datum --tokens)]
             [else (values (append datum data) --tokens)]))]
    [(atom-parser tag)
     (λ [[data : CSS-Shorthand-Datum] [tokens : (Listof CSS-Token)]]
       (define-values (subdata --tokens) (atom-parser null tokens))
       (cond [(or (false? subdata) (exn:css? subdata)) (values subdata --tokens)]
             [(symbol? tag) (values (css-shorthand-set data tag (reverse subdata)) --tokens)]
             [else (values (css-shorthand-set* data tag (reverse subdata)) --tokens)]))]
    [(atom-parser tag updater)
     (λ [[data : CSS-Shorthand-Datum] [tokens : (Listof CSS-Token)]]
       (define-values (subdata --tokens) (atom-parser null tokens))
       (cond [(or (false? subdata) (exn:css? subdata)) (values subdata --tokens)]
             [(list? tag) (values (css-shorthand-set* data tag (reverse subdata) updater) --tokens)]
             [else (values (css-shorthand-set data tag (reverse subdata) updater) --tokens)]))]))

(define CSS<$> : (case-> [(CSS-Parser (Listof Any)) -> (CSS-Parser (Listof Any))]
                         [(CSS-Parser (Listof Any)) Any -> (CSS-Parser (Listof Any))]
                         [(CSS-Parser (Listof Any)) Symbol (-> CSS-Shorthand-Datum Any) -> CSS-Shorthand-Parser])
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
     (λ [[data : CSS-Shorthand-Datum] [tokens : (Listof CSS-Token)]]
       (cond [(pair? tokens) (let ([css-parser (CSS<^> css:filter tag)]) (css-parser data tokens))]
             [else (values (css-shorthand-set data tag (fdatum data)) null)]))]))

(define CSS<~> : (All (a b) (-> (CSS-Parser (Listof a)) (-> (Listof a) (CSS-Option b)) (CSS-Parser (Listof b))))
  (lambda [css-parser css->racket]
     (λ [[data : (Listof b)] [tokens : (Listof CSS-Token)]]
       (define-values (datum --tokens) (css-parser null tokens))
       (cond [(or (exn:css? datum) (false? datum)) (values datum --tokens)]
             [else (let ([rdatum (css->racket (reverse datum))])
                     (cond [(or (exn:css? rdatum) (false? rdatum)) (values rdatum tokens)]
                           [else (values (cons rdatum data) --tokens)]))]))))

(define CSS<_> : (All (a) (-> (CSS-Parser a) (CSS-Parser a)))
  (lambda [css-parser]
    (λ [[data : a] [tokens : (Listof CSS-Token)]]
      (define-values (++data --tokens) (css-parser data tokens))
      (cond [(or (exn:css? ++data) (false? ++data)) (values ++data --tokens)]
            [else (values data tokens)]))))

(define CSS<++> : (All (a) (case-> [(Listof (CSS-Parser a)) -> (CSS-Parser a)]
                                   [(CSS-Parser a) * -> (CSS-Parser a)]))
  ;;; https://drafts.csswg.org/css-values/#comb-any
  (case-lambda
    [() values]
    [(css-parser)
     (cond [(list? css-parser) (apply CSS<++> css-parser)]
           [else css-parser])]
    [css-parsers
     (λ [[data : a] [tokens : (Listof CSS-Token)]]
       (let comb-any ([data++ : a data]
                      [tokens-- : (Listof CSS-Token) tokens]
                      [parsers : (Listof (CSS-Parser a)) css-parsers])
         (cond [(pair? parsers)
                (let ([css-parser (car parsers)])
                  (define-values (++data --tokens) (css-parser data++ tokens--))
                  (cond [(nor (false? ++data) (exn:css? ++data)) (comb-any ++data --tokens (cdr parsers))]
                        [else(let no-order-combine ([rest : (Listof (CSS-Parser a)) (cdr parsers)]
                                                    [sresol : (Listof (CSS-Parser a)) (list css-parser)])
                               (cond [(pair? rest)
                                      (let ([sub-parser (car rest)])
                                        (define-values (++subdata --subtokens) (sub-parser data++ tokens--))
                                        (cond [(or (false? ++subdata) (exn:css? ++subdata)) (no-order-combine (cdr rest) (cons sub-parser sresol))]
                                              [else (comb-any ++subdata --subtokens (append (reverse sresol) (cdr rest)))]))]
                                     [(eq? data++ data) (values ++data --tokens)] ; there should be at least one match
                                     [else (values data++ tokens--)]))]))]
               [(eq? data++ data) (values #false tokens)] ; there should be at least one match
               [else (values data++ tokens--)])))]))

(define CSS<&&> : (All (a) (case-> [(Listof (CSS-Parser a)) -> (CSS-Parser a)]
                                   [(CSS-Parser a) * -> (CSS-Parser a)]))
  ;;; https://drafts.csswg.org/css-values/#comb-all
  (case-lambda
    [() values]
    [(css-parser)
     (cond [(list? css-parser) (apply CSS<&&> css-parser)]
           [else css-parser])]
    [css-parsers
     (λ [[data : a] [tokens : (Listof CSS-Token)]]
       (let comb-all ([data++ : a data]
                      [tokens-- : (Listof CSS-Token) tokens]
                      [parsers : (Listof (CSS-Parser a)) css-parsers])
         (cond [(null? parsers) (values data++ tokens--)]
               [else (let ([css-parser (car parsers)])
                       (define-values (++data --tokens) (css-parser data++ tokens--))
                       (cond [(nor (false? ++data) (exn:css? ++data)) (comb-all ++data --tokens (cdr parsers))]
                             [else (let no-order-combine ([rest : (Listof (CSS-Parser a)) (cdr parsers)]
                                                          [sresol : (Listof (CSS-Parser a)) (list css-parser)])
                                     (cond [(null? rest) (values ++data --tokens)]
                                           [else (let ([sub-parser (car rest)])
                                                   (define-values (++subdata --subtokens) (sub-parser data++ tokens--))
                                                   (cond [(or (false? ++subdata) (exn:css? ++subdata)) (no-order-combine (cdr rest) (cons sub-parser sresol))]
                                                         [else (comb-all ++subdata --subtokens (append (reverse sresol) (cdr rest)))]))]))]))])))]))
  
(define CSS<*> : (All (a) (->* ((CSS-Parser a)) ((U (CSS-Multiplier Index) '+ '? '*)) (CSS-Parser a)))
  ;;; https://drafts.csswg.org/css-values/#mult-zero-plus
  (lambda [css-parser [multiplier '*]]
    (define-values (least most) (css:multiplier-range multiplier 0))
    (cond [(zero? most) values]
          [else (λ [[data : a] [tokens : (Listof CSS-Token)]]
                  (let mult-0+ ([data++ : a data]
                                [tokens-- : (Listof CSS-Token) tokens]
                                [n+1 : Natural 1])
                    (define-values (++data --tokens) (css-parser data++ tokens--))
                    (cond [(or (false? ++data) (exn:css? ++data)) (if (< least n+1) (values data++ tokens--) (values ++data --tokens))]
                          [(= n+1 most) (values ++data --tokens)] ; (= n +inf.0) also does not make much sense
                          [else (mult-0+ ++data --tokens (add1 n+1))])))])))

(define CSS<#> : (All (a) (->* ((CSS-Parser a)) ((U (CSS-Multiplier Positive-Index) '+) #:tolerate-leading-comma? Boolean) (CSS-Parser a)))
  ;;; https://drafts.csswg.org/css-values/#mult-comma
  ; It seems that, the omissible comma is said from the perspective of user instead of parser
  (lambda [css-parser [multiplier '+] #:tolerate-leading-comma? [allow-omissible-leading-comma? #true]]
    (define-values (least most) (css:multiplier-range multiplier 1))
    (λ [[data : a] [tokens : (Listof CSS-Token)]]
      (let mult-comma ([data++ : a data]
                       [tokens-- : (Listof CSS-Token) tokens]
                       [n+1 : Natural 1]
                       [omissible? : Boolean allow-omissible-leading-comma?])
        (define-values (++data tail) (css-parser data++ tokens--))
        (cond [(or (false? ++data) (exn:css? ++data))
               (let-values ([(?comma tail) (css-car/cdr tokens--)])
                 (if (and omissible? (css:comma? ?comma) (pair? data++))
                     (mult-comma data++ tail n+1 #false) ; preceding omittable argument is present, so sensibly ignore the first leading comma
                     (values (if (< least n+1) data++ ++data) tokens--)))]
              [(= n+1 most) (values ++data tail)]
              [else (let-values ([(?comma --tokens) (css-car/cdr tail)])
                      (cond [(not ?comma) (mult-comma ++data --tokens (add1 n+1) #false)] ; to check least boundry
                            [(not (css:comma? ?comma)) (values (make-exn:css:missing-comma ?comma) tail)]
                            [(null? --tokens) (values (make-exn:css:missing-value ?comma) --tokens)]
                            [(eq? data++ ++data) (values (make-exn:css:missing-value ?comma) --tokens)]
                            [else (mult-comma ++data --tokens (add1 n+1) #false)]))])))))

(define CSS<!> : (All (a) (->* ((CSS-Parser (Listof a))) ((U (CSS-Multiplier Positive-Index) '+)) (CSS-Parser (Listof Any))))
  ;;; (WARNING: this is *not*) https://drafts.csswg.org/css-values/#mult-req
  (lambda [css-parser [multiplier '+]]
    (define-values (least most) (css:multiplier-range multiplier 1))
    (λ [[data : (Listof Any)] [tokens : (Listof CSS-Token)]]
      (let not-mult-req ([subdata++ : (Listof a) null]
                         [tokens-- : (Listof CSS-Token) tokens]
                         [n+1 : Natural 1])
        (define-values (subdata --tokens) (css-parser subdata++ tokens--))
        (if (or (false? subdata) (exn:css? subdata))
            (cond [(< least n+1) (values (cons (reverse subdata++) data) tokens--)]
                  [else (values subdata --tokens)])
            (cond [(= n+1 most) (values (cons (reverse subdata) data) --tokens)]
                  [else (not-mult-req subdata --tokens (add1 n+1))]))))))

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

(define css-disjoin : (All (a b c) (case-> [(Listof (CSS-Parser a)) -> (CSS-Parser a)]
                                           [(CSS-Parser (Listof a)) (CSS-Parser (Listof b)) -> (CSS-Parser (Listof (U a b)))]
                                           [(CSS-Parser (Listof a)) (CSS-Parser (Listof b)) (CSS-Parser (Listof c)) -> (CSS-Parser (Listof (U a b c)))]))
  ;;; https://drafts.csswg.org/css-values/#comb-one
  (case-lambda
    [(atom-parsers)
     (cond [(null? atom-parsers) values]
           [(null? (cdr atom-parsers)) (car atom-parsers)]
           [else (λ [[data : a] [tokens : (Listof CSS-Token)]]
                   (let combine-one ([head-parser : (CSS-Parser a) (car atom-parsers)]
                                     [tail-parsers : (Listof (CSS-Parser a)) (cdr atom-parsers)])
                     (define-values (++data --tokens) (head-parser data tokens))
                     (cond [(nor (false? ++data) (exn:css? ++data)) (values ++data --tokens)]
                           [(pair? tail-parsers) (combine-one (car tail-parsers) (cdr tail-parsers))]
                           [else (values ++data --tokens)])))])]
    [(atom-parser1 atom-parser2)
     (λ [[data : (Listof (U a b))] [tokens : (Listof CSS-Token)]]
       (define-values (datum --tokens) (atom-parser1 null tokens))
       (cond [(nor (false? datum) (exn:css? datum)) (values (append datum data) --tokens)]
             [else (let-values ([(datum --tokens) (atom-parser2 null tokens)])
                     (cond [(or (false? datum) (exn:css? datum)) (values datum --tokens)]
                           [else (values (append datum data) --tokens)]))]))]
    [(atom-parser1 atom-parser2 atom-parser3)
     (λ [[data : (Listof (U a b c))] [tokens : (Listof CSS-Token)]]
       (define-values (datum --tokens) (atom-parser1 null tokens))
       (cond [(nor (false? datum) (exn:css? datum)) (values (append datum data) --tokens)]
             [else (let-values ([(datum --tokens) (atom-parser2 null tokens)])
                     (cond [(nor (false? datum) (exn:css? datum)) (values (append datum data) --tokens)]
                           [else (let-values ([(datum --tokens) (atom-parser3 null tokens)])
                                   (cond [(or (false? datum) (exn:css? datum)) (values datum --tokens)]
                                         [else (values (append datum data) --tokens)]))]))]))]))
  
(define css:juxtapose : (All (a b c) (case-> [(CSS:Filter a) (CSS:Filter b) -> (CSS:Filter (List b a))]
                                             [(CSS:Filter a) (CSS:Filter b) (CSS:Filter c) -> (CSS:Filter (List c b a))]))
  (case-lambda
    [(css-filter1 css-filter2)
     (λ [[token : CSS-Syntax-Any]]
       (define datum : (CSS-Option a) (css-filter1 token))
       (cond [(or (false? datum) (exn:css? datum)) datum]
             [else (let ([datum2 (css-filter2 token)])
                     (cond [(or (false? datum2) (exn:css? datum2)) datum2]
                           [else (list datum2 datum)]))]))]
    [(css-filter1 css-filter2 css-filter3)
     (λ [[token : CSS-Syntax-Any]]
       (define datum : (CSS-Option a) (css-filter1 token))
       (cond [(or (false? datum) (exn:css? datum)) datum]
             [else (let ([datum2 (css-filter2 token)])
                     (cond [(or (false? datum2) (exn:css? datum2)) datum2]
                           [else (let ([datum3 (css-filter3 token)])
                                   (cond [(or (false? datum3) (exn:css? datum3)) datum3]
                                         [else (list datum3 datum2 datum)]))]))]))]))

(define css-juxtapose : (All (a b c) (case-> [(Listof (CSS-Parser a)) -> (CSS-Parser a)]
                                             [(CSS-Parser (Listof a)) (CSS-Parser (Listof b)) -> (CSS-Parser (Listof (U a b)))]
                                             [(CSS-Parser (Listof a)) (CSS-Parser (Listof b)) (CSS-Parser (Listof c)) -> (CSS-Parser (Listof (U a b c)))]))
  ;;; https://drafts.csswg.org/css-values-4/#component-combinators [juxtaposing components]
  (case-lambda
    [(atom-parsers)
     (λ [[data : a] [tokens : (Listof CSS-Token)]]
       (let combine-all ([data++ : a data]
                         [tokens-- : (Listof CSS-Token) tokens]
                         [parsers : (Listof (CSS-Parser a)) atom-parsers])
         (cond [(null? parsers) (values data++ tokens--)]
               [else (let-values ([(++data --tokens) ((car parsers) data++ tokens--)])
                       (cond [(or (false? ++data) (exn:css? ++data)) (values ++data --tokens)]
                             [else (combine-all ++data --tokens (cdr parsers))]))])))]
    [(atom-parser1 atom-parser2)
     (λ [[data : (Listof (U a b))] [tokens : (Listof CSS-Token)]]
       (define-values (datum1 --tokens) (atom-parser1 null tokens))
       (cond [(or (false? datum1) (exn:css? datum1)) (values datum1 --tokens)]
             [else (let-values ([(datum2 ----tokens) (atom-parser2 null --tokens)])
                     (cond [(or (false? datum2) (exn:css? datum2)) (values datum2 ----tokens)]
                           [else (values (append datum2 datum1 data) ----tokens)]))]))]
    [(atom-parser1 atom-parser2 atom-parser3)
     (λ [[data : (Listof (U a b c))] [tokens : (Listof CSS-Token)]]
       (define-values (datum1 --tokens) (atom-parser1 null tokens))
       (cond [(or (false? datum1) (exn:css? datum1)) (values datum1 --tokens)]
             [else (let-values ([(datum2 ----tokens) (atom-parser2 null --tokens)])
                     (cond [(or (false? datum2) (exn:css? datum2)) (values datum2 ----tokens)]
                           [else (let-values ([(datum3 ------tokens) (atom-parser3 null ----tokens)])
                                   (cond [(or (false? datum3) (exn:css? datum3)) (values datum3 ------tokens)]
                                         [else (values (append datum3 datum2 datum1 data) ------tokens)]))]))]))]))

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
(define-type CSS-Boolean (U One Zero))
(define-type CSS-Flonum-% (U Flonum CSS-%))
(define-type CSS+Flonum-% (U Nonnegative-Flonum CSS+%))

(define css-boolean? : (-> Any Boolean : #:+ CSS-Boolean) (lambda [v] (and (byte? v) (or (= v 0) (= v 1)))))
(define css-true? : (-> Integer Boolean) (lambda [v] (not (css-false? v))))
(define css-false? : (-> Integer Boolean) (lambda [v] (= v 0)))

(define css-flonum-%? : (-> Any Boolean : CSS-Flonum-%) (lambda [v] (or (flonum? v) (css-%? v))))
(define css+flonum-%? : (-> Any Boolean : #:+ CSS+Flonum-%) (lambda [v] (or (nonnegative-flonum? v) (css+%? v))))

; for properties whoes computed values are not their used value
(struct css-unitless ([value : Flonum]) #:type-name CSS-Unitless #:transparent #:constructor-name make-css-unitless)
(struct css+unitless css-unitless ([value : Nonnegative-Flonum]) #:type-name CSS+Unitless #:constructor-name unsafe-css+unitless)
(define make-css+unitless : (-> Nonnegative-Flonum CSS+Unitless) (lambda [ul] (unsafe-css+unitless ul ul)))

(struct css-% ([value : Flonum]) #:type-name CSS-% #:transparent #:constructor-name make-css-%)
(struct css+% css-% ([value : Nonnegative-Flonum]) #:type-name CSS+% #:constructor-name unsafe-css+%)
(define make-css+% : (-> Nonnegative-Flonum CSS+%) (lambda [%] (unsafe-css+% % %)))

(struct css-position ([x : CSS-Flonum-%] [y : CSS-Flonum-%]) #:type-name CSS-Position #:transparent)
(struct css-region ([top : Flonum] [right : Flonum] [bottom : Flonum] [left : Flonum]) #:type-name CSS-Region #:transparent)

(define 0% : CSS+% (make-css+% 0.0))
(define 50% : CSS+% (make-css+% 0.5))
(define 100% : CSS+% (make-css+% 1.0))

(define css-center-position : CSS-Position (css-position 50% 50%))
(define css-full-region : CSS-Region (css-region 0.0 0.0 0.0 0.0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; WARNING: Racket's Boolean cannot be used directly since `#false` has been interpreted as a failure by recognizer 
(define-css-disjoint-filter <css-boolean> #:-> CSS-Boolean
  #:with [[true-value : Symbol 'true] [false-value : Symbol 'false]]
  (CSS:<=> (<css:integer> = 0) 0)
  (CSS:<=> (<css:integer> = 1) 1)
  (CSS:<=> (<css-keyword> true-value) 1)
  (CSS:<=> (<css-keyword> false-value) 0))

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
  #:with [[restriction : (Option (U '#:legacy '#:strict)) #false]]
  (case restriction
    [(#:strict) (<css:angle>)]
    [(#:legacy) (CSS:<+> (<css:angle>) (CSS:<=> (<css:integer> = 0) 0.0) (<css:flonum> = 0.0))] ; CSS will disable this in the future
    [else (CSS:<+> (<css:angle>) (<css:flonum>) (CSS:<~> (<css:integer>) exact->inexact))]))

(define <:css-keywords:> : (->* ((Listof Symbol)) (Symbol) (CSS-Parser (Listof Symbol)))
  (lambda [options [none 'none]]
    (CSS<+> (CSS:<^> (<css-keyword> none))
            (CSS:<*> (<css-keyword> options) '+))))

(define <:css-strings:> : (->* () ((U (CSS-Multiplier Index) '+ '? '*) (-> String Boolean)) (CSS-Parser (Listof String)))
  (lambda [[multipliers '+] [string-filter (λ _ #true)]]
    (CSS<*> (CSS:<^> (<css:string> string-filter)) multipliers)))

(define-css-atomic-filter <css-comma> #:-> Char #:with [[token : css:comma?]] #:on-error make-exn:css:missing-comma #\,)
(define-css-atomic-filter <css-slash> #:-> Char #:with [[token : css:slash?]] #:on-error make-exn:css:missing-slash #\/)

(define (<:css-skip-comma:> [omissible? : (Option '#:omissible) #false]) : (All (a) (CSS-Parser a))
  (λ [[data : a] [tokens : (Listof CSS-Token)]]
    (define-values (?comma --tokens) (css-car/cdr tokens))
    (cond [(css:comma? ?comma)
           (cond ; WARNING: please using `css-comma-followed-parser` instead if preceding value is omissible
             [(null? --tokens) (values (make-exn:css:missing-value ?comma) tokens)]
             [else (values data --tokens)])]
          [(or omissible?) (values data --tokens)]
          [(not ?comma) (values data --tokens)]
          [else (values (make-exn:css:missing-comma ?comma) tokens)])))

(define (<css-keyword:to>) : (CSS:Filter Symbol) (CSS:<?> (<css:ident> 'to) make-exn:css:missing-keyword))
(define (<css-keyword:at>) : (CSS:Filter Symbol) (CSS:<?> (<css:ident> 'at) make-exn:css:missing-keyword))
(define (<css-keyword:in>) : (CSS:Filter Symbol) (CSS:<?> (<css:ident> 'in) make-exn:css:missing-keyword))
(define (<css-keyword:from>) : (CSS:Filter Symbol) (CSS:<?> (<css:ident> 'from) make-exn:css:missing-keyword))

(define (<css+unitless>) : (CSS:Filter CSS+Unitless) (CSS:<~> (<css+real>) (λ [[v : Nonnegative-Real]] (make-css+unitless (real->double-flonum v)))))
(define (<css-percentage>) : (CSS:Filter CSS-%) (CSS:<~> (<css:percentage>) make-css-%))
(define (<css+percentage>) : (CSS:Filter CSS+%) (CSS:<~> (<css:percentage> nonnegative-flonum?) make-css+%))

(define (<css-length-percentage>) : (CSS:Filter CSS-Flonum-%) (CSS:<+> (<css:length>) (<css-percentage>)))
(define (<css+length-percentage>) : (CSS:Filter CSS+Flonum-%) (CSS:<+> (<css+length>) (<css+percentage>)))

(define (<css-angle-percentage>) : (CSS:Filter CSS-Flonum-%) (CSS:<+> (<css:angle>) (<css-percentage>)))
(define (<css+angle-percentage>) : (CSS:Filter CSS+Flonum-%) (CSS:<+> (<css+angle>) (<css+percentage>)))

(define (<css-frequency-percentage>) : (CSS:Filter CSS-Flonum-%) (CSS:<+> (<css:frequency>) (<css-percentage>)))
(define (<css+frequency-percentage>) : (CSS:Filter CSS+Flonum-%) (CSS:<+> (<css+frequency>) (<css+percentage>)))

(define (<css-time-percentage>) : (CSS:Filter CSS-Flonum-%) (CSS:<+> (<css:time>) (<css-percentage>)))
(define (<css+time-percentage>) : (CSS:Filter CSS+Flonum-%) (CSS:<+> (<css+time>) (<css+percentage>)))

(define css-omissible-comma-parser : (All (a) (-> (CSS-Parser a) (CSS-Parser a)))
  (lambda [atom-parser]
    (λ [[data : a] [tokens : (Listof CSS-Token)]]
      (define-values (?comma --tokens) (css-car/cdr tokens))
      (cond [(css:comma? ?comma) (atom-parser data --tokens)]
            [else (atom-parser data tokens)]))))

(define css-comma-followed-parser : (All (a) (->* ((CSS-Parser a)) ((Option '#:omissible)) (CSS-Parser a)))
  (lambda [atom-parser [omissible? #false]]
    (define skip-comma (<:css-skip-comma:> omissible?))
    (λ [[data : a] [tokens : (Listof CSS-Token)]]
      (define-values (++data --tokens) (atom-parser data tokens))
      (cond [(or (not ++data) (exn:css? ++data)) (values ++data --tokens)]
            [(eq? ++data data) (values ++data --tokens)] ; the value is omitted, so is the comma
            [else (skip-comma ++data --tokens)]))))

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
     (cons (CSS<*> (css-disjoin parsers) '+) names)]
    [(filters names)
     (cons (CSS<*> (css-disjoin (map CSS:<^> filters names)) '+) names)]
    [(filter vertical horizontal)
     (cons (CSS<&> #:any
                   (CSS:<^> filter vertical)
                   (CSS:<$> filter horizontal (λ [longhand] (css-shorthand-ref longhand vertical))))
           (list vertical horizontal))]
    [(filter top right bottom left)
     (cons (CSS<&> #:any
                   (CSS:<^> filter top)
                   (CSS:<$> filter right (λ [longhand] (css-shorthand-ref longhand top)))
                   (CSS:<$> filter bottom (λ [longhand] (css-shorthand-ref longhand top)))
                   (CSS:<$> filter left (λ [longhand] (css-shorthand-ref longhand right))))
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

(define make-css->unboxed-datum : (All (a b) (-> (-> Any Boolean : #:+ a) b (CSS->Racket (U a b))))
  (lambda [datum? fallback-value]
    (λ [property datum]
      (if (pair? datum)
          (let ([v (car datum)])
            (if (datum? v) v fallback-value))
          fallback-value))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; WARNING
; These are intentionally designed for function filter, which accepts a list for pattern matching.
; If they are used to parse declaration whose computed value is a single datum directly,
;   please keep in mind that the computed value is contained in a list,
;   and `make-css->unboxed-datum` is your friend.
(define <:css-position:> : (-> (CSS-Parser (Listof CSS-Position)))
  ;;; https://www.w3.org/TR/css-values-4/#position
  (let* ([xs : (Listof Symbol) '(left right)]
         [ys : (Listof Symbol) '(top bottom)]
         [cxs : (Listof Symbol) (cons 'center xs)]
         [cys : (Listof Symbol) (cons 'center ys)])
    (define (css->racket [v : (U Symbol CSS+Flonum-%)]) : CSS-Flonum-%
      (cond [(not (symbol? v)) v]
            [else (case v
                    [(left top)     0%]
                    [(right bottom) 100%]
                    [else           50%])]))

    (define (pos-negate [v : (U Symbol CSS+Flonum-%)]) : CSS-Flonum-%
      (cond [(flonum? v) (- v)]
            [(css-%? v) (make-css-% (- (css-%-value v)))]
            [else 50%]))
    
    (define (keyword->position [kws : (Listof Symbol)]) : CSS-Position
      (let pfold ([ps : (Listof Symbol) kws]
                  [x : (Option CSS-Flonum-%) #false]
                  [y : (Option CSS-Flonum-%) #false])
        (cond [(null? ps) (css-position (or x 50%) (or y 50%))]
              [else (let ([rest (cdr ps)])
                      (case (car ps)
                        [(left)   (pfold rest 0%   y)]
                        [(right)  (pfold rest 100% y)]
                        [(top)    (pfold rest x    0%)]
                        [(bottom) (pfold rest x    100%)]
                        [else (if (not x)
                                  (pfold rest 50%  y)
                                  (pfold rest x    50%))]))])))

    (define (keyword-length-%->position [vs : (Listof (U Symbol CSS+Flonum-%))]) : CSS-Position
      (cond [(null? vs) '#:deadcode css-center-position]
            [(null? (cdr vs)) (css-position (css->racket (car vs)) 50%)]
            [else (css-position (css->racket (car vs)) (css->racket (cadr vs)))]))
    
    (define (relative-position [vs : (Listof (U Symbol CSS+Flonum-%))]) : CSS-Position
      (let pfold ([ps : (Listof (U Symbol CSS+Flonum-%)) vs]
                  [x : (Option CSS-Flonum-%) #false]
                  [y : (Option CSS-Flonum-%) #false])
        (cond [(or (null? ps) (null? (cdr ps))) '#:deadcode (css-position (or x 50%) (or y 50%))]
              [else (let-values ([(pos rest) (values (cadr ps) (cddr ps))])
                      (case (car ps)
                        [(left)   (pfold rest (css->racket pos) y)]
                        [(top)    (pfold rest x (css->racket pos))]

                        ; NOTE: negative values are invalid by specification,
                        ;   so that we can employ negative values for right-bottom offsets
                        [(right)  (pfold rest (pos-negate pos) y)]
                        [(bottom) (pfold rest x (pos-negate pos))]
                        [else '#:deadcode (pfold rest x y)]))])))
    
    (lambda []
      ; WARNING: it is interpreted as specified in `background-position`,
      ;   however the syntax itself is different than of `background-position`
      ;   as it does not support the 3-value variant, which causes ambiguities.
      (CSS<+> (CSS<~> (CSS<&&> (CSS:<&> (<css-keyword> xs) (<css+length-percentage>))
                               (CSS:<&> (<css-keyword> ys) (<css+length-percentage>)))
                      relative-position)
              (CSS<~> (CSS<&> (CSS:<^> (CSS:<+> (<css-keyword> cxs) (<css+length-percentage>)))
                              (CSS:<*> (CSS:<+> (<css-keyword> cys) (<css+length-percentage>)) '?))
                      keyword-length-%->position)
              (CSS<~> (CSS:<++> (<css-keyword> cxs) (<css-keyword> cys))
                      keyword->position)))))

(define <:css-region:> : (-> (CSS:Filter Flonum) (CSS-Parser (Listof CSS-Region)))
  (let ()
    (define (smart-region [fs : (Listof Flonum)]) : CSS-Region
      (define-values (top right bottom left) (list->4:values fs 0.0))
      (css-region top right bottom left))
    
    (lambda [atom-filter]
      (CSS<~> (CSS:<*> atom-filter '+)
              smart-region))))
