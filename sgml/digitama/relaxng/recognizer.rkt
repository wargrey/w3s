#lang typed/racket

;;; Parser Combinators and Syntax Sugars of parsing the compact syntax of RelaxNG
;;; This is ported from w3s/css by removing unused APIs

(provide (all-defined-out))

(require "../digicore.rkt")
(require "../namespace.rkt")

(require css/digitama/syntax/misc)

(require racket/string)
(require digimon/symbol)

(require (for-syntax syntax/parse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (define-rnc-disjoint-filter stx)
  (syntax-case stx [:]
    [(_ compound-filter #:-> RangeType #:with [[dom : DomType defval ...] ...] atom-filters ...)
     (syntax/loc stx (define (compound-filter [dom : DomType defval ...] ...) : (XML:Filter RangeType) (RNC:<+> atom-filters ...)))]
    [(_ compound-filter #:-> RangeType atom-filters ...)
     (syntax/loc stx (define (compound-filter) : (XML:Filter RangeType) (RNC:<+> atom-filters ...)))]))
  
(define-syntax (define-rnc-atomic-filter stx)
  (syntax-parse stx #:datum-literals [:]
    [(_ atom-filter #:-> RangeType #:with [[token : token?] [dom : DomType defval ...] ...]
        (~optional (~seq #:on-error make-exn) #:defaults ([make-exn #'#false]))
        atom-body ...
        #:where [defaux ...])
     (syntax/loc stx
       (define (atom-filter [dom : DomType defval ...] ...) : (XML:Filter RangeType) defaux ...
         (λ [[token : XML-Syntax-Any]] : (XML-Option RangeType)
           (cond [(token? token) atom-body ...]
                 [else (and make-exn (make-exn token))]))))]
    [(defilter atom-filter #:-> RangeType #:with [[token : token?] [dom : DomType defval ...] ...] atom-body ...)
     (syntax/loc stx (defilter atom-filter #:-> RangeType #:with [[token : token?] [dom : DomType defval ...] ...] atom-body ... #:where []))]))

(define-syntax (RNC<?> stx)
  (syntax-parse stx #:datum-literals [else]
    [(_) #'values]
    [(_ [else <else> ...]) (syntax/loc stx (RNC<&> <else> ...))]
    [(_ [<if> <then> ...]) (syntax/loc stx (rnc:if <if> (RNC<&> <then> ...) #false))]
    [(_ [<if> <then> ...] [else <else> ...]) (syntax/loc stx (rnc:if <if> (RNC<&> <then> ...) (RNC<&> <else> ...)))]
    [(_ [<if> <then> ...] ... [else <else> ...]) (syntax/loc stx (rnc:if (list (cons <if> (RNC<&> <then> ...)) ...) (RNC<&> <else> ...)))]
    [(_ [<if> <then> ...] ...) (syntax/loc stx (rnc:if (list (cons <if> (RNC<&> <then> ...)) ...) #false))]))
  
(define-syntax (RNC:<+> stx)
  (syntax-case stx []
    [(_ rnc-filter) #'rnc-filter]
    [(_ rnc-filter rnc-filters ...) (syntax/loc stx (rnc:disjoin rnc-filter (RNC:<+> rnc-filters ...)))]))

(define-syntax (RNC<+> stx)
  (syntax-case stx []
    [(_) #'values]
    [(_ #:any rnc-parser rnc-parsers ...) (syntax/loc stx (rnc-disjoin (list rnc-parser rnc-parsers ...)))]
    [(_ rnc-parser) #'rnc-parser]
    [(_ rnc-parser rnc-parsers ...) (syntax/loc stx (rnc-disjoin rnc-parser (RNC<+> rnc-parsers ...)))]))

(define-syntax (RNC<&> stx)
  (syntax-case stx []
    [(_) #'values]
    [(_ #:any rnc-parser rnc-parsers ...) (syntax/loc stx (rnc-juxtapose (list rnc-parser rnc-parsers ...)))]
    [(_ rnc-parser) #'rnc-parser]
    [(_ rnc-parser rnc-parsers ...) (syntax/loc stx (rnc-juxtapose rnc-parser (RNC<&> rnc-parsers ...)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define RNC:<=> : (All (a) (-> (XML:Filter Any) a (XML:Filter a)))
  (lambda [rnc-filter const]
    (λ [[token : XML-Syntax-Any]]
      (define datum : (XML-Option Any) (rnc-filter token))
      (if (exn:xml? datum) datum (and datum const)))))

(define RNC:<?> : (All (a b c) (case-> [(XML:Filter a) (-> (U XML-Syntax-Any (Listof XML-Token)) XML-Syntax-Error) -> (XML:Filter a)]
                                       [(XML:Filter a) b (-> XML-Syntax-Error c) -> (XML:Filter (U a b c))]))
  (case-lambda
    [(rnc:filter make-exn)
     (λ [[token : XML-Syntax-Any]]
       (define datum : (XML-Option a) (rnc:filter token))
       (cond [(or (not datum) (exn:xml? datum)) (make-exn token)]
             [else datum]))]
    [(rnc:filter false-value fexn-value)
     ;;; NOTICE
     ; this filter is intentionally designed for tolerating a failed match,
     ; the input token therefore would be consumed unconditionally.
     (λ [[token : XML-Syntax-Any]]
       (define datum : (XML-Option a) (rnc:filter token))
       (cond [(not datum) false-value]
             [(not (exn:xml? datum)) datum]
             [(not fexn-value) false-value]
             [else (fexn-value datum)]))]))

(define RNC:<^> : (All (a) (-> (U (XML:Filter a) (Listof+ (XML:Filter a))) (XML-Parser (Listof a))))
  (case-lambda
    [(atom-filter)
     (if (pair? atom-filter)
         (apply RNC:<&> atom-filter)
         (RNC:<&> atom-filter))]))

(define RNC:<_> : (All (a) (-> (XML:Filter Any) (XML-Parser a)))
  (lambda [atom-filter]
    (λ [[data : a] [tokens : (Listof XML-Token)]]
      (define-values (head tail) (rnc-car/cdr tokens))
      (define datum : (XML-Option Any) (atom-filter head))
      (cond [(or (exn:xml? datum) (not datum)) (values datum tokens)]
            [else (values data tail)]))))

(define RNC:<~> : (All (a b) (case-> [(XML:Filter a) (-> a b) -> (XML:Filter b)]
                                     [(XML:Filter a) (-> a b) (-> b XML-Syntax-Any (XML-Option True)) -> (XML:Filter b)]))
  (case-lambda
    [(rnc-filter xml->racket)
     (λ [[token : XML-Syntax-Any]]
       (define datum : (XML-Option a) (rnc-filter token))
       (if (exn:xml? datum) datum (and datum (xml->racket datum))))]
    [(rnc-filter xml->racket datum-filter)
     (λ [[token : XML-Syntax-Any]]
       (define datum : (XML-Option a) (rnc-filter token))
       (cond [(or (not datum) (exn:xml? datum)) datum]
             [else (let* ([rdatum (xml->racket datum)]
                          [okay? (datum-filter rdatum token)])
                     (cond [(or (not okay?) (exn:xml? okay?)) okay?]
                           [else rdatum]))]))]))

(define RNC:<&> : (All (a) (-> (XML:Filter a) * (XML-Parser (Listof a))))
  ;;; https://drafts.csswg.org/css-values-4/#component-combinators [juxtaposing components]
  (case-lambda
    [() values]
    [(atom-filter)
     (λ [[data : (Listof a)] [tokens : (Listof XML-Token)]]
       (define-values (head tail) (rnc-car/cdr tokens))
       (define datum : (XML-Option a) (atom-filter head))
       (cond [(or (not datum) (exn:xml? datum)) (values datum tokens)]
             [else (values (cons datum data) tail)]))]
    [atom-filters
     (λ [[data : (Listof a)] [tokens : (Listof XML-Token)]]
       (let juxtapose ([data++ : (Listof a) data]
                       [tokens-- : (Listof XML-Token) tokens]
                       [filters : (Listof (XML:Filter a)) atom-filters])
         (cond [(null? filters) (values data++ tokens--)]
               [else (let ([rnc-filter (car filters)])
                       (define-values (token --tokens) (rnc-car/cdr tokens--))
                       (define datum : (XML-Option a) (rnc-filter token))
                       (cond [(or (not datum) (exn:xml? datum)) (values datum tokens--)]
                             [else (juxtapose (cons datum data++) --tokens (cdr filters))]))])))]))

(define RNC:<*> : (All (a) (->* ((XML:Filter a)) ((U (XML-Multiplier Index) '+ '? '*)) (XML-Parser (Listof a))))
  ;;; https://drafts.csswg.org/css-values/#mult-zero-plus
  (lambda [atom-filter [multiplier '*]]
    (define-values (least most) (rnc:multiplier-range multiplier 0))
    (cond [(zero? most) values]
          [else (λ [[data : (Listof a)] [tokens : (Listof XML-Token)]]
                  (let mult-0+ ([data++ : (Listof a) data]
                                [tokens-- : (Listof XML-Token) tokens]
                                [n+1 : Natural 1])
                    (define-values (token --tokens) (rnc-car/cdr tokens--))
                    (define datum : (XML-Option a) (atom-filter token))
                    (cond [(or (not datum) (exn:xml? datum)) (values (if (< least n+1) data++ datum) tokens--)]
                          [(= n+1 most) (values (cons datum data++) --tokens)] ; (= n +inf.0) also does not make much sense
                          [else (mult-0+ (cons datum data++) --tokens (add1 n+1))])))])))

(define RNC:<!> : (All (a) (->* ((XML:Filter a)) ((U (XML-Multiplier Positive-Index) '+)) (XML-Parser (Listof Any))))
  ;;; (WARNING: this is *not*) https://drafts.csswg.org/css-values/#mult-req
  (lambda [atom-filter [multiplier '+]]
    (define-values (least most) (rnc:multiplier-range multiplier 1))
    (λ [[data : (Listof Any)] [tokens : (Listof XML-Token)]]
      (let not-mult-req ([sub++ : (Listof a) null]
                         [tokens-- : (Listof XML-Token) tokens]
                         [n+1 : Natural 1])
        (define-values (token --tokens) (rnc-car/cdr tokens--))
        (define datum : (XML-Option a) (atom-filter token))
        (if (or (not datum) (exn:xml? datum))
            (cond [(< least n+1) (values (cons (reverse sub++) data) tokens--)]
                  [else (values datum --tokens)])
            (cond [(= n+1 most) (values (cons (reverse (cons datum sub++)) data) --tokens)]
                  [else (not-mult-req (cons datum sub++) --tokens (add1 n+1))]))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define RNC<^> : (All (a) (-> (XML-Parser (Listof a)) (XML-Parser (Listof Any))))
  (case-lambda
    [(atom-parser) ; equivalent to casting `(XML-Parser (Listof a))` into `(XML-Parser (Listof Any))` for `XML-Declaration-Parser`
     (λ [[data : (Listof Any)] [tokens : (Listof XML-Token)]]
       (define-values (datum --tokens) (atom-parser null tokens))
       (cond [(or (not datum) (exn:xml? datum)) (values datum --tokens)]
             [else (values (append datum data) --tokens)]))]))

(define RNC<$> : (case-> [(XML-Parser (Listof Any)) -> (XML-Parser (Listof Any))]
                         [(XML-Parser (Listof Any)) Any -> (XML-Parser (Listof Any))])
  (case-lambda
    [(rnc-parser)
     (λ [[data : (Listof Any)] [tokens : (Listof XML-Token)]]
       (cond [(pair? tokens) (rnc-parser data tokens)]
             [else (values data null)]))]
    [(rnc-parser eof-value)
     (λ [[data : (Listof Any)] [tokens : (Listof XML-Token)]]
       (cond [(pair? tokens) (rnc-parser data tokens)]
             [else (values (cons eof-value data) null)]))]))

(define RNC<~> : (All (a b) (case-> [(XML-Parser (Listof a)) (-> (Listof a) b) -> (XML-Parser (Listof b))]
                                    [(XML-Parser (Listof a)) (-> (Listof a) b) (-> (Listof b) b (Listof XML-Token) (XML-Option True)) -> (XML-Parser (Listof b))]))
  (case-lambda
    [(rnc-parser rnc->racket)
     (λ [[data : (Listof b)] [tokens : (Listof XML-Token)]]
       (define-values (datum --tokens) (rnc-parser null tokens))
       (cond [(or (exn:xml? datum) (false? datum)) (values datum --tokens)]
             [else (values (cons (rnc->racket (reverse datum)) data) --tokens)]))]
    [(rnc-parser rnc->racket data-filter)
     (λ [[data : (Listof b)] [tokens : (Listof XML-Token)]]
       (define-values (datum --tokens) (rnc-parser null tokens))
       (cond [(or (exn:xml? datum) (false? datum)) (values datum --tokens)]
             [else (let* ([rdatum (rnc->racket (reverse datum))]
                          [okay? (data-filter data rdatum (if (eq? tokens --tokens) null (drop-right tokens (length --tokens))))])
                     (cond [(or (not okay?) (exn:xml? okay?)) (values okay? tokens)]
                           [else (values (cons rdatum data) --tokens)]))]))]))

(define RNC<_> : (All (a) (-> (XML-Parser (Listof Any)) (XML-Parser a)))
  (lambda [rnc-parser]
    (λ [[data : a] [tokens : (Listof XML-Token)]]
      (define-values (++data --tokens) (rnc-parser null tokens))
      (cond [(or (exn:xml? ++data) (not ++data)) (values ++data --tokens)]
            [else (values data --tokens)]))))
  
(define RNC<*> : (All (a) (->* ((XML-Parser a)) ((U (XML-Multiplier Index) '+ '? '*)) (XML-Parser a)))
  ;;; https://drafts.csswg.org/css-values/#mult-zero-plus
  (lambda [rnc-parser [multiplier '*]]
    (define-values (least most) (rnc:multiplier-range multiplier 0))
    (cond [(zero? most) values]
          [else (λ [[data : a] [tokens : (Listof XML-Token)]]
                  (let mult-0+ ([data++ : a data]
                                [tokens-- : (Listof XML-Token) tokens]
                                [n+1 : Natural 1])
                    (define-values (++data --tokens) (rnc-parser data++ tokens--))
                    (cond [(or (not ++data) (exn:xml? ++data)) (if (< least n+1) (values data++ tokens--) (values ++data --tokens))]
                          [(= n+1 most) (values ++data --tokens)] ; (= n +inf.0) also does not make much sense
                          [else (mult-0+ ++data --tokens (add1 n+1))])))])))

(define RNC<!> : (All (a) (->* ((XML-Parser (Listof a))) ((U (XML-Multiplier Positive-Index) '+)) (XML-Parser (Listof Any))))
  ;;; (WARNING: this is *not*) https://drafts.csswg.org/css-values/#mult-req
  (lambda [rnc-parser [multiplier '+]]
    (define-values (least most) (rnc:multiplier-range multiplier 1))
    (λ [[data : (Listof Any)] [tokens : (Listof XML-Token)]]
      (let not-mult-req ([subdata++ : (Listof a) null]
                         [tokens-- : (Listof XML-Token) tokens]
                         [n+1 : Natural 1])
        (define-values (subdata --tokens) (rnc-parser subdata++ tokens--))
        (if (or (not subdata) (exn:xml? subdata))
            (cond [(< least n+1) (values (cons (reverse subdata++) data) tokens--)]
                  [else (values subdata --tokens)])
            (cond [(= n+1 most) (values (cons (reverse subdata) data) --tokens)]
                  [else (not-mult-req subdata --tokens (add1 n+1))]))))))

(define RNC<λ> : (All (a) (-> (-> (XML-Parser (Listof a))) (XML-Parser (Listof a))))
  (lambda [rnc-parser]
    (λ [[data : (Listof a)] [tokens : (Listof XML-Token)]]
      ((rnc-parser) data tokens))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define rnc:disjoin : (All (a b c) (case-> [(XML:Filter a) (XML:Filter b) -> (XML:Filter (U a b))]))
  (case-lambda
    [(rnc-filter1 rnc-filter2)
     (λ [[token : XML-Syntax-Any]]
       (define datum : (XML-Option a) (rnc-filter1 token))
       (cond [(nor (not datum) (exn:xml? datum)) datum]
             [else (rnc-filter2 token)]))]))

(define rnc-disjoin : (All (a b) (case-> [(Listof (XML-Parser a)) -> (XML-Parser a)]
                                         [(XML-Parser (Listof a)) (XML-Parser (Listof b)) -> (XML-Parser (Listof (U a b)))]))
  ;;; https://drafts.csswg.org/css-values/#comb-one
  (case-lambda
    [(atom-parsers)
     (cond [(null? atom-parsers) values]
           [(null? (cdr atom-parsers)) (car atom-parsers)]
           [else (λ [[data : a] [tokens : (Listof XML-Token)]]
                   (let combine-one ([head-parser : (XML-Parser a) (car atom-parsers)]
                                     [tail-parsers : (Listof (XML-Parser a)) (cdr atom-parsers)])
                     (define-values (++data --tokens) (head-parser data tokens))
                     (cond [(nor (not ++data) (exn:xml? ++data)) (values ++data --tokens)]
                           [(pair? tail-parsers) (combine-one (car tail-parsers) (cdr tail-parsers))]
                           [else (values ++data --tokens)])))])]
    [(atom-parser1 atom-parser2)
     (λ [[data : (Listof (U a b))] [tokens : (Listof XML-Token)]]
       (define-values (datum --tokens) (atom-parser1 null tokens))
       (cond [(nor (not datum) (exn:xml? datum)) (values (append datum data) --tokens)]
             [else (let-values ([(datum --tokens) (atom-parser2 null tokens)])
                     (cond [(or (not datum) (exn:xml? datum)) (values datum --tokens)]
                           [else (values (append datum data) --tokens)]))]))]))

(define rnc-juxtapose : (All (a b) (case-> [(Listof (XML-Parser a)) -> (XML-Parser a)]
                                           [(XML-Parser (Listof a)) (XML-Parser (Listof b)) -> (XML-Parser (Listof (U a b)))]))
  ;;; https://drafts.csswg.org/css-values-4/#component-combinators [juxtaposing components]
  (case-lambda
    [(atom-parsers)
     (λ [[data : a] [tokens : (Listof XML-Token)]]
       (let combine-all ([data++ : a data]
                         [tokens-- : (Listof XML-Token) tokens]
                         [parsers : (Listof (XML-Parser a)) atom-parsers])
         (cond [(null? parsers) (values data++ tokens--)]
               [else (let-values ([(++data --tokens) ((car parsers) data++ tokens--)])
                       (cond [(or (not ++data) (exn:xml? ++data)) (values ++data --tokens)]
                             [else (combine-all ++data --tokens (cdr parsers))]))])))]
    [(atom-parser1 atom-parser2)
     (λ [[data : (Listof (U a b))] [tokens : (Listof XML-Token)]]
       (define-values (datum1 --tokens) (atom-parser1 null tokens))
       (cond [(or (not datum1) (exn:xml? datum1)) (values datum1 --tokens)]
             [else (let-values ([(datum2 ----tokens) (atom-parser2 null --tokens)])
                     (cond [(or (not datum2) (exn:xml? datum2)) (values datum2 ----tokens)]
                           [else (values (append datum2 datum1 data) ----tokens)]))]))]))

(define rnc:if : (All (a) (case-> [(Listof+ (Pairof (XML:Filter Any) (XML-Parser a))) (Option (XML-Parser a)) -> (XML-Parser a)]
                                  [(XML:Filter Any) (XML-Parser a) (Option (XML-Parser a)) -> (XML-Parser a)]))
  (case-lambda
    [(cond-parsers else-parser)
     (λ [[data : a] [tokens : (Listof XML-Token)]]
       (let else-if ([branch (car cond-parsers)]
                     [branches-- (cdr cond-parsers)])
         (define-values (if:filter then-parser) (values (car branch) (cdr branch)))
         (define-values (token --tokens) (rnc-car/cdr tokens))
         (define if:datum : (XML-Option Any) (if:filter token))
         (cond [(nor (not if:datum) (exn:xml? if:datum)) (then-parser data --tokens)]
               [(pair? branches--) (else-if (car branches--) (cdr branches--))]
               [(not else-parser) (values if:datum tokens)]
               [else (else-parser data tokens)])))]
    [(if:filter then-parser else-parser)
     (λ [[data : a] [tokens : (Listof XML-Token)]]
       (define-values (token --tokens) (rnc-car/cdr tokens))
       (define if:datum : (XML-Option Any) (if:filter token))
       (cond [(nor (not if:datum) (exn:xml? if:datum)) (then-parser data --tokens)]
             [(not else-parser) (values if:datum tokens)]
             [else (else-parser data tokens)]))]))

(define rnc:multiplier-range : (-> (U (XML-Multiplier Index) '+ '? '*) Index (Values Natural (U Natural +inf.0)))
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
(define-rnc-atomic-filter <rnc:assign> #:-> Char #:with [[token : xml:eq?]] #:on-error make-exn:rnc:missing-delim #\=)

(define #:forall (a) (<:=:>) : (XML-Parser a) ((inst RNC<_> a) (RNC:<^> (<rnc:assign>))))
(define #:forall (a) (<:~:>) : (XML-Parser a) ((inst RNC<_> a) (RNC:<^> (<xml:delim> #\~))))
(define #:forall (a) (<:-:>) : (XML-Parser a) ((inst RNC<_> a) (RNC:<^> (<xml:name> '-))))
(define #:forall (a) (<:*:>) : (XML-Parser a) ((inst RNC<_> a) (RNC:<^> (<xml:delim> #\*))))
(define #:forall (a) (<:>:>) : (XML-Parser a) ((inst RNC<_> a) (RNC:<^> (<xml:delim> #\>))))

(define-rnc-disjoint-filter <rnc:keyword> #:-> Keyword
  #:with [[options : (U (-> Keyword Boolean) (Listof Keyword) Keyword)]]
  (<xml:pereference> options))

(define-rnc-disjoint-filter <rnc:id> #:-> Symbol
  (<xml:name> xml-ncname?)
  #;(RNC:<~> (<xml:pereference> null) keyword->symbol))

(define-rnc-disjoint-filter <rnc:cname> #:-> Symbol
  (<xml:name> xml-cname?))

(define-rnc-disjoint-filter <rnc:nsname> #:-> Symbol
  (<xml:name> xml-nsname?))

(define-rnc-disjoint-filter <rnc:id-or-keyword> #:-> Symbol
  (<xml:name> xml-ncname?)
  (RNC:<~> (<xml:pereference>) keyword->symbol))

(define-rnc-disjoint-filter <rnc:inherit> #:-> Symbol
  (RNC:<=> (<xml:pereference> '#:inherit) 'inherit))

(define-rnc-disjoint-filter <rnc:name> #:-> Symbol
  (<xml:name>)
  (<rnc:id-or-keyword>))

(define-rnc-disjoint-filter <rnc:literal> #:-> String
  #:with [[options : (U (-> String Boolean) (Listof String) String)]]
  (<xml:string> options))

(define-rnc-disjoint-filter <rnc:assign-method> #:-> Char
  (<rnc:assign>)
  (RNC:<=> (<xml:delim> #\&) #\&)
  (RNC:<=> (<xml:delim> #\λ #| not a typo |#) #\|))

(define (<:inherit:>) : (XML-Parser (Listof Symbol))
  (RNC<&> ((inst RNC<_> (Listof Symbol)) (RNC:<^> (<xml:pereference> '#:inherit)))
          ((inst <:=:> (Listof Symbol)))
          (RNC:<^> (<rnc:id-or-keyword>))))

(define (<:rnc-literal:>) : (XML-Parser (Listof String))
  (RNC<~> (RNC<&> (RNC:<^> (<xml:string>))
                  (RNC<*> (RNC<&> ((inst <:~:> (Listof String)))
                                  (RNC:<^> (<xml:string>))) '*))
          (λ [[segments : (Listof String)]] : String
            (apply string-append segments))))

(define (<:rnc-ns:literal:>) : (XML-Parser (Listof (U String Symbol)))
  (RNC<+> (<:rnc-literal:>)
          (RNC:<^> (<rnc:inherit>))))

(define <:rnc-name=value:> : (All (a) (-> (XML:Filter Symbol) (-> Symbol String a) (XML-Parser (Listof a))))
  (lambda [<name> rnc->racket]
    (RNC<~> (RNC<&> (RNC:<^> <name>) ((inst <:=:> (Listof (U String Symbol)))) (<:rnc-literal:>))
            (λ [[data : (Listof (U Symbol String))]] : a
              (cond [(or (null? data) (null? (cdr data))) (rnc->racket 'dead "code")]
                    [else (rnc->racket (assert (car data) symbol?) (assert (cadr data) string?))])))))

(define <:rnc-annotation:> : (All (d a b c) (-> (Option (XML-Parser (Listof a))) (XML-Parser (Listof b)) (Option (XML-Parser (Listof c)))
                                                (-> (Option a) b (Listof c) d) (XML-Parser (Listof (U b d)))))
  (lambda [<:initial:> <:body:> <:follow:> rnc->racket]
    (define <:initial?:> (if (not <:initial:>) values <:initial:>)) ; we won't check the result, so no need using of `RNC<*>`
    (define <:follow*:> (and <:follow:> (RNC<*> <:follow:> '*)))
    
    (λ [[data : (Listof (U b d))] [tokens : (Listof XML-Token)]]
      (define-values (initial body-tokens) (<:initial?:> null tokens))
      (define-values (body follow-tokens) (<:body:> null body-tokens))
      
      (cond [(not (pair? body)) (values body follow-tokens)]
            [(not <:follow*:>) (values (cons (if (pair? initial) (rnc->racket (car initial) (car body) null) (car body)) data) follow-tokens)]
            [else (let-values ([(follows rest) (<:follow*:> null follow-tokens)])
                    (values (cons (cond [(not (or (pair? initial) (pair? follows))) (car body)]
                                        [else (rnc->racket (and (pair? initial) (car initial)) (car body) (if (list? follows) follows null))])
                                  data)
                            rest))]))))

(define <:rnc-bracket:> : (All (a) (->* ((XML-Parser (Listof a))) ((U False (XML-Multiplier Index) '+ '? '*) Char Char) (XML-Parser (Listof a))))
  (lambda [<:body:> [multiplier #false] [open #\[] [close #\]]]
    (if (not multiplier)
        (RNC<&> ((inst RNC:<_> (Listof a)) (<xml:delim> open))
                <:body:>
                ((inst RNC:<_> (Listof a)) (<xml:delim> close)))
        (RNC<&> ((inst RNC:<_> (Listof a)) (<xml:delim> open))
                (RNC<*> <:body:> multiplier)
                ((inst RNC:<_> (Listof a)) (<xml:delim> close))))))

(define <:rnc-parenthesis:> : (All (a) (->* ((XML-Parser (Listof a))) ((U False (XML-Multiplier Index) '+ '? '*)) (XML-Parser (Listof a))))
  (lambda [<:body:> [multiplier #false]]
    (<:rnc-bracket:> <:body:> multiplier #\( #\))))

(define <:rnc-brace:> : (All (a) (->* ((XML-Parser (Listof a))) ((U False (XML-Multiplier Index) '+ '? '*)) (XML-Parser (Listof a))))
  (lambda [<:body:> [multiplier #false]]
    (<:rnc-bracket:> <:body:> multiplier #\{ #\})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define rnc-car/cdr : (All (a) (-> (Listof a) (Values (Option a) (Listof a))))
  (lambda [dirty]
    (let skip-whitespace ([rest dirty])
      (cond [(null? rest) (values #false null)]
            [else (let-values ([(head tail) (values (car rest) (cdr rest))])
                    (cond [(not (xml:whitespace? head)) (values head tail)]
                          [(not (xml:comment? head)) (skip-whitespace tail)]
                          [(string-prefix? (xml:whitespace-datum head) "#") (values head tail)]
                          [else (skip-whitespace tail)]))]))))
