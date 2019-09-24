#lang typed/racket/base

;;; https://drafts.csswg.org/css-variables

(provide (all-defined-out))

(require "misc.rkt")
(require "digicore.rkt")

(define css-any->declaration-value : (-> CSS-Token (Listof CSS-Token) Boolean
                                         (Values (U (Listof+ CSS-Token) CSS-Syntax-Error) Boolean Boolean))
  ;;; https://drafts.csswg.org/css-syntax/#typedef-declaration-value
  ;;; https://drafts.csswg.org/css-variables/#defining-variables
  (lambda [hint-token components var?]
    (let fold ([lla : (Listof CSS-Token) null]
               [<!> : (Option CSS:Delim) #false]
               [lazy? : Boolean #false]
               [rest : (Listof CSS-Token) components])
      (define-values (head tail) (if <!> (css-car/cdr rest) (css-car rest)))
      (if (eof-object? head)
          (let ([all (reverse lla)]) ; (reverse) does not know non-null list.
            ; whitespace-only <declaration-value>s are also valid, but they are only meaningful for variables.
            (cond [(pair? all) (values all (and (not var?) <!> #true) lazy?)]
                  [(and var? (pair? components)) (values components #false #false)]
                  [else (values (make+exn:css:missing-value hint-token) #false lazy?)]))
          (cond [(and <!> (css:ident-norm=:=? head 'important)) (fold (if var? (cons head lla) lla) <!> lazy? tail)]
                [(and <!>) (values (make+exn:css:unrecognized <!>) #false lazy?)]
                [(css:delim=:=? head #\!) (fold lla head lazy? tail)]
                [(css-bad-token? head) (values (make+exn:css:malformed head) #false lazy?)]
                [else (let-values ([(shadow shadow-lazy? real-tail) (css-declaration-value-filter head tail)])
                        (cond [(exn? shadow) (values shadow #false lazy?)]
                              [else (fold (cons shadow lla) <!> (or lazy? shadow-lazy?) real-tail)]))])))))

(define css-resolve-variables : (-> CSS-Values (Option CSS-Values) CSS-Values)
  ;;; https://drafts.csswg.org/css-variables/#cycles
  (lambda [declared-values inherited-values]
    (define ?inherited-vars : (Option CSS-Variables) (and inherited-values (css-varbase-ref inherited-values)))
    (define ?declared-vars : (Option CSS-Variables)
      (cond [(not ?inherited-vars) (css-varbase-ref declared-values)]
            [else (let ([declared-vars (css-varbase-ref! declared-values)])
                    (for ([(--var --value) (in-hash ?inherited-vars)])
                      (hash-ref! declared-vars --var (λ [] --value)))
                    declared-vars)]))
    (when ?declared-vars
      (for ([(--var --value) (in-hash ?declared-vars)])
        (when (and (css-declaration? --value) (css-declaration-lazy? --value))
          (define property : CSS:Ident (css-declaration-name --value))
          (define --values : (Listof+ CSS-Token) (css-declaration-values --value))
          (define flat-values : (Listof CSS-Token) (css-variable-substitute property --values ?declared-vars (list --var)))
          (hash-set! ?declared-vars --var
                     (cond [(pair? flat-values) (struct-copy css-declaration --value [values flat-values] [lazy? #false])]
                           [else null])))))
    declared-values))

(define css-variable-substitute : (-> CSS:Ident (Listof CSS-Token) (Option CSS-Variables) (Listof Symbol) (Listof CSS-Token))
  ;;; https://drafts.csswg.org/css-variables/#invalid-variables
  (lambda [property var-values varbase refpath]
    (let var-fold ([seulav : (Listof CSS-Token) null]
                   [--values : (Listof CSS-Token) var-values])
      (define-values (head tail) (css-car/cdr --values))
      (cond [(eof-object? head) (if (css-pair? seulav) (reverse (filter-not css:whitespace? seulav)) seulav)]
            [(not (css-lazy-token? head)) (var-fold (cons head seulav) tail)]
            [(and (css:function? head) (css:function-lazy? head))
             (define args : (Listof CSS-Token) (css-variable-substitute property (css:function-arguments head) varbase refpath))
             (if (null? args) null (var-fold (cons (css:function-copy head args #false) seulav) tail))]
            [(and (css:λracket? head) (css:λracket-lazy? head))
             (define args : (Listof CSS-Token) (css-variable-substitute property (css:λracket-arguments head) varbase refpath))
             (define argl : (U (Listof CSS-Token) CSS-Syntax-Error) (css-λarguments-filter args))
             (if (or (null? args) (exn:css? argl)) null (var-fold (cons (css:λracket-copy head argl #false) seulav) tail))]
            [(and (css:url? head) (css:url-lazy? head))
             (define args : (Listof CSS-Token) (css-variable-substitute property (css:url-modifiers head) varbase refpath))
             (define mods : (Listof CSS-URL-Modifier) (css-url-modifiers-filter head args))
             (if (null? args) null (var-fold (cons (css:url-copy head mods #false) seulav) tail))]
            [(and (css:block? head) (css:block-lazy? head))
             (define coms : (Listof CSS-Token) (css-variable-substitute property (css:block-components head) varbase refpath))
             (if (null? coms) null (var-fold (cons (css:block-copy head coms #false) seulav) tail))]
            [(not (css:var? head)) (var-fold (cons head seulav) tail)]
            [(memq (css:var-datum head) refpath) (make+exn:css:cyclic head property 'debug) null]
            [else (let ([--var (css:var-datum head)])
                    (define --value : (U CSS-Declaration Null) (if (not varbase) null (hash-ref varbase --var (λ [] null))))
                    (define-values (--vs lazy?)
                      (cond [(null? --value) (values (css:var-fallback head) (css:var-lazy? head))]
                            [else (values (css-declaration-values --value) (css-declaration-lazy? --value))]))
                    (cond [(null? --vs) (make+exn:css:missing-value head property 'debug) null]
                          [(not lazy?) (var-fold (append (reverse --vs) seulav) tail)]
                          [else (let ([vs (css-variable-substitute property --vs varbase (cons --var refpath))])
                                  (if (null? vs) vs (var-fold (append (reverse vs) seulav) tail)))]))]))))

(define css-function->var : (-> CSS:Function (U CSS:Var CSS-Syntax-Error))
  (lambda [var]
    (define-values (--var ?fallback-list) (css-car (css:function-arguments var)))
    (define-values (?comma fallback) (css-car ?fallback-list))
    (cond [(not (css:ident=<-? --var symbol-unreadable?)) (make+exn:css:type:variable --var)]
          [(eof-object? ?comma) (css-remake-token var css:var (css:ident-datum --var) null #false)]
          [(not (css:comma? ?comma)) (make+exn:css:missing-comma ?comma)]
          [else (let-values ([(?fallback _ lazy?) (css-any->declaration-value ?comma fallback #true)])
                  (cond [(exn? ?fallback) ?fallback]
                        [else (css-remake-token var css:var (css:ident-datum --var) ?fallback lazy?)]))])))

(define css-declaration-value-filter : (-> CSS-Token (Listof CSS-Token)
                                           (Values (U CSS-Token CSS-Syntax-Error) Boolean (Listof CSS-Token)))
  (lambda [token candidates]
    (cond [(css:url? token)
           (define-values (submodifiers lazy?) (css-lazy-subtokens-map (css:url-modifiers token)))
           (define url : (U CSS:URL CSS-Syntax-Error)
             (cond [(exn:css? submodifiers) submodifiers]
                   [(not lazy?) token]
                   [else (css:url-copy token (css-url-modifiers-filter token submodifiers) lazy?)]))
           (values url (not (eq? token url)) candidates)]
          [(css:function? token)
           (define func : (U CSS:Function CSS:Var CSS-Syntax-Error)
             (cond [(eq? (css:function-norm token) 'var) (css-function->var token)]
                   [else (let-values ([(subarguments lazy?) (css-lazy-subtokens-map (css:function-arguments token))])
                           (cond [(exn:css? subarguments) subarguments]
                                 [(not lazy?) token]
                                 [else (css:function-copy token subarguments lazy?)]))]))
           (values func (not (eq? token func)) candidates)]
          [(css:@keyword? token)
           (define-values (next rest) (css-car/cdr candidates))
           (define binding : Symbol (string->symbol (substring (keyword->string (css:@keyword-datum token)) 1)))
           (if (or (eof-object? next) (not (css:block? next)))
               (values (css-remake-token token css:racket binding) #false candidates)
               (let-values ([(argl lazy?) (css-lazy-subtokens-map (filter-not css:whitespace? (css:block-components next)))])
                 (cond [(exn:css? argl) (values argl #false candidates)]
                       [(and lazy?) (values (css-remake-token [token next] css:λracket binding argl lazy?) lazy? rest)]
                       [else (let ([reargl (css-λarguments-filter argl)])
                               (cond [(exn:css? reargl) (values reargl #false candidates)]
                                     [else (values (css-remake-token [token next] css:λracket binding reargl lazy?) lazy? rest)]))])))]
          [(css:block? token)
           (define-values (subcomponents lazy?) (css-lazy-subtokens-map (css:block-components token)))
           (define block : (U CSS:Block CSS-Syntax-Error)
             (cond [(exn:css? subcomponents) subcomponents]
                   [(not lazy?) token]
                   [else (css:block-copy token subcomponents lazy?)]))
           (values block (not (eq? token block)) candidates)]
          [else (values token #false candidates)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type CSS-Variables (HashTable Symbol (U CSS-Declaration Null)))

(define --varbases : (HashTable CSS-Values CSS-Variables) ((inst make-hasheq CSS-Values CSS-Variables)))

(define css-varbase-ref : (-> CSS-Values (Option CSS-Variables))
  (lambda [which]
    (hash-ref --varbases which (λ [] #false))))

(define css-varbase-ref! : (-> CSS-Values CSS-Variables)
  (lambda [which]
    (hash-ref! --varbases which
               (λ [] ((inst make-hasheq Symbol (U CSS-Declaration Null)))))))

(define varbase-set! : (-> CSS-Values Symbol (U CSS-Declaration Null) Void)
  (lambda [which --var --value]
    (hash-set! (css-varbase-ref! which) --var --value)))

(define css-lazy-subtokens-map : (-> (Listof CSS-Token) (Values (U (Listof CSS-Token) CSS-Syntax-Error) Boolean))
  (lambda [subtokens]
    (let lazy-fold ([tokens : (Listof CSS-Token) null]
                    [lazy? : Boolean #false]
                    [subrest : (Listof CSS-Token) subtokens])
      (cond [(null? subrest) (values (reverse tokens) lazy?)]
            [else (let ([head (car subrest)])
                    (define-values (shadow shadow-lazy? tail) (css-declaration-value-filter head (cdr subrest)))
                    (cond [(exn:css? shadow) (values shadow #false)]
                          [else (lazy-fold (cons shadow tokens) (or lazy? shadow-lazy?) tail)]))]))))

(define css-λarguments-filter : (-> (Listof CSS-Token) (U (Listof CSS-Token) CSS-Syntax-Error))
  (lambda [argl]
    (let rearrange ([swk : (Listof CSS-Token) null]
                    [lgra : (Listof CSS-Token) null]
                    [tail : (Listof CSS-Token) argl])
      (define-values (head rest) (css-car/cdr tail))
      (cond [(eof-object? head) (append (reverse swk) (reverse lgra))]
            [(css:delim=:=? head #\#)
             (define-values (?: :kw+rest) (css-car/cdr rest))
             (define-values (?kw value+rest) (css-car/cdr :kw+rest))
             (define-values (kw-value others) (css-car/cdr value+rest))
             (cond [(or (eof-object? ?:) (not (css:colon? ?:))) (rearrange swk (cons head lgra) rest)]
                   [(or (eof-object? ?kw) (not (css:ident? ?kw))) (rearrange swk (list* ?: head lgra) :kw+rest)]
                   [else (let* ([:kw (string->keyword (symbol->string (css:ident-datum ?kw)))]
                                [<#:kw> (css-remake-token [head ?kw] css:#:keyword :kw)])
                           (cond [(eof-object? kw-value) (make+exn:css:missing-value <#:kw>)]
                                 [else (rearrange (cons kw-value (cons <#:kw> swk)) lgra others)]))])]
            [else (rearrange swk (cons head lgra) rest)]))))

(define css-url-modifiers-filter : (-> CSS-Token (Listof CSS-Token) (Listof CSS-URL-Modifier))
  (lambda [url modifiers]
    (let modifiers-filter ([sreifidom : (Listof CSS-URL-Modifier) null]
                           [tail : (Listof CSS-Token) modifiers])
      (define-values (head rest) (css-car tail))
      (cond [(eof-object? head) (reverse sreifidom)]
            [(or (css:ident? head) (css-lazy-token? head)) (modifiers-filter (cons head sreifidom) rest)]
            [else (make+exn:css:type (list url head)) (modifiers-filter sreifidom rest)]))))
