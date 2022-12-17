#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out racket/flonum racket/fixnum racket/bool racket/list racket/format))
(provide (struct-out SYN-Token) syn-token-port-location syn-remake-token syn-token-skip-whitespace)

(require racket/fixnum)
(require racket/flonum)
(require racket/list)
(require racket/bool)
(require racket/format)
(require racket/symbol)
(require racket/keyword)
(require racket/match)

(require digimon/token)

(require "misc.rkt")

(require (for-syntax racket/base))
(require (for-syntax racket/string))
(require (for-syntax racket/symbol))
(require (for-syntax racket/syntax))
(require (for-syntax syntax/parse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(begin-for-syntax
  (define-splicing-syntax-class opt
    #:attributes (mutable? transparent? prefab? cname ecname [prop 1] [prop-val 1])
    (pattern (~seq (~or (~optional (~seq (~and #:mutable mutable?)))
                        (~optional (~seq (~and #:transparent transparent?)))
                        (~optional (~seq (~and #:prefab prefab?)))
                        (~optional (~or (~and (~seq #:constructor-name cname)
                                              (~bind [ecname #f]))
                                        (~and (~seq #:extra-constructor-name ecname)
                                              (~bind [cname #f]))))
                        
                        (~optional (~seq #:guard guard:expr))
                        (~seq #:property prop:expr prop-val:expr))
                   ...))))

(define-syntax (define-preference stx)
  (syntax-parse stx #:literals [:]
    [(self preference : Preference (fields ...) options ...)
     (syntax/loc stx (self preference : Preference #:with [] (fields ...) options ...))]
    [(self preference : Preference #:with [[bindings BindTypes ...] ...]
           ([property : DataType info ...] ...)
           options:opt)
     (with-syntax* ([make-preference (format-id #'preference "make-~a" (syntax-e #'preference))]
                    [(Uv uv? uv) (list #'CSS-Wide-Keyword #'css-wide-keyword? #'css:initial)]
                    [([initial-value property-filter ArgumentType defval] ...)
                     (for/list ([field-info (in-list (syntax->list #'([property DataType info ...] ...)))])
                       (syntax-parse field-info
                         [(p T #:= dv #:~> Super fltr) #'[(fltr dv) (if (uv? p) (fltr dv) (fltr p)) (U Super Uv) uv]]
                         [(p T #:= dv #:~> fltr) #'[(fltr dv) (if (uv? p) (fltr dv) (fltr p)) Any uv]]
                         [(p T #:= dv) #'[dv (if (uv? p) dv p) (U T Uv) uv]]
                         [(p rest ...) (raise-syntax-error (syntax-e #'self) "property requires an initial value" #'p)]))]
                    [(#%property ...)
                     (for/list ([property (in-list (syntax->list #'(property ...)))])
                       (format-id property "#%~a-~a" (syntax-e #'preference) (syntax-e property)))]
                    [(args ...)
                     (for/fold ([args null])
                               ([argument (in-list (syntax->list #'([property : ArgumentType defval] ...)))])
                       (cons (datum->syntax argument (string->keyword (symbol->immutable-string (car (syntax->datum argument)))))
                             (cons argument args)))]
                    [([pref:bindings properties ...] ...)
                     (for/list ([binding (in-list (syntax->list #'(bindings ...)))]
                                [Types (in-list (syntax->list #'([BindTypes ...] ...)))])
                       (define types (syntax->datum Types))
                       (cons (format-id #'preference "~a:~a" (syntax-e #'preference) (syntax-e binding))
                             (for/fold ([properties null])
                                       ([property (in-list (syntax->list #'(property ...)))]
                                        [type (in-list (syntax->list #'(DataType ...)))])
                               (cond [(not (memq (syntax-e type) types)) properties]
                                     [else (cons (syntax-e property) properties)]))))])
       (quasisyntax/loc stx
         (begin (struct preference ([property : DataType] ...) #:type-name Preference . options)
                (define (make-preference args ...) : Preference (preference property-filter ...))
                (define (#%property) : DataType initial-value) ...
                (define pref:bindings : (Listof Symbol) (list 'properties ...)) ...)))]))

(define-syntax (define-token stx)
  (syntax-parse stx #:literals [: Symbol Keyword]
    [(_ id : Number parent #:as Type #:=? type=? #:with id? id-datum)
     (with-syntax ([id=? (format-id #'id "~a=?" (syntax-e #'id))])
       (syntax/loc stx
         (begin (struct id parent ([datum : Type]) #:transparent #:type-name Number)
                (define (id=? [t1 : Number] [t2 : Number]) : Boolean (type=? (id-datum t1) (id-datum t2)))
                (define-token-interface id : Type id? id-datum #:+ Number #:= type=? #:for CSS-Syntax-Any #:throw exn:css:range))))]
    [(_ id : Identifier parent ((~and (~or Symbol Keyword) Type) #:ci rest ...) #:with id? id-datum)
     (with-syntax ([id=? (format-id #'id "~a=?" (syntax-e #'id))]
                   [id-norm=? (format-id #'id "~a-norm=?" (syntax-e #'id))]
                   [id-norm (format-id #'id "~a-norm" (syntax-e #'id))])
       (syntax/loc stx
         (begin (struct id parent ([datum : Type] [norm : Type] rest ...) #:transparent #:type-name Identifier)
                (define (id=? [t1 : Identifier] [t2 : Identifier]) : Boolean (eq? (id-datum t1) (id-datum t2)))
                (define (id-norm=? [t1 : Identifier] [t2 : Identifier]) : Boolean (eq? (id-norm t1) (id-norm t2)))
                (define-token-interface id : Type id? id-datum #:+ Identifier #:eq? eq? #:for CSS-Syntax-Any #:throw exn:css:range)
                (define-token-interface id-norm : Type id? id-norm  #:+ Identifier #:eq? eq? #:for CSS-Syntax-Any #:throw exn:css:range))))]
    [(_ id : Otherwise parent (Type rest ...) #:with id? id-datum)
     (with-syntax ([type=? (case (syntax-e #'Type) [(String) #'string=?] [(Char) #'char=?] [else #'equal?])]
                   [id=? (format-id #'id "~a=?" (syntax-e #'id))])
       (syntax/loc stx
         (begin (struct id parent ([datum : Type] rest ...) #:transparent #:type-name Otherwise)
                (define (id=? [t1 : Otherwise] [t2 : Otherwise]) : Boolean (type=? (id-datum t1) (id-datum t2)))
                (define-token-interface id : Type id? id-datum #:+ Otherwise #:eq? type=? #:for CSS-Syntax-Any #:throw exn:css:range))))]))

(define-syntax (define-symbolic-tokens stx)
  (syntax-parse stx
    [(_ token #:+ Token [id #:+ ID #:as Type rest ...] ...)
     (with-syntax ([token->datum (format-id #'token "~a->datum" (syntax-e #'token))]
                   [Token-Datum (format-id #'token "~a-Datum" (syntax-e #'Token))]
                   [([id? id-datum] ...)
                    (for/list ([<id> (in-list (syntax->list #'(id ...)))])
                      (list (format-id <id> "~a?" (syntax-e <id>))
                            (format-id <id> "~a-datum" (syntax-e <id>))))])
       (syntax/loc stx
         (begin (struct token css-token () #:transparent #:type-name Token)
                (define-token id : ID token (Type rest ...) #:with id? id-datum) ...
                (define-type Token-Datum (U Type ...))
                (define (token->datum [t : Token]) : (Option Token-Datum) (cond [(id? t) (id-datum t)] ... [else #false])))))]))

(define-syntax (define-lazy-tokens stx)
  (syntax-parse stx
    [(_ token #:+ Token [id #:+ ID #:with components #:as Type ...] ...)
     (with-syntax ([([id? id-copy Component] ...)
                    (for/list ([<id> (in-list (syntax->list #'(id ...)))])
                      (list (format-id <id> "~a?" (syntax-e <id>))
                            (format-id <id> "~a-copy" (syntax-e <id>))
                            (if (eq? (syntax-e <id>) 'css:url) #'CSS-URL-Modifier #'CSS-Token)))])
       (syntax/loc stx
         (begin (define-symbolic-tokens token #:+ Token [id #:+ ID #:as Type ... [components : (Listof Component)] [lazy? : Boolean]] ...)

                (define id-copy : (-> ID (Listof Component) Boolean ID)
                  (lambda [instance subcoms ?]
                    (struct-copy id instance [components (if (css-pair? subcoms) subcoms null)] [lazy? ?])))
                ...)))]))

(define-syntax (define-numeric-tokens stx)
  (syntax-case stx []
    [(_ token #:+ Token #:nan nan [id #:+ ID #:as Type] ...)
     (with-syntax ([token->datum (format-id #'token "~a->datum" (syntax-e #'token))]
                   [([id? id=? id-datum type=?] ...)
                    (for/list ([<id> (in-list (syntax->list #'(id ...)))]
                               [<type> (in-list (syntax->list #'(Type ...)))])
                      (list (format-id <id> "~a?" (syntax-e <id>))
                            (format-id <id> "~a=?" (syntax-e <id>))
                            (format-id <id> "~a-datum" (syntax-e <id>))
                            (let ([type-name (symbol->immutable-string (syntax-e <type>))])
                              (cond [(string-contains? type-name "Flonum") #'fl=]
                                    [(string-contains? type-name "Fixnum") #'fx=]
                                    [else #'=]))))])
       (syntax/loc stx
         (begin (struct token css-numeric () #:transparent #:type-name Token)
                (define-token id : ID token #:as Type #:=? type=? #:with id? id-datum) ...
                (define (token->datum [t : Token]) : (U Type ...) (cond [(id? t) (id-datum t)] ... [else nan])))))]))
  
(define-syntax (define-tokens stx)
  (syntax-case stx []
    [(_ token header #:+ Token
        [[ptoken #:+ PToken #:-> pparent pfields] ...]
        [[ctoken #:+ CToken #:-> cparent] ...]
        (define-typical-tokens group #:+ Group rest ...) ...)
     (with-syntax ([token->datum (format-id #'token "~a->datum" (syntax-e #'token))]
                   [Token-Datum (format-id #'Token "~a-Datum" (syntax-e #'Token))]
                   [([type? type->datum] ...)
                    (for/list ([<type> (in-list (syntax->list #'(group ...)))]
                               #:unless (eq? (syntax-e <type>) 'css:dimension))
                      (list (format-id <type> "~a?" (syntax-e <type>))
                            (format-id <type> "~a->datum" (syntax-e <type>))))]
                   [(Symbolic-Datum ...)
                    (for/list ([<define> (in-list (syntax->list #'(define-typical-tokens ...)))]
                               [<Type> (in-list (syntax->list #'(Group ...)))]
                               #:when (eq? (syntax-e <define>) 'define-symbolic-tokens))
                      (format-id <Type> "~a-Datum" (syntax-e <Type>)))])
       (syntax/loc stx
         (begin (struct token header () #:transparent #:type-name Token)
                (struct ptoken pparent pfields #:transparent #:type-name PToken) ...
                (define-typical-tokens group #:+ Group rest ...) ...
                (struct ctoken cparent () #:transparent #:type-name CToken) ...

                (define-type Token-Datum (U False Number (Pairof Number Symbol) Symbolic-Datum ...))
                (define token->datum : (-> Token Token-Datum)
                  (lambda [instance]
                    (cond [(css:dimension? instance) (cons (css:dimension-datum instance) (css:dimension-unit instance))]
                          [(type? instance) (type->datum instance)] ...
                          [else (assert (object-name instance) symbol?)]))))))]))

;;; https://drafts.csswg.org/css-syntax/#tokenization
;; https://drafts.csswg.org/css-syntax/#component-value
;; https://drafts.csswg.org/css-syntax/#current-input-token
(define-type CSS-URL-Modifier (U CSS:Ident CSS-Lazy-Token))
(define-type CSS-Zero (U CSS:Zero CSS:Flzero))
(define-type CSS-One (U CSS:One CSS:Flone))

(define-tokens css-token syn-token #:+ CSS-Token
  [[css-numeric         #:+ CSS-Numeric         #:-> css-token   ([representation : String] [signed? : Boolean])]
   [css:dimension       #:+ CSS:Dimension       #:-> css-numeric ([datum : Flonum] [unit : Symbol])]]

  [[css:one             #:+ CSS:One             #:-> css:integer]
   [css:zero            #:+ CSS:Zero            #:-> css:integer]
     
   [css:flone           #:+ CSS:Flone           #:-> css:flonum]
   [css:flzero          #:+ CSS:Flzero          #:-> css:flonum]

   [css:open            #:+ CSS:Open            #:-> css:delim]
   [css:colon           #:+ CSS:Colon           #:-> css:delim]
   [css:semicolon       #:+ CSS:Semicolon       #:-> css:delim]
   [css:comma           #:+ CSS:Comma           #:-> css:delim]
   [css:slash           #:+ CSS:Slash           #:-> css:delim]
   [css:vbar            #:+ CSS:VBar            #:-> css:delim]
   [css:cdo             #:+ CSS:CDO             #:-> css:cd]
   [css:cdc             #:+ CSS:CDC             #:-> css:cd]

   [css:bad:eof         #:+ CSS:Bad:EOF         #:-> css:bad]
   [css:bad:eol         #:+ CSS:Bad:EOL         #:-> css:bad]
   [css:bad:char        #:+ CSS:Bad:Char        #:-> css:bad]
   [css:bad:blank       #:+ CSS:Bad:Blank       #:-> css:bad]
   [css:bad:range       #:+ CSS:Bad:Range       #:-> css:bad]
   [css:bad:stdin       #:+ CSS:Bad:Stdin       #:-> css:bad]]

  ; WARNING: Carefully defining types to avoid happening to mess up '(list? datum)'. 
  (define-symbolic-tokens css-bad-token #:+ CSS-Bad-Token
    [css:bad            #:+ CSS:Bad             #:as (Pairof Symbol String)]
    [css:close          #:+ CSS:Close           #:as Char])
    
  ; TODO: Typed Racket is buggy if there are more than 11 conditions
  (define-symbolic-tokens css-symbolic-token #:+ CSS-Symbolic-Token
    [css:delim          #:+ CSS:Delim           #:as Char]
    [css:ident          #:+ CSS:Ident           #:as Symbol            #:ci]
    [css:@keyword       #:+ CSS:@Keyword        #:as Keyword           #:ci]
    [css:hash           #:+ CSS:Hash            #:as Keyword]
    [css:string         #:+ CSS:String          #:as String]
    [css:match          #:+ CSS:Match           #:as Char]
    [css:cd             #:+ CSS:CD              #:as Symbol]
    [css:urange         #:+ CSS:URange          #:as (Pairof Index Index)]
    [css:whitespace     #:+ CSS:WhiteSpace      #:as (U String Char)])

  (define-lazy-tokens css-lazy-token #:+ CSS-Lazy-Token
    [css:url            #:+ CSS:URL             #:with modifiers       #:as String]   ; "" means 'about:invalid
    [css:block          #:+ CSS:Block           #:with components      #:as Char]
    [css:function       #:+ CSS:Function        #:with arguments       #:as Symbol #:ci]
    [css:λracket        #:+ CSS:λRacket         #:with arguments       #:as Symbol]
    [css:var            #:+ CSS:Var             #:with fallback        #:as Symbol])

  (define-numeric-tokens css-number #:+ CSS-Number #:nan +nan.0
    [css:integer        #:+ CSS:Integer         #:as Integer]
    [css:flonum         #:+ CSS:Flonum          #:as Flonum])

  (define-numeric-tokens css-fraction #:+ CSS-Fraction #:nan +nan.0
    [css:percentage     #:+ CSS:Percentage      #:as Flonum])

  (define-symbolic-tokens css-unreadable-token #:+ CSS-Unreadable-Token
    ; These tokens are remade by the parser instead of being produced by the tokenizer.
    [css:ratio          #:+ CSS:Ratio           #:as Positive-Exact-Rational]
    [css:racket         #:+ CSS:Racket          #:as Symbol]
    [css:#:keyword      #:+ CSS:#:Keyword       #:as Keyword]))

;; https://drafts.csswg.org/css-syntax/#style-rules
;; https://drafts.csswg.org/selectors/#invalid
(define-syntax-error exn:css #:as CSS-Syntax-Error #:for CSS-Token
  #:with [css-make-syntax-error css-log-syntax-error]
  [exn:css:resource           #:-> exn:css]
  [exn:css:deprecated         #:-> exn:css]
  [exn:css:loop               #:-> exn:css]
  [exn:css:namespace          #:-> exn:css]
  [exn:css:racket             #:-> exn:css]
  [exn:css:contract           #:-> exn:css:racket]
  [exn:css:unrecognized       #:-> exn:css]
  [exn:css:misplaced          #:-> exn:css:unrecognized]
  [exn:css:type               #:-> exn:css:unrecognized]
  [exn:css:type:An+B          #:-> exn:css:type]
  [exn:css:type:identifier    #:-> exn:css:type]
  [exn:css:type:variable      #:-> exn:css:type:identifier]
  [exn:css:range              #:-> exn:css:unrecognized]
  [exn:css:unit               #:-> exn:css:range]
  [exn:css:digit              #:-> exn:css:range]
  [exn:css:overconsumption    #:-> exn:css:unrecognized]
  [exn:css:enclosed           #:-> exn:css:overconsumption]
  [exn:css:malformed          #:-> exn:css]
  [exn:css:arity              #:-> exn:css:malformed]
  [exn:css:empty              #:-> exn:css:malformed]
  [exn:css:missing-block      #:-> exn:css:malformed]
  [exn:css:missing-value      #:-> exn:css:malformed]
  [exn:css:missing-feature    #:-> exn:css:malformed]
  [exn:css:missing-keyword    #:-> exn:css:malformed]
  [exn:css:missing-delimiter  #:-> exn:css:malformed]
  [exn:css:missing-colon      #:-> exn:css:missing-delimiter]
  [exn:css:missing-comma      #:-> exn:css:missing-delimiter]
  [exn:css:missing-slash      #:-> exn:css:missing-delimiter])

(define css-zero? : (-> Any Boolean : CSS-Zero) (λ [v] (or (css:zero? v) (css:flzero? v))))
(define css-one? : (-> Any Boolean : CSS-One) (λ [v] (or (css:one? v) (css:flone? v))))

(define css-signed-integer? : (-> CSS:Integer Boolean)
  (lambda [<B>]
    (or (< (css:integer-datum <B>) 0)
        (let ([raw (css-numeric-representation <B>)])
          (and (> (string-length raw) 0)
               (let ([sign (string-ref raw 0)])
                 (or (eq? sign #\+)
                     (eq? sign #\- #| -0 |#))))))))

(define css-nan? : (-> CSS-Numeric Boolean)
  (lambda [token]
    (or (and (css:flonum? token) (eqv? (css:flonum-datum token) +nan.0))
        (and (css:dimension? token) (eqv? (css:dimension-datum token) +nan.0))
        (and (css:percentage? token) (eqv? (css:percentage-datum token) +nan.0)))))

(define css-token->syntax : (-> CSS-Token Syntax)
  (lambda [instance]
    (datum->syntax #false (css-token->datum instance)
                   (syn-token->syntax-location instance))))

(define css-token-datum->string : (-> CSS-Token String)
  (lambda [instance]
    (cond [(css:ident? instance) (symbol->immutable-string (css:ident-datum instance))]
          [(css-numeric? instance) (css-numeric-representation instance)]
          [(css:@keyword? instance) (keyword->immutable-string (css:@keyword-datum instance))]
          [(css:hash? instance) (~a "#" (keyword->immutable-string (css:hash-datum instance)))]
          [(css:match? instance) (~a (css:match-datum instance) '=)]
          [(css:delim=:=? instance #\tab) "||"]
          [(css:string? instance) (~s (css:string-datum instance))]
          [else (~a (css-token->datum instance))])))
  
(define css-token->string : (->* (CSS-Token) ((Option Any) (Option Any)) String)
  (lambda [instance [alt-object #false] [alt-datum #false]]
    (string-append (syn-token-location-string instance) ": "
                   (format "~a: ~a"
                     (or (object-name alt-object) (object-name instance))
                     (or alt-datum (css-token-datum->string instance))))))

(define #:forall (Error) css-make-syntax-error : (-> (-> String Continuation-Mark-Set (Listof Syntax) Error)
                                                     (U CSS-Syntax-Any (Listof CSS-Token))
                                                     Error)
  (lambda [exn:css any]
    (match any
      [(or #false (list)) (exn:css (~a eof) (current-continuation-marks) null)]
      [(list token) (syn-token->exn exn:css css-token->string css-token->syntax token)]
      [(list main others ...) (syn-token->exn exn:css css-token->string css-token->syntax css-token-datum->string main (filter-not css:whitespace? others))]
      [(? css:function? main) (syn-token->exn exn:css css-token->string css-token->syntax css-token-datum->string main (css:function-arguments main))]
      [(? css:λracket? main) (syn-token->exn exn:css css-token->string css-token->syntax css-token-datum->string main (css:λracket-arguments main))]
      [(? css:block? main) (syn-token->exn exn:css css-token->string css-token->syntax css-token-datum->string main (css:block-components main))]
      [(? css-token?) (syn-token->exn exn:css css-token->string css-token->syntax any)])))

(define css-log-syntax-error : (->* (CSS-Syntax-Error) ((Option CSS-Token) (Option Log-Level)) Void)
  (lambda [errobj [property #false] [level #false]]
    (syn-log-syntax-error 'exn:css:syntax css-token->string css-token->datum
                          errobj property (or level 'warning))))

;;; https://drafts.csswg.org/css-syntax/#parsing
;; Parser Combinators and Syntax Sugars of dealing with declarations for client applications
(define-type CSS-Syntax-Any (Option CSS-Token))
(define-type (CSS-Multiplier idx) (U idx (List idx) (Pairof (U idx Symbol) (U idx Symbol))))
(define-type (CSS-Maybe css) (U css CSS-Wide-Keyword))
(define-type (CSS-Option css) (U css CSS-Syntax-Error False))
(define-type (CSS:Filter css) (-> CSS-Syntax-Any (CSS-Option css)))
(define-type (CSS-Parser css) (-> css (Listof CSS-Token) (Values (CSS-Option css) (Listof CSS-Token))))
(define-type CSS-Shorthand+Parser (Pairof (CSS-Parser CSS-Shorthand-Datum) (Listof+ Symbol)))
(define-type CSS-Shorthand-Parser (CSS-Parser CSS-Shorthand-Datum))

(struct css-declaration
  ([name : CSS:Ident]
   [values : (Listof+ CSS-Token)]
   [important? : Boolean]
   [lazy? : Boolean])
  #:type-name CSS-Declaration
  #:transparent)

;; https://drafts.csswg.org/css-cascade/#shorthand
;; https://drafts.csswg.org/css-cascade/#filtering
;; https://drafts.csswg.org/css-cascade/#cascading
(define-type CSS-Values (HashTable Symbol (-> Any)))
(define-type CSS-Declaration-Parser (U Void False CSS-Shorthand+Parser Procedure #| generalize CSS:Filter and CSS-Parser |#))
(define-type CSS-Declaration-Parsers (-> Symbol (-> Void) CSS-Declaration-Parser))
(define-type (CSS-Cascaded-Value-Filter Preference) (-> CSS-Values (Option CSS-Values) Preference))
(define-type (CSS-Cascaded-Value+Filter Preference Env) (-> CSS-Values (Option CSS-Values) Env Preference))
(define-type (CSS->Racket racket) (-> Symbol Any racket))

(define-syntax (define-css-value stx)
  (syntax-case stx [:]
    [(_ datum #:as Datum (fields ...) options ...)
     (syntax/loc stx (struct datum (fields ...) #:type-name Datum #:transparent options ...))]
    [(_ datum #:as Datum #:=> parent (fields ...) options ...)
     (syntax/loc stx (struct datum parent (fields ...) #:type-name Datum #:transparent options ...))]))

(define-syntax (define-prefab-keyword stx)
  (syntax-case stx [:]
    [(_ css-wide-keyword #:as CSS-Wide-Keyword [keyword ...])
     (with-syntax ([(<keywords> keywords-filter-map css:symbol ...)
                    (list* (format-id #'css-wide-keyword "<~as>" (syntax-e #'css-wide-keyword))
                           (format-id #'css-wide-keyword "~as-filter-map" (syntax-e #'css-wide-keyword))
                           (for/list ([kwd (in-list (syntax->list #'(keyword ...)))])
                             (format-id kwd "css:~a" (syntax-e kwd))))])
       (syntax/loc stx
         (begin (define-css-value css-wide-keyword #:as CSS-Wide-Keyword ([datum : Symbol]))
                (define css:symbol : CSS-Wide-Keyword (css-wide-keyword 'keyword)) ...
                
                (define <keywords> : (-> (CSS:Filter CSS-Wide-Keyword))
                  (lambda []
                    (λ [[token : CSS-Syntax-Any]]
                      (and (css:ident? token)
                           (case (css:ident-norm token)
                             [(keyword) css:symbol] ...
                             [else (make-exn:css:range token)])))))

                (define keywords-filter-map : (-> (U Symbol CSS-Syntax-Error) (U Symbol CSS-Wide-Keyword CSS-Syntax-Error))
                  (lambda [key]
                    (cond [(not (symbol? key)) key]
                          [(eq? key 'keyword) css:symbol] ...
                          [else key]))))))]))

; https://drafts.csswg.org/css-cascade/#all-shorthand
; https://drafts.csswg.org/css-values/#relative-lengths
; https://drafts.csswg.org/css-values/#common-keywords
(define default-css-all-exceptions : (Parameterof (Listof Symbol)) (make-parameter (list 'direction 'unicode-bidi)))
(define-prefab-keyword css-wide-keyword #:as CSS-Wide-Keyword [initial inherit unset revert])

(define-css-parameter css-root-element-type : Symbol #:= 'root)
(define-css-parameter css-root-element-id : (U Keyword (Listof+ Keyword)) #:= '#:root)
(define-css-parameters css-root-relative-lengths [vw vh rem rlh] : Nonnegative-Flonum #:= +nan.0)
(define-css-parameters css-font-relative-lengths [em ex cap ch ic lh] : Nonnegative-Flonum #:= +nan.0)

(define make-css-values : (-> CSS-Values) (λ [] ((inst make-hasheq Symbol (-> Any)))))
  
(define css-ref : (All (a b c) (case-> [CSS-Values (U CSS-Values Boolean) Symbol -> Any]
                                       [CSS-Values (U CSS-Values Boolean) Symbol (CSS->Racket a) -> a]
                                       [CSS-Values (U CSS-Values Boolean) Symbol (-> Any Boolean : #:+ a) b -> (U a b)]
                                       [CSS-Values (U CSS-Values Boolean) Symbol (-> Any Boolean : #:+ a) b c -> (U a b c)]))
  (case-lambda
    [(declared-values inherited-values desc-name)
     (define-values (cascaded-value specified-value) (css-ref-raw declared-values inherited-values desc-name))
     specified-value]
    [(declared-values inherited-values desc-name datum->value)
     (define-values (cascaded-value specified-value) (css-ref-raw declared-values inherited-values desc-name))
     (css-tee-computed-value declared-values desc-name cascaded-value (datum->value desc-name specified-value))]
    [(declared-values inherited-values desc-name datum? default-value)
     (define-values (cascaded-value specified-value) (css-ref-raw declared-values inherited-values desc-name))
     (css-tee-computed-value declared-values desc-name cascaded-value (if (datum? specified-value) specified-value default-value))]
    [(declared-values inherited-values desc-name datum? default-value inherit-value)
     ;;; See NOTE in `css-ref-raw`
     (define-values (cascaded-value specified-value) (css-ref-raw declared-values inherited-values desc-name))
     (css-tee-computed-value declared-values desc-name cascaded-value
                             (cond [(datum? specified-value) specified-value]
                                   [(eq? specified-value css:inherit) inherit-value]
                                   [else default-value]))]))

(define css-ref-raw : (-> CSS-Values (U CSS-Values Boolean) Symbol (Values Any Any))
  (lambda [declared-values inherited-values desc-name]
    (define declared-value : (-> Any)
      (hash-ref declared-values desc-name
                (λ [] (cond [(memq desc-name (default-css-all-exceptions)) (λ [] css:unset)]
                            [else (hash-ref declared-values 'all (λ [] (λ [] css:unset)))]))))
    (define cascaded-value : Any (declared-value))
    (define specified-value : Any
      ;;; NOTE
      ;; when the `inherited-values` is #true, it means this property is inheritable
      ;; but the value is not stored with the property, clients will deal this on their own.
      ;; For example, the font consists of several separate properties, and some of their value types between Racket
      ;; and CSS are not stay the same, it is therefore more convenient to store them with the shorthand property 'font'
      ;; as a Racket object than resetting all relevent properties every time after making a new font object.
      (cond [(not (css-wide-keyword? cascaded-value)) cascaded-value]
            [(eq? cascaded-value css:initial) css:initial]
            [(hash? inherited-values) (let-values ([(_ sv) (css-ref-raw inherited-values #true desc-name)]) sv)]
            [(not inherited-values) css:initial]
            [else css:inherit]))
    (unless (eq? cascaded-value specified-value)
      (hash-set! declared-values desc-name (λ [] specified-value)))
    (values cascaded-value specified-value)))

(define css-tee-computed-value : (All (a) (-> CSS-Values Symbol Any a a))
  (lambda [properties desc-name cascaded-value computed-value]
    (unless (eq? cascaded-value computed-value)
      (hash-set! properties desc-name (λ [] computed-value)))
    computed-value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define css-car/cdr : (All (a b) (case-> [(Pairof a b) -> (Values a b)]
                                         [(Listof a) -> (Values (Option a) (Listof a))]))
  (lambda [pretend-no-whitespace]
    (cond [(null? pretend-no-whitespace) (values #false null)]
          [else (values (car pretend-no-whitespace) (cdr pretend-no-whitespace))])))

(define css-car/cadr : (All (a) (case-> [(Pairof a (Listof a)) -> (Values a (Listof a) (Option a) (Listof a))]
                                        [(Listof a) -> (Values (Option a) (Listof a) (Option a) (Listof a))]))
  (lambda [pretend-no-whitespace]
    (cond [(null? pretend-no-whitespace) (values #false null #false null)]
          [else (let ([1st (car pretend-no-whitespace)]
                      [2nd (cdr pretend-no-whitespace)])
                  (cond [(null? 2nd) (values 1st null #false null)]
                        [else (values 1st 2nd (car 2nd) (cdr 2nd))]))])))

(define css-car : (All (a) (-> (Listof a) (Values (Option a) (Listof a))))
  (lambda [dirty]
    (let skip-whitespace ([rest dirty])
      (cond [(null? rest) (values #false null)]
            [else (let-values ([(head tail) (values (car rest) (cdr rest))])
                    (cond [(css:whitespace? head) (skip-whitespace tail)]
                          [else (values head tail)]))]))))

(define css-pair? : (All (a) (-> (Listof a) Boolean : #:+ (Listof+ a)))
  (lambda [dirty]
    (and (pair? dirty)
         (let skip-whitespace : Boolean ([head : a (car dirty)]
                                         [tail : (Listof a) (cdr dirty)])
           (implies (css:whitespace? head)
                    (and (pair? tail)
                         (skip-whitespace (car tail)
                                          (cdr tail))))))))

(define css-null? : (All (a) (-> (Listof a) Boolean : #:- (Listof+ a)))
  (lambda [dirty]
    (not (css-pair? dirty))))

(define css-cons : (All (css) (-> (U CSS-Syntax-Error css) (Listof css) (Listof css)))
  (lambda [item items]
    (cond [(exn? item) items]
          [else (cons item items)])))
