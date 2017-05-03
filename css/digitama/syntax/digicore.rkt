#lang typed/racket

(provide (all-from-out racket/flonum racket/fixnum))
(provide (except-out (all-defined-out) css-make-syntax-error css-tee-computed-value css-ref-raw
                     define-tokens define-token define-token-interface
                     define-symbolic-tokens define-numeric-tokens
                     define-prefab-keyword define-syntax-error))

(require racket/fixnum)
(require racket/flonum)

(require "misc.rkt")

(require (for-syntax racket/string))
(require (for-syntax racket/syntax))
(require (for-syntax syntax/parse))

(define-syntax (define-preference stx)
  (syntax-parse stx #:literals [:]
    [(self preference #:as Preference (fields ...) options ...)
     #'(self preference #:as Preference #:with [] (fields ...) options ...)]
    [(self preference #:as Preference #:with [[bindings BindTypes ...] ...] ([property : DataType info ...] ...) options ...)
     (with-syntax* ([make-preference (format-id #'preference "make-~a" (syntax-e #'preference))]
                    [(Uv uv? uv) (list #'CSS-Wide-Keyword #'css-wide-keyword? #'css:initial)]
                    [([initial-value property-filter ArgumentType defval ...] ...)
                     (for/list ([field-info (in-list (syntax->list #'([property DataType info ...] ...)))])
                       (syntax-parse field-info
                         [(p T #:= dv #:~> Super fltr) #'[(fltr dv) (if (uv? p) (fltr dv) (fltr p)) (U Super Uv) uv]]
                         [(p T #:= dv #:~> fltr) #'[(fltr dv) (if (uv? p) (fltr initial) (fltr p)) Any uv]]
                         [(p T #:= dv) #'[dv (if (uv? p) dv p) (U T Uv) uv]]
                         [(p rest ...) (raise-syntax-error (syntax-e #'self) "property requires an initial value" #'p)]))]
                    [(initial-property ...)
                     (for/list ([property (in-list (syntax->list #'(property ...)))])
                       (format-id property "initial-~a-~a" (syntax-e #'preference) (syntax-e property)))]
                    [(args ...)
                     (for/fold ([args null])
                               ([argument (in-list (syntax->list #'([property : ArgumentType defval ...] ...)))])
                       (cons (datum->syntax argument (string->keyword (symbol->string (car (syntax->datum argument)))))
                             (cons argument args)))]
                    [([pref-bindings properties ...] ...)
                     (for/list ([binding (in-list (syntax->list #'(bindings ...)))]
                                [Types (in-list (syntax->list #'([BindTypes ...] ...)))])
                       (define types (syntax->datum Types))
                       (cons (format-id #'preference "~a-~a" (syntax-e #'preference) (syntax-e binding))
                             (for/fold ([properties null])
                                       ([property (in-list (syntax->list #'(property ...)))]
                                        [type (in-list (syntax->list #'(DataType ...)))])
                               (cond [(not (memq (syntax-e type) types)) properties]
                                     [else (cons (syntax-e property) properties)]))))])
       #'(begin (struct preference ([property : DataType] ...) options ...)
                (define pref-bindings : (Listof Symbol) (list 'properties ...)) ...
                (define (make-preference args ...) : Preference (preference property-filter ...))
                (define (initial-property) : DataType initial-value) ...
                (define-type Preference preference)))]))
  
(define-syntax (define-token-interface stx)
  (syntax-case stx [:]
    [(_ symbolic-prefix : Type id? id-datum #:+ CSS:ID #:eq? type=?)
     (with-syntax ([<id> (format-id #'symbolic-prefix "<~a>" (syntax-e #'symbolic-prefix))]
                   [id=<-? (format-id #'symbolic-prefix "~a=<-?" (syntax-e #'symbolic-prefix))]
                   [id=:=? (format-id #'symbolic-prefix "~a=:=?" (syntax-e #'symbolic-prefix))])
       #'(begin (define id=<-? : (All (a) (case-> [Any (-> Type Boolean : #:+ a) -> (Option a) : #:+ CSS:ID]
                                                  [Any (U (-> Type Boolean) (Listof Type)) -> (Option Type) : #:+ CSS:ID]))
                  (lambda [token range?]
                    (and (id? token)
                         (let ([datum : Type (id-datum token)])
                           (cond [(procedure? range?) (and (range? datum) datum)]
                                 [else (and (member datum range? type=?) datum)])))))

                (define <id> : (All (a) (case-> [(-> Type Boolean : #:+ a) -> (CSS:Filter a)]
                                                [(U (-> Type Boolean) (Listof Type) Type) -> (CSS:Filter Type)]
                                                [-> (CSS:Filter Type)]))
                  (case-lambda
                    [() (λ [[t : CSS-Syntax-Any]] (and (id? t) (id-datum t)))]
                    [(range?) (cond [(procedure? range?)
                                     (λ [[t : CSS-Syntax-Any]]
                                       (and (id? t)
                                            (or (let ([d : Type (id-datum t)]) (and (range? d) d))
                                                (make-exn:css:range t))))]
                                    [(list? range?)
                                     (λ [[t : CSS-Syntax-Any]]
                                       (and (id? t)
                                            (let ([d : Type (id-datum t)])
                                              (cond [(member d range? type=?) d]
                                                    [else (make-exn:css:range t)]))))]
                                    [else (λ [[t : CSS-Syntax-Any]]
                                            (and (id? t)
                                                 (let ([d : Type (id-datum t)])
                                                   (if (type=? d range?) d (make-exn:css:range t)))))])]))

                (define id=:=? : (-> Any Type (Option Type) : #:+ CSS:ID) #| for performance |#
                  (lambda [t v]
                    (and (id? t)
                         (let ([d : Type (id-datum t)])
                           (and (type=? d v) d)))))))]
    [(_ numeric-prefix : Type id? id-datum #:+ CSS:ID #:= type=?)
     (with-syntax ([<id> (format-id #'numeric-prefix "<~a>" (syntax-e #'numeric-prefix))]
                   [id=<-? (format-id #'numeric-prefix "~a=<-?" (syntax-e #'numeric-prefix))])
       #'(begin (define id=<-? : (All (a) (case-> [Any (-> Type Boolean : #:+ a) -> (Option a) : #:+ CSS:ID]
                                                  [Any (-> Type Type Boolean) Type -> (Option Type) : #:+ CSS:ID]
                                                  [Any Type (-> Type Type Boolean) Type -> (Option Type) : #:+ CSS:ID]
                                                  [Any (Listof Type) -> (Option Type) : #:+ CSS:ID]))
                  (case-lambda
                    [(token op n)   (and (id? token) (let ([d : Type (id-datum token)]) (and (op d n) d)))]
                    [(token l op r) (and (id? token) (let ([m : Type (id-datum token)]) (and (op l m) (op m r) m)))]
                    [(token range?) (and (id? token) (let ([d : Type (id-datum token)])
                                                       (cond [(procedure? range?) (and (range? d) d)]
                                                             [else (for/or : (Option Type) ([v (in-list range?)])
                                                                     (and (type=? d v) d))])))]))

                (define <id> : (All (a) (case-> [(-> Type Boolean : #:+ a) -> (CSS:Filter a)]
                                                [(-> Type Type Boolean) Type -> (CSS:Filter Type)]
                                                [Type (-> Type Type Boolean) Type -> (CSS:Filter Type)]
                                                [(Listof Type) -> (CSS:Filter Type)]
                                                [-> (CSS:Filter Type)]))
                  (case-lambda
                    [() (λ [[t : CSS-Syntax-Any]] (and (id? t) (id-datum t)))]
                    [(op n) (λ [[t : CSS-Syntax-Any]]
                              (and (id? t)
                                   (let ([d : Type (id-datum t)])
                                     (if (op d n) d (make-exn:css:range t)))))]
                    [(l op r) (λ [[t : CSS-Syntax-Any]]
                                (and (id? t)
                                     (let ([m : Type (id-datum t)])
                                       (if (and (op l m) (op m r)) m (make-exn:css:range t)))))]
                    [(range?) (λ [[t : CSS-Syntax-Any]]
                                (and (id? t)
                                     (let ([d : Type (id-datum t)])
                                       (or (cond [(procedure? range?) (and (range? d) d)]
                                                 [(list? range?) (and (member d range? type=?) d)]
                                                 [else (and (type=? d range?) d)])
                                           (make-exn:css:range t)))))]))))]))

(define-syntax (define-token stx)
  (syntax-parse stx #:literals [: Symbol Keyword]
    [(_ id : Number parent #:as Type #:=? type=? #:with id? id-datum)
     (with-syntax ([id=? (format-id #'id "~a=?" (syntax-e #'id))])
       #'(begin (struct: id : Number parent ([datum : Type]))
                (define (id=? [t1 : Number] [t2 : Number]) : Boolean (type=? (id-datum t1) (id-datum t2)))
                (define-token-interface id : Type id? id-datum #:+ Number #:= type=?)))]
    [(_ id : Identifier parent ((~and (~or Symbol Keyword) Type) #:ci rest ...) #:with id? id-datum)
     (with-syntax ([id=? (format-id #'id "~a=?" (syntax-e #'id))]
                   [id-norm=? (format-id #'id "~a-norm=?" (syntax-e #'id))]
                   [id-norm (format-id #'id "~a-norm" (syntax-e #'id))])
       #'(begin (struct: id : Identifier parent ([datum : Type] [norm : Type] rest ...))
                (define (id=? [t1 : Identifier] [t2 : Identifier]) : Boolean (eq? (id-datum t1) (id-datum t2)))
                (define (id-norm=? [t1 : Identifier] [t2 : Identifier]) : Boolean (eq? (id-norm t1) (id-norm t2)))
                (define-token-interface id : Type id? id-datum #:+ Identifier #:eq? eq?)
                (define-token-interface id-norm : Type id? id-norm  #:+ Identifier #:eq? eq?)))]
    [(_ id : Otherwise parent (Type rest ...) #:with id? id-datum)
     (with-syntax ([type=? (case (syntax-e #'Type) [(String) #'string=?] [(Char) #'char=?] [else #'equal?])]
                   [id=? (format-id #'id "~a=?" (syntax-e #'id))])
       #'(begin (struct: id : Otherwise parent ([datum : Type] rest ...))
                (define (id=? [t1 : Otherwise] [t2 : Otherwise]) : Boolean (type=? (id-datum t1) (id-datum t2)))
                (define-token-interface id : Type id? id-datum #:+ Otherwise #:eq? type=?)))]))

(define-syntax (define-symbolic-tokens stx)
  (syntax-parse stx
    [(_ token #:+ Token [id #:+ ID #:as Type rest ...] ...)
     (with-syntax ([token->datum (format-id #'token "~a->datum" (syntax-e #'token))]
                   [Token-Datum (format-id #'token "~a-Datum" (syntax-e #'Token))]
                   [([id? id-datum] ...)
                    (for/list ([<id> (in-list (syntax->list #'(id ...)))])
                      (list (format-id <id> "~a?" (syntax-e <id>))
                            (format-id <id> "~a-datum" (syntax-e <id>))))])
       #'(begin (struct: token : Token css-token ())
                (define-token id : ID token (Type rest ...) #:with id? id-datum) ...
                (define-type Token-Datum (U Type ...))
                (define (token->datum [t : Token]) : (Option Token-Datum) (cond [(id? t) (id-datum t)] ... [else #false]))))]))

(define-syntax (define-lazy-tokens stx)
  (syntax-parse stx
    [(_ token #:+ Token [id #:+ ID #:with components #:as Type ...] ...)
     (with-syntax ([([id? id-copy Component] ...)
                    (for/list ([<id> (in-list (syntax->list #'(id ...)))])
                      (list (format-id <id> "~a?" (syntax-e <id>))
                            (format-id <id> "~a-copy" (syntax-e <id>))
                            (if (eq? (syntax-e <id>) 'css:url) #'CSS-URL-Modifier #'CSS-Token)))])
       #'(begin (define-symbolic-tokens token #:+ Token [id #:+ ID #:as Type ... [components : (Listof Component)] [lazy? : Boolean]] ...)

                (define id-copy : (-> ID (Listof Component) Boolean ID)
                  (lambda [instance subcoms ?]
                    (struct-copy id instance [components (if (css-pair? subcoms) subcoms null)] [lazy? ?])))
                ...))]))

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
                            (let ([type-name (symbol->string (syntax-e <type>))])
                              (cond [(string-contains? type-name "Single-Flonum") #'=]
                                    [(string-contains? type-name "Flonum") #'fl=]
                                    [(string-contains? type-name "Fixnum") #'fx=]
                                    [else #'=]))))])
       #'(begin (struct: token : Token css-numeric ())
                (define-token id : ID token #:as Type #:=? type=? #:with id? id-datum) ...
                (define (token->datum [t : Token]) : (U Type ...) (cond [(id? t) (id-datum t)] ... [else nan]))))]))
  
(define-syntax (define-tokens stx)
  (syntax-case stx []
    [(_ token #:+ Token header
        [[ptoken #:+ PToken #:-> pparent pfields] ...]
        [[ctoken #:+ CToken #:-> cparent] ...]
        (define-typical-tokens group #:+ Group rest ...) ...)
     (with-syntax ([token->datum (format-id #'token "~a->datum" (syntax-e #'token))]
                   [Token-Datum (format-id #'token "~a-Datum" (syntax-e #'Token))]
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
       #'(begin (struct: token : Token header)
                (struct: ptoken : PToken pparent pfields) ...
                (define-typical-tokens group #:+ Group rest ...) ...
                (struct: ctoken : CToken cparent ()) ...

                (define-type Token-Datum (U False Number (Pairof Number Symbol) Symbolic-Datum ...))
                (define token->datum : (-> Token Token-Datum)
                  (lambda [instance]
                    (cond [(css:dimension? instance) (cons (css:dimension-datum instance) (css:dimension-unit instance))]
                          [(type? instance) (type->datum instance)] ...
                          [else (assert (object-name instance) symbol?)])))))]))

(define-syntax (define-syntax-error stx)
  (syntax-case stx []
    [(_ exn:css #:as Syntax-Error [subexn #:-> parent] ...)
     (with-syntax ([([make-exn make+exn throw-exn] ...)
                    (for/list ([<exn> (in-list (syntax->list #'(subexn ...)))])
                      (list (format-id <exn> "make-~a" (syntax-e <exn>))
                            (format-id <exn> "make+~a" (syntax-e <exn>))
                            (format-id <exn> "throw-~a" (syntax-e <exn>))))])
       #'(begin (define-type Syntax-Error exn:css)
                (struct exn:css exn:fail:syntax ())
                (struct subexn parent ()) ...

                (define make-exn : (-> (U CSS-Syntax-Any (Listof CSS-Token)) CSS-Syntax-Error)
                  (lambda [v]
                    (css-make-syntax-error subexn v)))
                ...

                (define make+exn : (->* ((U CSS-Syntax-Any (Listof CSS-Token))) ((Option CSS:Ident) Log-Level) CSS-Syntax-Error)
                  (lambda [v [property #false] [level 'warning]]
                    (define errobj : CSS-Syntax-Error (css-make-syntax-error subexn v))
                    (css-log-syntax-error errobj property level)
                    errobj))
                ...

                (define throw-exn : (->* ((U CSS-Syntax-Any (Listof CSS-Token))) ((Option CSS:Ident) Log-Level) Nothing)
                  (lambda [v [property #false] [level 'warning]]
                    (raise (make+exn v property level))))
                ...))]))

;;; https://drafts.csswg.org/css-syntax/#tokenization
;; https://drafts.csswg.org/css-syntax/#component-value
;; https://drafts.csswg.org/css-syntax/#current-input-token
(define-type CSS-URL-Modifier (U CSS:Ident CSS-Lazy-Token))
(define-type CSS-Zero (U CSS:Zero CSS:Flzero))
(define-type CSS-One (U CSS:One CSS:Flone))

(define-tokens css-token #:+ CSS-Token
  ([source : (U String Symbol)]
   [line : Positive-Integer]
   [column : Natural]
   [start : Positive-Integer] ; `start` and `end` (instead of `position` and `span`) are required by color lexer.
   [end : Positive-Integer])
  [[css-numeric         #:+ CSS-Numeric         #:-> css-token   ([representation : String])]
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
   [css:bad:stdin       #:+ CSS:Bad:StdIn       #:-> css:bad]]

  ; WARNING: Carefully defining types to avoid happening to mess up '(list? datum)'. 
  (define-symbolic-tokens css-bad-token #:+ CSS-Bad-Token
    [css:bad            #:+ CSS:Bad             #:as String]
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

  (define-numeric-tokens css-fraction #:+ CSS-Fraction #:nan +nan.f
    [css:percentage     #:+ CSS:Percentage      #:as Single-Flonum])

  (define-symbolic-tokens css-unreadable-token #:+ CSS-Unreadable-Token
    ; These tokens are remade by the parser instead of being produced by the tokenizer.
    [css:ratio          #:+ CSS:Ratio           #:as Positive-Exact-Rational]
    [css:racket         #:+ CSS:Racket          #:as Symbol]
    [css:#:keyword      #:+ CSS:#:Keyword       #:as Keyword]))

;; https://drafts.csswg.org/css-syntax/#style-rules
;; https://drafts.csswg.org/selectors/#invalid
(define-syntax-error exn:css #:as CSS-Syntax-Error
  [exn:css:resource           #:-> exn:css]
  [exn:css:deprecated         #:-> exn:css]
  [exn:css:cyclic             #:-> exn:css]
  [exn:css:namespace          #:-> exn:css]
  [exn:css:racket             #:-> exn:css]
  [exn:css:contract           #:-> exn:css:racket]
  [exn:css:unrecognized       #:-> exn:css]
  [exn:css:misplaced          #:-> exn:css:unrecognized]
  [exn:css:type               #:-> exn:css:unrecognized]
  [exn:css:type:identifier    #:-> exn:css:type]
  [exn:css:type:variable      #:-> exn:css:type:identifier]
  [exn:css:range              #:-> exn:css:unrecognized]
  [exn:css:unit               #:-> exn:css:range]
  [exn:css:overconsumption    #:-> exn:css:unrecognized]
  [exn:css:enclosed           #:-> exn:css:overconsumption]
  [exn:css:malformed          #:-> exn:css]
  [exn:css:arity              #:-> exn:css:malformed]
  [exn:css:empty              #:-> exn:css:malformed]
  [exn:css:missing-block      #:-> exn:css:malformed]
  [exn:css:missing-value      #:-> exn:css:malformed]
  [exn:css:missing-feature    #:-> exn:css:malformed]
  [exn:css:missing-delimiter  #:-> exn:css:malformed]
  [exn:css:missing-colon      #:-> exn:css:missing-delimiter]
  [exn:css:missing-comma      #:-> exn:css:missing-delimiter]
  [exn:css:missing-slash      #:-> exn:css:missing-delimiter])

(define css-zero? : (-> Any Boolean : #:+ CSS-Zero) (λ [v] (or (css:zero? v) (css:flzero? v))))
(define css-one? : (-> Any Boolean : #:+ CSS-One) (λ [v] (or (css:one? v) (css:flone? v))))

(define css-nan? : (-> CSS-Numeric Boolean)
  (lambda [token]
    (or (and (css:flonum? token) (eqv? (css:flonum-datum token) +nan.0))
        (and (css:dimension? token) (eqv? (css:dimension-datum token) +nan.0))
        (and (css:percentage? token) (eqv? (css:percentage-datum token) +nan.f)))))

(define-syntax (css-remake-token stx)
  (syntax-case stx []
    [(_ [start-token end-token] make-css:token datum extra ...)
     #'(make-css:token (css-token-source start-token) (css-token-line start-token)
                       (css-token-column start-token) (css-token-start start-token)
                       (css-token-end end-token) datum extra ...)]
    [(_ here-token make-css:token datum ...)
     #'(css-remake-token [here-token here-token] make-css:token datum ...)]))

(define css-token->syntax : (-> CSS-Token Syntax)
  (lambda [instance]
    (datum->syntax #false (css-token->datum instance)
                   (vector (css-token-source instance) (css-token-line instance) (css-token-column instance)
                           (css-token-start instance) (fx- (css-token-end instance) (css-token-start instance))))))

(define css-token-datum->string : (-> CSS-Token String)
  (lambda [instance]
    (cond [(css:ident? instance) (symbol->string (css:ident-datum instance))]
          [(css-numeric? instance) (css-numeric-representation instance)]
          [(css:@keyword? instance) (keyword->string (css:@keyword-datum instance))]
          [(css:hash? instance) (~a "#" (keyword->string (css:hash-datum instance)))]
          [(css:match? instance) (~a (css:match-datum instance) '=)]
          [(css:delim=:=? instance #\tab) "||"]
          [(css:string? instance) (~s (css:string-datum instance))]
          [else (~a (css-token->datum instance))])))
  
(define css-token->string : (->* (CSS-Token) ((Option Any) (Option Any)) String)
  (lambda [instance [alt-object #false] [alt-datum #false]]
    (format "~a:~a:~a: ~a: ~a" (css-token-source instance) (css-token-line instance) (add1 (css-token-column instance))
            (or (object-name alt-object) (object-name instance))
            (or alt-datum (css-token-datum->string instance)))))

(define css-make-syntax-error : (-> (-> String Continuation-Mark-Set (Listof Syntax) CSS-Syntax-Error)
                                    (U CSS-Syntax-Any (Listof CSS-Token))
                                    CSS-Syntax-Error)
  (let ([empty-stacks (continuation-marks #false)])
    (lambda [exn:css any]
      (define (token->exn [main : CSS-Token]) : CSS-Syntax-Error
        (exn:css (css-token->string main exn:css) empty-stacks (list (css-token->syntax main))))
      (define (tokens->exn [head : CSS-Token] [others : (Listof CSS-Token)]) : CSS-Syntax-Error
        (exn:css (format "~a ~a" (css-token->string head exn:css) (map css-token-datum->string others))
                 empty-stacks (map css-token->syntax (cons head others))))
      (match any
        [(or (? eof-object?) (list)) (exn:css (~a eof) empty-stacks null)]
        [(list token) (token->exn token)]
        [(list main others ...) (tokens->exn main (filter-not css:whitespace? others))]
        [(? css:function? main) (tokens->exn main (css:function-arguments main))]
        [(? css:λracket? main) (tokens->exn main (css:λracket-arguments main))]
        [(? css:block? main) (tokens->exn main (css:block-components main))]
        [(? css-token?) (token->exn any)]))))
  
(define css-log-syntax-error : (->* (CSS-Syntax-Error) ((Option CSS:Ident) Log-Level) Void)
  (lambda [errobj [property #false] [level 'warning]]
    (define logger : Logger (current-logger))
    (define topic : Symbol 'exn:css:syntax)
    (define msg : String (exn-message errobj))
    (define <eof>? : Boolean (regexp-match? #px"#<eof>" msg))
    (cond [(false? property) (log-message logger level topic msg errobj)]
          [(not <eof>?) (log-message logger level topic (format "~a @‹~a›" msg (css:ident-datum property)) errobj)]
          [else (let ([eof-msg (css-token->string property errobj eof)])
                  (log-message logger level topic (format "~a @‹~a›" eof-msg (css:ident-datum property)) errobj))])))

;;; https://drafts.csswg.org/css-syntax/#parsing
;; Parser Combinators and Syntax Sugars of dealing with declarations for client applications
(define-type CSS-Syntax-Any (U CSS-Token EOF))
(define-type (CSS-Multiplier idx) (U idx (List idx) (Pairof (U idx Symbol) (U idx Symbol))))
(define-type (CSS-Maybe css) (U css CSS-Wide-Keyword))
(define-type (CSS-Option css) (U css CSS-Syntax-Error False))
(define-type (CSS:Filter css) (-> CSS-Syntax-Any (CSS-Option css)))
(define-type (CSS-Parser css) (-> css (Listof CSS-Token) (Values (CSS-Option css) (Listof CSS-Token))))
(define-type CSS-Shorthand+Parser (Pairof (CSS-Parser (HashTable Symbol Any)) (Listof+ Symbol)))
(define-type CSS-Shorthand-Parser (CSS-Parser (HashTable Symbol Any)))
(define-type CSS-Longhand-Update (-> Symbol Any Any Any))

(struct: css-declaration : CSS-Declaration
  ([name : CSS:Ident]
   [values : (Listof+ CSS-Token)]
   [important? : Boolean]
   [lazy? : Boolean]))

;; https://drafts.csswg.org/css-cascade/#shorthand
;; https://drafts.csswg.org/css-cascade/#filtering
;; https://drafts.csswg.org/css-cascade/#cascading
(define-type CSS-Values (HashTable Symbol (-> Any)))
(define-type CSS-Declaration-Parsers (-> Symbol (-> Void) CSS-Declaration-Parser))
(define-type (CSS-Cascaded-Value-Filter Preference) (-> CSS-Values (Option CSS-Values) Preference))
(define-type (CSS-Cascaded-Value+Filter Preference Env) (-> CSS-Values (Option CSS-Values) Env Preference))
(define-type (CSS->Racket racket) (-> Symbol Any racket))

(define-type CSS-Declaration-Parser
  (U CSS-Shorthand+Parser
     (CSS-Parser (Listof Any))
     (CSS:Filter Any)
     Void False))

(define-syntax (define-css-value stx)
  (syntax-case stx [:]
    [(_ datum #:as Datum (fields ...) options ...)
     #'(begin (define-type Datum datum)
              (struct datum (fields ...) #:transparent options ...))]
    [(_ datum #:as Datum #:=> parent (fields ...) options ...)
     #'(begin (define-type Datum datum)
              (struct datum parent (fields ...) #:transparent options ...))]))

(define-syntax (define-prefab-keyword stx)
  (syntax-case stx [:]
    [(_ css-wide-keyword #:as CSS-Wide-Keyword [keyword ...])
     (with-syntax ([(<keywords> keywords-filter-map css:symbol ...)
                    (list* (format-id #'css-wide-keyword "<~as>" (syntax-e #'css-wide-keyword))
                           (format-id #'css-wide-keyword "~as-filter-map" (syntax-e #'css-wide-keyword))
                           (for/list ([kwd (in-list (syntax->list #'(keyword ...)))])
                             (format-id kwd "css:~a" (syntax-e kwd))))])
       #'(begin (define-css-value css-wide-keyword #:as CSS-Wide-Keyword ([datum : Symbol]))
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
                          [else key])))))]))

; https://drafts.csswg.org/css-cascade/#all-shorthand
; https://drafts.csswg.org/css-values/#relative-lengths
; https://drafts.csswg.org/css-values/#common-keywords
(define default-css-all-exceptions : (Parameterof (Listof Symbol)) (make-parameter (list 'direction 'unicode-bidi)))
(define-prefab-keyword css-wide-keyword #:as CSS-Wide-Keyword [initial inherit unset revert])

(define-css-parameter css-root-element-type : Symbol #:= 'root)
(define-css-parameter css-root-element-id : (U Keyword (Listof+ Keyword)) #:= '#:root)
(define-css-parameters css-root-relative-lengths [vw vh rem rlh] : Nonnegative-Flonum #:= +nan.0)
(define-css-parameters css-font-relative-lengths [em ex cap ch ic lh] : Nonnegative-Flonum #:= +nan.0)
(define css-longhand : (HashTable Symbol Any) (make-immutable-hasheq))
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
                (thunk (cond [(memq desc-name (default-css-all-exceptions)) (thunk css:unset)]
                             [else (hash-ref declared-values 'all (thunk (thunk css:unset)))]))))
    (define cascaded-value : Any (declared-value))
    (define specified-value : Any
      ;;; NOTE
      ;; when the `inherited-values` is #true, it means this property is inheritable
      ;; but the value is not stored with the property, clients will deal this on their on.
      ;; For example, the font consists of several separate properties, and some of them value types between Racket
      ;; and CSS are not stay the same, it is therefore more convenient to store it with the shorthand property 'font'
      ;; as a Racket object than reset all relevent properties every time after making a new font object.
      (cond [(not (css-wide-keyword? cascaded-value)) cascaded-value]
            [(eq? cascaded-value css:initial) css:initial]
            [(hash? inherited-values) (let-values ([(_ sv) (css-ref-raw inherited-values #true desc-name)]) sv)]
            [(false? inherited-values) css:initial]
            [else css:inherit]))
    (unless (eq? cascaded-value specified-value)
      (hash-set! declared-values desc-name (thunk specified-value)))
    (values cascaded-value specified-value)))

(define css-tee-computed-value : (All (a) (-> CSS-Values Symbol Any a a))
  (lambda [properties desc-name cascaded-value computed-value]
    (unless (eq? cascaded-value computed-value)
      (hash-set! properties desc-name (thunk computed-value)))
    computed-value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define css-car/cdr : (All (a b) (case-> [(Pairof a b) -> (Values a b)]
                                         [(Listof a) -> (Values (U a EOF) (Listof a))]))
  (lambda [pretent-no-whitespace]
    (cond [(null? pretent-no-whitespace) (values eof null)]
          [else (values (car pretent-no-whitespace) (cdr pretent-no-whitespace))])))

(define css-car/cadr : (All (a) (case-> [(Pairof a (Listof a)) -> (Values a (Listof a) (U a EOF) (Listof a))]
                                        [(Listof a) -> (Values (U a EOF) (Listof a) (U a EOF) (Listof a))]))
  (lambda [pretent-no-whitespace]
    (cond [(null? pretent-no-whitespace) (values eof null eof null)]
          [else (let ([1st (car pretent-no-whitespace)]
                      [2nd (cdr pretent-no-whitespace)])
                  (cond [(null? 2nd) (values 1st null eof null)]
                        [else (values 1st 2nd (car 2nd) (cdr 2nd))]))])))

(define css-car : (-> (Listof CSS-Token) (Values CSS-Syntax-Any (Listof CSS-Token)))
  (lambda [dirty]
    (let skip-whitespace ([rest dirty])
      (cond [(null? rest) (values eof null)]
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