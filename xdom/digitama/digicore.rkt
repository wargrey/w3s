#lang typed/racket/base

(provide (all-from-out racket/flonum racket/fixnum racket/bool racket/list racket/format))
(provide (except-out (all-defined-out) xml-make-syntax-error
                     define-tokens define-token define-token-interface
                     define-symbolic-tokens define-numeric-tokens
                     define-syntax-error))

(require racket/fixnum)
(require racket/flonum)
(require racket/list)
(require racket/bool)
(require racket/format)
(require racket/match)

(require "misc.rkt")

(require (for-syntax racket/base))
(require (for-syntax racket/string))
(require (for-syntax racket/syntax))
(require (for-syntax syntax/parse))
  
(define-syntax (define-token-interface stx)
  (syntax-case stx [:]
    [(_ symbolic-prefix : Type id? id-datum #:+ XML:ID #:eq? type=?)
     (with-syntax ([<id> (format-id #'symbolic-prefix "<~a>" (syntax-e #'symbolic-prefix))]
                   [id=<-? (format-id #'symbolic-prefix "~a=<-?" (syntax-e #'symbolic-prefix))]
                   [id=:=? (format-id #'symbolic-prefix "~a=:=?" (syntax-e #'symbolic-prefix))])
       #'(begin (define id=<-? : (All (a) (case-> [Any (-> Type Boolean : #:+ a) -> (Option a) : #:+ XML:ID]
                                                  [Any (U (-> Type Boolean) (Listof Type)) -> (Option Type) : #:+ XML:ID]))
                  (lambda [token range?]
                    (and (id? token)
                         (let ([datum : Type (id-datum token)])
                           (cond [(procedure? range?) (and (range? datum) datum)]
                                 [else (and (member datum range? type=?) datum)])))))

                (define <id> : (All (a) (case-> [(-> Type Boolean : #:+ a) -> (XML:Filter a)]
                                                [(U (-> Type Boolean) (Listof Type) Type) -> (XML:Filter Type)]
                                                [-> (XML:Filter Type)]))
                  (case-lambda
                    [() (λ [[t : XML-Syntax-Any]] (and (id? t) (id-datum t)))]
                    [(range?) (cond [(procedure? range?)
                                     (λ [[t : XML-Syntax-Any]]
                                       (and (id? t)
                                            (or (let ([d : Type (id-datum t)]) (and (range? d) d))
                                                (make-exn:xml:range t))))]
                                    [(list? range?)
                                     (λ [[t : XML-Syntax-Any]]
                                       (and (id? t)
                                            (let ([d : Type (id-datum t)])
                                              (cond [(member d range? type=?) d]
                                                    [else (make-exn:xml:range t)]))))]
                                    [else (λ [[t : XML-Syntax-Any]]
                                            (and (id? t)
                                                 (let ([d : Type (id-datum t)])
                                                   (if (type=? d range?) d (make-exn:xml:range t)))))])]))

                (define id=:=? : (-> Any Type (Option Type) : #:+ XML:ID) #| for performance |#
                  (lambda [t v]
                    (and (id? t)
                         (let ([d : Type (id-datum t)])
                           (and (type=? d v) d)))))))]
    [(_ numeric-prefix : Type id? id-datum #:+ XML:ID #:= type=?)
     (with-syntax ([<id> (format-id #'numeric-prefix "<~a>" (syntax-e #'numeric-prefix))]
                   [id=<-? (format-id #'numeric-prefix "~a=<-?" (syntax-e #'numeric-prefix))])
       #'(begin (define id=<-? : (All (a) (case-> [Any (-> Type Boolean : #:+ a) -> (Option a) : #:+ XML:ID]
                                                  [Any (-> Type Type Boolean) Type -> (Option Type) : #:+ XML:ID]
                                                  [Any Type (-> Type Type Boolean) Type -> (Option Type) : #:+ XML:ID]
                                                  [Any (Listof Type) -> (Option Type) : #:+ XML:ID]))
                  (case-lambda
                    [(token op n)   (and (id? token) (let ([d : Type (id-datum token)]) (and (op d n) d)))]
                    [(token l op r) (and (id? token) (let ([m : Type (id-datum token)]) (and (op l m) (op m r) m)))]
                    [(token range?) (and (id? token) (let ([d : Type (id-datum token)])
                                                       (cond [(procedure? range?) (and (range? d) d)]
                                                             [else (for/or : (Option Type) ([v (in-list range?)])
                                                                     (and (type=? d v) d))])))]))

                (define <id> : (All (a) (case-> [(-> Type Boolean : #:+ a) -> (XML:Filter a)]
                                                [(-> Type Type Boolean) Type -> (XML:Filter Type)]
                                                [Type (-> Type Type Boolean) Type -> (XML:Filter Type)]
                                                [(Listof Type) -> (XML:Filter Type)]
                                                [-> (XML:Filter Type)]))
                  (case-lambda
                    [() (λ [[t : XML-Syntax-Any]] (and (id? t) (id-datum t)))]
                    [(op n) (λ [[t : XML-Syntax-Any]]
                              (and (id? t)
                                   (let ([d : Type (id-datum t)])
                                     (if (op d n) d (make-exn:xml:range t)))))]
                    [(l op r) (λ [[t : XML-Syntax-Any]]
                                (and (id? t)
                                     (let ([m : Type (id-datum t)])
                                       (if (and (op l m) (op m r)) m (make-exn:xml:range t)))))]
                    [(range?) (λ [[t : XML-Syntax-Any]]
                                (and (id? t)
                                     (let ([d : Type (id-datum t)])
                                       (or (cond [(procedure? range?) (and (range? d) d)]
                                                 [(list? range?) (and (member d range? type=?) d)]
                                                 [else (and (type=? d range?) d)])
                                           (make-exn:xml:range t)))))]))))]))

(define-syntax (define-token stx)
  (syntax-parse stx #:literals [: Symbol Keyword]
    [(_ id : Number parent #:as Type #:=? type=? #:with id? id-datum)
     (with-syntax ([id=? (format-id #'id "~a=?" (syntax-e #'id))])
       #'(begin (define-type Number id)
                (struct id parent ([datum : Type]) #:transparent)
                (define (id=? [t1 : Number] [t2 : Number]) : Boolean (type=? (id-datum t1) (id-datum t2)))
                (define-token-interface id : Type id? id-datum #:+ Number #:= type=?)))]
    [(_ id : Identifier parent ((~and (~or Symbol Keyword) Type) #:ci rest ...) #:with id? id-datum)
     (with-syntax ([id=? (format-id #'id "~a=?" (syntax-e #'id))]
                   [id-norm=? (format-id #'id "~a-norm=?" (syntax-e #'id))]
                   [id-norm (format-id #'id "~a-norm" (syntax-e #'id))])
       #'(begin (define-type Identifier id)
                (struct id parent ([datum : Type] [norm : Type] rest ...) #:transparent)
                (define (id=? [t1 : Identifier] [t2 : Identifier]) : Boolean (eq? (id-datum t1) (id-datum t2)))
                (define (id-norm=? [t1 : Identifier] [t2 : Identifier]) : Boolean (eq? (id-norm t1) (id-norm t2)))
                (define-token-interface id : Type id? id-datum #:+ Identifier #:eq? eq?)
                (define-token-interface id-norm : Type id? id-norm  #:+ Identifier #:eq? eq?)))]
    [(_ id : Otherwise parent (Type rest ...) #:with id? id-datum)
     (with-syntax ([type=? (case (syntax-e #'Type) [(String) #'string=?] [(Char) #'char=?] [else #'equal?])]
                   [id=? (format-id #'id "~a=?" (syntax-e #'id))])
       #'(begin (define-type Otherwise id)
                (struct id parent ([datum : Type] rest ...) #:transparent)
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
       #'(begin (struct token xml-token () #:transparent) (define-type Token token)
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
                            (if (eq? (syntax-e <id>) 'xml:url) #'XML-URL-Modifier #'XML-Token)))])
       #'(begin (define-symbolic-tokens token #:+ Token [id #:+ ID #:as Type ... [components : (Listof Component)] [lazy? : Boolean]] ...)

                (define id-copy : (-> ID (Listof Component) Boolean ID)
                  (lambda [instance subcoms ?]
                    (struct-copy id instance [components (if (xml-pair? subcoms) subcoms null)] [lazy? ?])))
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
       #'(begin (struct token xml-numeric () #:transparent) (define-type Token token)
                (define-token id : ID token #:as Type #:=? type=? #:with id? id-datum) ...
                (define (token->datum [t : Token]) : (U Type ...) (cond [(id? t) (id-datum t)] ... [else nan]))))]))
  
(define-syntax (define-tokens stx)
  (syntax-case stx []
    [(_ token #:+ Token header
        [[ptoken #:+ PToken #:-> pparent pfields] ...]
        [[ctoken #:+ CToken #:-> cparent] ...]
        (define-typical-tokens group #:+ Group rest ...) ...)
     (with-syntax ([token->datum (format-id #'token "~a->datum" (syntax-e #'token))]
                   [Token-Datum (format-id #'Token "~a-Datum" (syntax-e #'Token))]
                   [([type? type->datum] ...)
                    (for/list ([<type> (in-list (syntax->list #'(group ...)))]
                               #:unless (eq? (syntax-e <type>) 'xml:dimension))
                      (list (format-id <type> "~a?" (syntax-e <type>))
                            (format-id <type> "~a->datum" (syntax-e <type>))))]
                   [(Symbolic-Datum ...)
                    (for/list ([<define> (in-list (syntax->list #'(define-typical-tokens ...)))]
                               [<Type> (in-list (syntax->list #'(Group ...)))]
                               #:when (eq? (syntax-e <define>) 'define-symbolic-tokens))
                      (format-id <Type> "~a-Datum" (syntax-e <Type>)))])
       #'(begin (struct token header #:transparent) (define-type Token token)
                (struct ptoken pparent pfields #:transparent) ...  (define-type PToken ptoken) ...
                (define-typical-tokens group #:+ Group rest ...) ...
                (struct ctoken cparent () #:transparent) ... (define-type CToken ctoken) ...

                (define-type Token-Datum (U False Number (Pairof Number Symbol) Symbolic-Datum ...))
                (define token->datum : (-> Token Token-Datum)
                  (lambda [instance]
                    (cond [(xml:dimension? instance) (cons (xml:dimension-datum instance) (xml:dimension-unit instance))]
                          [(type? instance) (type->datum instance)] ...
                          [else (assert (object-name instance) symbol?)])))))]))

(define-syntax (define-syntax-error stx)
  (syntax-case stx []
    [(_ exn:xml #:as Syntax-Error [subexn #:-> parent] ...)
     (with-syntax ([([make-exn make+exn throw-exn] ...)
                    (for/list ([<exn> (in-list (syntax->list #'(subexn ...)))])
                      (list (format-id <exn> "make-~a" (syntax-e <exn>))
                            (format-id <exn> "make+~a" (syntax-e <exn>))
                            (format-id <exn> "throw-~a" (syntax-e <exn>))))])
       #'(begin (define-type Syntax-Error exn:xml)
                (struct exn:xml exn:fail:syntax ())
                (struct subexn parent ()) ...

                (define make-exn : (-> (U XML-Syntax-Any (Listof XML-Token)) XML-Syntax-Error)
                  (lambda [v]
                    (xml-make-syntax-error subexn v)))
                ...

                (define make+exn : (->* ((U XML-Syntax-Any (Listof XML-Token))) ((Option XML:Ident) Log-Level) XML-Syntax-Error)
                  (lambda [v [property #false] [level 'warning]]
                    (define errobj : XML-Syntax-Error (xml-make-syntax-error subexn v))
                    (xml-log-syntax-error errobj property level)
                    errobj))
                ...

                (define throw-exn : (->* ((U XML-Syntax-Any (Listof XML-Token))) ((Option XML:Ident) Log-Level) Nothing)
                  (lambda [v [property #false] [level 'warning]]
                    (raise (make+exn v property level))))
                ...))]))

;;; https://drafts.xmlwg.org/xml-syntax/#tokenization
;; https://drafts.xmlwg.org/xml-syntax/#component-value
;; https://drafts.xmlwg.org/xml-syntax/#current-input-token
(define-type XML-URL-Modifier (U XML:Ident XML-Lazy-Token))
(define-type XML-Zero (U XML:Zero XML:Flzero))
(define-type XML-One (U XML:One XML:Flone))

(define-tokens xml-token #:+ XML-Token
  ([source : (U String Symbol)]
   [line : Positive-Integer]
   [column : Natural]
   [start : Positive-Integer] ; `start` and `end` (instead of `position` and `span`) are required by color lexer.
   [end : Positive-Integer])
  [[xml-numeric         #:+ XML-Numeric         #:-> xml-token   ([representation : String])]
   [xml:dimension       #:+ XML:Dimension       #:-> xml-numeric ([datum : Flonum] [unit : Symbol])]]

  [[xml:one             #:+ XML:One             #:-> xml:integer]
   [xml:zero            #:+ XML:Zero            #:-> xml:integer]
     
   [xml:flone           #:+ XML:Flone           #:-> xml:flonum]
   [xml:flzero          #:+ XML:Flzero          #:-> xml:flonum]

   [xml:open            #:+ XML:Open            #:-> xml:delim]
   [xml:colon           #:+ XML:Colon           #:-> xml:delim]
   [xml:semicolon       #:+ XML:Semicolon       #:-> xml:delim]
   [xml:comma           #:+ XML:Comma           #:-> xml:delim]
   [xml:slash           #:+ XML:Slash           #:-> xml:delim]
   [xml:vbar            #:+ XML:VBar            #:-> xml:delim]
   [xml:cdo             #:+ XML:CDO             #:-> xml:cd]
   [xml:cdc             #:+ XML:CDC             #:-> xml:cd]

   [xml:bad:eof         #:+ XML:Bad:EOF         #:-> xml:bad]
   [xml:bad:eol         #:+ XML:Bad:EOL         #:-> xml:bad]
   [xml:bad:char        #:+ XML:Bad:Char        #:-> xml:bad]
   [xml:bad:blank       #:+ XML:Bad:Blank       #:-> xml:bad]
   [xml:bad:range       #:+ XML:Bad:Range       #:-> xml:bad]
   [xml:bad:stdin       #:+ XML:Bad:StdIn       #:-> xml:bad]]

  ; WARNING: Carefully defining types to avoid happening to mess up '(list? datum)'. 
  (define-symbolic-tokens xml-bad-token #:+ XML-Bad-Token
    [xml:bad            #:+ XML:Bad             #:as String]
    [xml:close          #:+ XML:Close           #:as Char])
    
  ; TODO: Typed Racket is buggy if there are more than 11 conditions
  (define-symbolic-tokens xml-symbolic-token #:+ XML-Symbolic-Token
    [xml:delim          #:+ XML:Delim           #:as Char]
    [xml:ident          #:+ XML:Ident           #:as Symbol            #:ci]
    [xml:@keyword       #:+ XML:@Keyword        #:as Keyword           #:ci]
    [xml:hash           #:+ XML:Hash            #:as Keyword]
    [xml:string         #:+ XML:String          #:as String]
    [xml:match          #:+ XML:Match           #:as Char]
    [xml:cd             #:+ XML:CD              #:as Symbol]
    [xml:urange         #:+ XML:URange          #:as (Pairof Index Index)]
    [xml:whitespace     #:+ XML:WhiteSpace      #:as (U String Char)])

  (define-lazy-tokens xml-lazy-token #:+ XML-Lazy-Token
    [xml:url            #:+ XML:URL             #:with modifiers       #:as String]   ; "" means 'about:invalid
    [xml:block          #:+ XML:Block           #:with components      #:as Char]
    [xml:function       #:+ XML:Function        #:with arguments       #:as Symbol #:ci]
    [xml:λracket        #:+ XML:λRacket         #:with arguments       #:as Symbol]
    [xml:var            #:+ XML:Var             #:with fallback        #:as Symbol])

  (define-numeric-tokens xml-number #:+ XML-Number #:nan +nan.0
    [xml:integer        #:+ XML:Integer         #:as Integer]
    [xml:flonum         #:+ XML:Flonum          #:as Flonum])

  (define-numeric-tokens xml-fraction #:+ XML-Fraction #:nan +nan.f
    [xml:percentage     #:+ XML:Percentage      #:as Single-Flonum])

  (define-symbolic-tokens xml-unreadable-token #:+ XML-Unreadable-Token
    ; These tokens are remade by the parser instead of being produced by the tokenizer.
    [xml:ratio          #:+ XML:Ratio           #:as Positive-Exact-Rational]
    [xml:racket         #:+ XML:Racket          #:as Symbol]
    [xml:#:keyword      #:+ XML:#:Keyword       #:as Keyword]))

;; https://drafts.xmlwg.org/xml-syntax/#style-rules
;; https://drafts.xmlwg.org/selectors/#invalid
(define-syntax-error exn:xml #:as XML-Syntax-Error
  [exn:xml:resource           #:-> exn:xml]
  [exn:xml:deprecated         #:-> exn:xml]
  [exn:xml:cyclic             #:-> exn:xml]
  [exn:xml:namespace          #:-> exn:xml]
  [exn:xml:racket             #:-> exn:xml]
  [exn:xml:contract           #:-> exn:xml:racket]
  [exn:xml:unrecognized       #:-> exn:xml]
  [exn:xml:misplaced          #:-> exn:xml:unrecognized]
  [exn:xml:type               #:-> exn:xml:unrecognized]
  [exn:xml:type:identifier    #:-> exn:xml:type]
  [exn:xml:type:variable      #:-> exn:xml:type:identifier]
  [exn:xml:range              #:-> exn:xml:unrecognized]
  [exn:xml:unit               #:-> exn:xml:range]
  [exn:xml:overconsumption    #:-> exn:xml:unrecognized]
  [exn:xml:enclosed           #:-> exn:xml:overconsumption]
  [exn:xml:malformed          #:-> exn:xml]
  [exn:xml:arity              #:-> exn:xml:malformed]
  [exn:xml:empty              #:-> exn:xml:malformed]
  [exn:xml:missing-block      #:-> exn:xml:malformed]
  [exn:xml:missing-value      #:-> exn:xml:malformed]
  [exn:xml:missing-feature    #:-> exn:xml:malformed]
  [exn:xml:missing-delimiter  #:-> exn:xml:malformed]
  [exn:xml:missing-colon      #:-> exn:xml:missing-delimiter]
  [exn:xml:missing-comma      #:-> exn:xml:missing-delimiter]
  [exn:xml:missing-slash      #:-> exn:xml:missing-delimiter])

(define xml-zero? : (-> Any Boolean : #:+ XML-Zero) (λ [v] (or (xml:zero? v) (xml:flzero? v))))
(define xml-one? : (-> Any Boolean : #:+ XML-One) (λ [v] (or (xml:one? v) (xml:flone? v))))

(define xml-nan? : (-> XML-Numeric Boolean)
  (lambda [token]
    (or (and (xml:flonum? token) (eqv? (xml:flonum-datum token) +nan.0))
        (and (xml:dimension? token) (eqv? (xml:dimension-datum token) +nan.0))
        (and (xml:percentage? token) (eqv? (xml:percentage-datum token) +nan.f)))))

(define-syntax (xml-remake-token stx)
  (syntax-case stx []
    [(_ [start-token end-token] make-xml:token datum extra ...)
     #'(make-xml:token (xml-token-source start-token) (xml-token-line start-token)
                       (xml-token-column start-token) (xml-token-start start-token)
                       (xml-token-end end-token) datum extra ...)]
    [(_ here-token make-xml:token datum ...)
     #'(xml-remake-token [here-token here-token] make-xml:token datum ...)]))

(define xml-token->syntax : (-> XML-Token Syntax)
  (lambda [instance]
    (datum->syntax #false (xml-token->datum instance)
                   (vector (xml-token-source instance) (xml-token-line instance) (xml-token-column instance)
                           (xml-token-start instance) (fx- (xml-token-end instance) (xml-token-start instance))))))

(define xml-token-datum->string : (-> XML-Token String)
  (lambda [instance]
    (cond [(xml:ident? instance) (symbol->string (xml:ident-datum instance))]
          [(xml-numeric? instance) (xml-numeric-representation instance)]
          [(xml:@keyword? instance) (keyword->string (xml:@keyword-datum instance))]
          [(xml:hash? instance) (~a "#" (keyword->string (xml:hash-datum instance)))]
          [(xml:match? instance) (~a (xml:match-datum instance) '=)]
          [(xml:delim=:=? instance #\tab) "||"]
          [(xml:string? instance) (~s (xml:string-datum instance))]
          [else (~a (xml-token->datum instance))])))
  
(define xml-token->string : (->* (XML-Token) ((Option Any) (Option Any)) String)
  (lambda [instance [alt-object #false] [alt-datum #false]]
    (format "~a:~a:~a: ~a: ~a" (xml-token-source instance) (xml-token-line instance) (add1 (xml-token-column instance))
            (or (object-name alt-object) (object-name instance))
            (or alt-datum (xml-token-datum->string instance)))))

(define xml-make-syntax-error : (-> (-> String Continuation-Mark-Set (Listof Syntax) XML-Syntax-Error)
                                    (U XML-Syntax-Any (Listof XML-Token))
                                    XML-Syntax-Error)
  (let ([empty-stacks (continuation-marks #false)])
    (lambda [exn:xml any]
      (define (token->exn [main : XML-Token]) : XML-Syntax-Error
        (exn:xml (xml-token->string main exn:xml) empty-stacks (list (xml-token->syntax main))))
      (define (tokens->exn [head : XML-Token] [others : (Listof XML-Token)]) : XML-Syntax-Error
        (exn:xml (format "~a ~a" (xml-token->string head exn:xml) (map xml-token-datum->string others))
                 empty-stacks (map xml-token->syntax (cons head others))))
      (match any
        [(or (? eof-object?) (list)) (exn:xml (~a eof) empty-stacks null)]
        [(list token) (token->exn token)]
        [(list main others ...) (tokens->exn main (filter-not xml:whitespace? others))]
        [(? xml:function? main) (tokens->exn main (xml:function-arguments main))]
        [(? xml:λracket? main) (tokens->exn main (xml:λracket-arguments main))]
        [(? xml:block? main) (tokens->exn main (xml:block-components main))]
        [(? xml-token?) (token->exn any)]))))
  
(define xml-log-syntax-error : (->* (XML-Syntax-Error) ((Option XML:Ident) Log-Level) Void)
  (lambda [errobj [property #false] [level 'warning]]
    (define logger : Logger (current-logger))
    (define topic : Symbol 'exn:xml:syntax)
    (define msg : String (exn-message errobj))
    (define <eof>? : Boolean (regexp-match? #px"#<eof>" msg))
    (cond [(not property) (log-message logger level topic msg errobj)]
          [(not <eof>?) (log-message logger level topic (format "~a @‹~a›" msg (xml:ident-datum property)) errobj)]
          [else (let ([eof-msg (xml-token->string property errobj eof)])
                  (log-message logger level topic (format "~a @‹~a›" eof-msg (xml:ident-datum property)) errobj))])))

;;; https://drafts.xmlwg.org/xml-syntax/#parsing
;; Parser Combinators and Syntax Sugars of dealing with declarations for client applications
(define-type XML-Syntax-Any (U XML-Token EOF))
(define-type (XML-Option xml) (U xml XML-Syntax-Error False))
(define-type (XML:Filter xml) (-> XML-Syntax-Any (XML-Option xml)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-car/cdr : (All (a b) (case-> [(Pairof a b) -> (Values a b)]
                                         [(Listof a) -> (Values (U a EOF) (Listof a))]))
  (lambda [pretent-no-whitespace]
    (cond [(null? pretent-no-whitespace) (values eof null)]
          [else (values (car pretent-no-whitespace) (cdr pretent-no-whitespace))])))

(define xml-car/cadr : (All (a) (case-> [(Pairof a (Listof a)) -> (Values a (Listof a) (U a EOF) (Listof a))]
                                        [(Listof a) -> (Values (U a EOF) (Listof a) (U a EOF) (Listof a))]))
  (lambda [pretent-no-whitespace]
    (cond [(null? pretent-no-whitespace) (values eof null eof null)]
          [else (let ([1st (car pretent-no-whitespace)]
                      [2nd (cdr pretent-no-whitespace)])
                  (cond [(null? 2nd) (values 1st null eof null)]
                        [else (values 1st 2nd (car 2nd) (cdr 2nd))]))])))

(define xml-car : (-> (Listof XML-Token) (Values XML-Syntax-Any (Listof XML-Token)))
  (lambda [dirty]
    (let skip-whitespace ([rest dirty])
      (cond [(null? rest) (values eof null)]
            [else (let-values ([(head tail) (values (car rest) (cdr rest))])
                    (cond [(xml:whitespace? head) (skip-whitespace tail)]
                          [else (values head tail)]))]))))

(define xml-pair? : (All (a) (-> (Listof a) Boolean : #:+ (Listof+ a)))
  (lambda [dirty]
    (and (pair? dirty)
         (let skip-whitespace : Boolean ([head : a (car dirty)]
                                         [tail : (Listof a) (cdr dirty)])
           (implies (xml:whitespace? head)
                    (and (pair? tail)
                         (skip-whitespace (car tail)
                                          (cdr tail))))))))

(define xml-null? : (All (a) (-> (Listof a) Boolean : #:- (Listof+ a)))
  (lambda [dirty]
    (not (xml-pair? dirty))))

(define xml-cons : (All (xml) (-> (U XML-Syntax-Error xml) (Listof xml) (Listof xml)))
  (lambda [item items]
    (cond [(exn? item) items]
          [else (cons item items)])))
