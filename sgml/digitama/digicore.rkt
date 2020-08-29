#lang typed/racket/base

(provide (all-from-out racket/bool racket/list racket/format))
(provide (except-out (all-defined-out) xml-make-syntax-error
                     define-tokens define-token define-token-interface
                     define-symbolic-tokens define-syntax-error))

(require racket/fixnum)
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
  
(define-syntax (define-tokens stx)
  (syntax-case stx []
    [(_ token #:+ Token header
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
                (define-typical-tokens group #:+ Group rest ...) ...
                (struct ctoken cparent () #:transparent) ... (define-type CToken ctoken) ...

                (define-type Token-Datum (U False Number (Pairof Number Symbol) Symbolic-Datum ...))
                (define token->datum : (-> Token Token-Datum)
                  (lambda [instance]
                    (cond [(type? instance) (type->datum instance)] ...
                          [else (assert (object-name instance) symbol?)])))))]))

(define-syntax (define-xml stx)
  (syntax-case stx []
    [(_ [token ...])
     (with-syntax ([([line col] ...)
                    (for/list ([<token> (in-list (syntax->list #'(token ...)))])
                      (list (datum->syntax <token> (syntax-line <token>))
                            (datum->syntax <token> (syntax-column <token>))))])
     #'(begin (list 'token line col) ...))]))

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

                (define make+exn : (->* ((U XML-Syntax-Any (Listof XML-Token))) ((Option XML:Name) Log-Level) XML-Syntax-Error)
                  (lambda [v [property #false] [level 'warning]]
                    (define errobj : XML-Syntax-Error (xml-make-syntax-error subexn v))
                    (xml-log-syntax-error errobj property level)
                    errobj))
                ...

                (define throw-exn : (->* ((U XML-Syntax-Any (Listof XML-Token))) ((Option XML:Name) Log-Level) Nothing)
                  (lambda [v [property #false] [level 'warning]]
                    (raise (make+exn v property level))))
                ...))]))

;;; https://drafts.xmlwg.org/xml-syntax/#tokenization
;; https://drafts.xmlwg.org/xml-syntax/#component-value
;; https://drafts.xmlwg.org/xml-syntax/#current-input-token
(define-tokens xml-token #:+ XML-Token
  ([source : (U String Symbol)]
   [line : Positive-Integer]
   [column : Natural]
   [start : Positive-Integer] ; `start` and `end` (instead of `position` and `span`) are required by color lexer.
   [end : Positive-Integer])
  [[xml:open            #:+ XML:Open            #:-> xml:delim]
   [xml:close           #:+ XML:Close           #:-> xml:delim]
   [xml:eq              #:+ XML:Eq              #:-> xml:delim]
   [xml:pe              #:+ XML:PE              #:-> xml:delim]

   [xml:stag            #:+ XML:STag            #:-> xml:open]
   [xml:etag            #:+ XML:ETag            #:-> xml:close]
   [xml:cstag           #:+ XML:CSTag           #:-> xml:delim]
   [xml:oetag           #:+ XML:OETag           #:-> xml:delim]
   
   [xml:pi              #:+ XML:PI              #:-> xml:open]
   [xml:decl            #:+ XML:Decl            #:-> xml:open]

   [xml:&string         #:+ XML:&String         #:-> xml:string]
   
   [xml:comment         #:+ XML:Comment         #:-> xml:whitespace]]

  ; WARNING: Carefully defining types to avoid happening to mess up '(list? datum)'. 
  (define-symbolic-tokens xml-bad-token #:+ XML-Bad-Token
    [xml:bad            #:+ XML:Bad             #:as String])
    
  ; TODO: Typed Racket is buggy if there are more than 11 conditions
  (define-symbolic-tokens xml-symbolic-token #:+ XML-Symbolic-Token
    [xml:delim          #:+ XML:Delim           #:as (U Symbol Char)]
    [xml:name           #:+ XML:Name            #:as Symbol]
    [xml:entity         #:+ XML:Entity          #:as (U Symbol Index)]
    [xml:string         #:+ XML:String          #:as String]
    [xml:keyword        #:+ XML:Keyword         #:as Keyword]
    [xml:whitespace     #:+ XML:WhiteSpace      #:as String]))

;; https://drafts.xmlwg.org/xml-syntax/#style-rules
;; https://drafts.xmlwg.org/selectors/#invalid
(define-syntax-error exn:xml #:as XML-Syntax-Error
  [exn:xml:unrecognized  #:-> exn:xml]
  [exn:xml:range         #:-> exn:xml:unrecognized]
  [exn:xml:eof           #:-> exn:xml:unrecognized]
  [exn:xml:malformed     #:-> exn:xml]
  [exn:xml:end-tag       #:-> exn:xml]
  [exn:xml:missing-name  #:-> exn:xml:malformed]
  [exn:xml:missing-value #:-> exn:xml:malformed]
  [exn:xml:misplaced     #:-> exn:xml:malformed]
  [exn:xml:duplicate     #:-> exn:xml:malformed]
  [exn:xml:unimplemented #:-> exn:xml])

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
    (cond [(xml:name? instance) (symbol->string (xml:name-datum instance))]
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
        [(? xml-token?) (token->exn any)]))))
  
(define xml-log-syntax-error : (->* (XML-Syntax-Error) ((Option XML:Name) Log-Level) Void)
  (lambda [errobj [property #false] [level 'warning]]
    (define logger : Logger (current-logger))
    (define topic : Symbol 'exn:xml:syntax)
    (define msg : String (exn-message errobj))
    (define <eof>? : Boolean (regexp-match? #px"#<eof>" msg))
    (cond [(not property) (log-message logger level topic msg errobj)]
          [(not <eof>?) (log-message logger level topic (format "~a @‹~a›" msg (xml:name-datum property)) errobj)]
          [else (let ([eof-msg (xml-token->string property errobj eof)])
                  (log-message logger level topic (format "~a @‹~a›" eof-msg (xml:name-datum property)) errobj))])))

;;; https://drafts.xmlwg.org/xml-syntax/#parsing
;; Parser Combinators and Syntax Sugars of dealing with declarations for client applications
(define-type XML-Syntax-Any (U XML-Token EOF))
(define-type (XML-Option xml) (U xml XML-Syntax-Error False))
(define-type (XML:Filter xml) (-> XML-Syntax-Any (XML-Option xml)))
