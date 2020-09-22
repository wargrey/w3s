#lang typed/racket/base

(provide (all-from-out racket/list racket/format))
(provide (struct-out W3S-Token) w3s-token-location-string w3s-remake-token)
(provide (except-out (all-defined-out) xml-make-syntax-error
                     define-tokens define-token define-token-interface
                     define-symbolic-tokens))

(require racket/list)
(require racket/format)
(require racket/match)

(require css/digitama/syntax/w3s)

(require (for-syntax racket/base))
(require (for-syntax racket/string))
(require (for-syntax racket/syntax))
(require (for-syntax syntax/parse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type XML-Syntax-Any (U XML-Token EOF))

(define-syntax (define-token-interface stx)
  (syntax-case stx [:]
    [(_ symbolic-prefix : Type id? id-datum #:+ XML:ID #:eq? type=?)
     (with-syntax ([id=<-? (format-id #'symbolic-prefix "~a=<-?" (syntax-e #'symbolic-prefix))]
                   [id=:=? (format-id #'symbolic-prefix "~a=:=?" (syntax-e #'symbolic-prefix))])
       #'(begin (define id=:=? : (-> Any Type (Option Type) : #:+ XML:ID) #| for performance |#
                  (lambda [t v]
                    (and (id? t)
                         (let ([d : Type (id-datum t)])
                           (and (type=? d v) d)))))))]))

(define-syntax (define-token stx)
  (syntax-parse stx #:literals [: Symbol Keyword]
    [(_ id : Identifier parent ((~and (~or Symbol Keyword) Type) #:ci rest ...) #:with id? id-datum)
     (with-syntax ([id=? (format-id #'id "~a=?" (syntax-e #'id))]
                   [id-norm=? (format-id #'id "~a-norm=?" (syntax-e #'id))]
                   [id-norm (format-id #'id "~a-norm" (syntax-e #'id))])
       #'(begin (struct id parent ([datum : Type] [norm : Type] rest ...) #:transparent #:type-name Identifier)
                (define (id=? [t1 : Identifier] [t2 : Identifier]) : Boolean (eq? (id-datum t1) (id-datum t2)))
                (define (id-norm=? [t1 : Identifier] [t2 : Identifier]) : Boolean (eq? (id-norm t1) (id-norm t2)))
                (define-token-interface id : Type id? id-datum #:+ Identifier #:eq? eq?)
                (define-token-interface id-norm : Type id? id-norm  #:+ Identifier #:eq? eq?)))]
    [(_ id : Otherwise parent (Type rest ...) #:with id? id-datum)
     (with-syntax ([type=? (case (syntax-e #'Type) [(String) #'string=?] [(Char) #'char=?] [else #'equal?])]
                   [id=? (format-id #'id "~a=?" (syntax-e #'id))])
       #'(begin (struct id parent ([datum : Type] rest ...) #:transparent #:type-name Otherwise)
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
       #'(begin (struct token xml-token () #:transparent #:type-name Token)
                (define-token id : ID token (Type rest ...) #:with id? id-datum) ...
                (define-type Token-Datum (U Type ...))
                (define (token->datum [t : Token]) : (Option Token-Datum)
                  (cond [(id? t) (id-datum t)] ... [else #false]))))]))
  
(define-syntax (define-tokens stx)
  (syntax-case stx []
    [(_ token header #:+ Token
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
       #'(begin (struct token header () #:transparent #:type-name Token)
                (define-typical-tokens group #:+ Group rest ...) ...
                (struct ctoken cparent () #:transparent #:type-name CToken) ...

                (define-type Token-Datum (U False Symbolic-Datum ...))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-tokens xml-token w3s-token #:+ XML-Token
  [[xml:open            #:+ XML:Open            #:-> xml:delim]
   [xml:close           #:+ XML:Close           #:-> xml:delim]
   [xml:eq              #:+ XML:Eq              #:-> xml:delim]
   [xml:pe              #:+ XML:PE              #:-> xml:delim]

   [xml:stag            #:+ XML:STag            #:-> xml:open]
   [xml:etag            #:+ XML:ETag            #:-> xml:close]
   [xml:cstag           #:+ XML:CSTag           #:-> xml:delim]
   [xml:oetag           #:+ XML:OETag           #:-> xml:delim]

   [xml:csec            #:+ XML:CSec            #:-> xml:open]
   [xml:csec$           #:+ XML:CSec$           #:-> xml:delim]
   
   [xml:pi              #:+ XML:PI              #:-> xml:open]
   [xml:decl            #:+ XML:Decl            #:-> xml:open]

   [xml:&string         #:+ XML:&String         #:-> xml:string]
   [xml:newline         #:+ XML:Newline         #:-> xml:whitespace]
   [xml:comment         #:+ XML:Comment         #:-> xml:whitespace]]

  ; WARNING: Carefully defining types to avoid happening to mess up '(list? datum)'
  ; TODO: Typed Racket is buggy if there are more than 11 conditions

  (define-symbolic-tokens xml-cdata-token #:+ XML-CDATA-Token
    [xml:string         #:+ XML:String          #:as String]
    [xml:whitespace     #:+ XML:WhiteSpace      #:as String])

  (define-symbolic-tokens xml-reference-token #:+ XML-Reference-Token
    [xml:char           #:+ XML:Char            #:as Index]
    [xml:reference      #:+ XML:Reference       #:as Symbol])
  
  (define-symbolic-tokens xml-symbolic-token #:+ XML-Symbolic-Token
    [xml:delim          #:+ XML:Delim           #:as (U Symbol Char)]
    [xml:name           #:+ XML:Name            #:as Symbol]
    [xml:pereference    #:+ XML:PEReference     #:as Keyword]
    [xml:bad            #:+ XML:Bad             #:as (Pairof String Symbol)]))

(define-syntax-error exn:xml #:as XML-Syntax-Error #:for XML-Token
  ;;; https://www.w3.org/TR/xml/#sec-terminology
  #:with [xml-make-syntax-error xml-log-syntax-error]
  [exn:xml:error         #:-> exn:xml]
  [exn:xml:fatal         #:-> exn:xml]
  [exn:xml:eof           #:-> exn:xml]
  [exn:xml:reserved      #:-> exn:xml]
  [exn:xml:multiple      #:-> exn:xml]
  [exn:xml:unimplemented #:-> exn:xml]
  
  [exn:xml:vc            #:-> exn:xml:error]
  [exn:xml:token         #:-> exn:xml:vc]
  [exn:xml:duplicate     #:-> exn:xml:vc]
  [exn:xml:type          #:-> exn:xml:vc]
  [exn:xml:entity        #:-> exn:xml:vc]
  [exn:xml:nest          #:-> exn:xml:vc]
  [exn:xml:enum          #:-> exn:xml:vc]
  
  [exn:xml:wfc           #:-> exn:xml:fatal]
  [exn:xml:unique        #:-> exn:xml:wfc]
  [exn:xml:external      #:-> exn:xml:wfc]
  [exn:xml:foreign       #:-> exn:xml:wfc]
  [exn:xml:char          #:-> exn:xml:wfc]
  [exn:xml:loop          #:-> exn:xml:wfc]
  [exn:xml:misplaced     #:-> exn:xml:wfc]
  [exn:xml:undeclared    #:-> exn:xml:wfc]
  [exn:xml:mismatch      #:-> exn:xml:wfc]
  [exn:xml:malformed     #:-> exn:xml:wfc]

  [exn:xml:missing-name  #:-> exn:xml:malformed]
  [exn:xml:missing-value #:-> exn:xml:malformed]
  
  [exn:xml:space         #:-> exn:xml:char])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-token->syntax : (-> XML-Token Syntax)
  (lambda [instance]
    (datum->syntax #false (xml-token->datum instance)
                   (w3s-token->syntax-location instance))))

(define xml-token-datum->string : (-> XML-Token String)
  (lambda [instance]
    (cond [(xml:name? instance) (symbol->string (xml:name-datum instance))]
          [(xml:string? instance) (~s (xml:string-datum instance))]
          [(xml:bad? instance) (~s (car (xml:bad-datum instance)))]
          [else (~a (xml-token->datum instance))])))

(define xml-token->string : (->* (XML-Token) ((Option Any) (Option Any)) String)
  (lambda [instance [alt-object #false] [alt-datum #false]]
    (string-append (w3s-token-location-string instance) ": "
                   (format "~a: ~a"
                     (or (object-name alt-object) (object-name instance))
                     (or alt-datum (xml-token-datum->string instance))))))
 
(define #:forall (Error) xml-make-syntax-error : (-> (-> String Continuation-Mark-Set (Listof Syntax) Error) (U XML-Syntax-Any (Listof XML-Token)) Error)
  (lambda [exn:xml any]
    (match any
      [(or (? eof-object?) (list)) (exn:xml (~a eof) (current-continuation-marks) null)]
      [(list token) (w3s-token->exn exn:xml xml-token->string xml-token->syntax token)]
      [(list main others ...) (w3s-token->exn exn:xml xml-token->string xml-token->syntax xml-token-datum->string main (filter-not xml:whitespace? others))]
      [(? xml-token?) (w3s-token->exn exn:xml xml-token->string xml-token->syntax any)])))

(define xml-log-syntax-error : (->* (XML-Syntax-Error) ((Option XML-Token) (Option Log-Level)) Void)
  (lambda [errobj [property #false] [level #false]]
    (w3s-log-syntax-error 'exn:xml:syntax xml-token->string xml-token->datum
                          errobj property (or level
                                              (cond [(exn:xml:fatal? errobj) 'error]
                                                    [(exn:xml:error? errobj) 'warning]
                                                    [else 'debug])))))
