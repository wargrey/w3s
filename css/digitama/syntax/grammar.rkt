#lang typed/racket/base

;;; https://drafts.csswg.org/css-syntax/#parse-a-css-stylesheet

(provide (all-defined-out))

(require racket/string)
(require racket/path)

(require "digicore.rkt")
(require "parser.rkt")
(require "selector.rkt")
(require "condition.rkt")
(require "misc.rkt")

;; https://drafts.csswg.org/css-syntax/#css-stylesheets
(define-type CSS-Stylesheet-Pool (HashTable Natural CSS-Stylesheet))
(define-type CSS-Grammar-Rule (U CSS-Style-Rule CSS-@Rule CSS-@Media-Rule CSS-@Supports-Rule))

(struct css-@import-rule
  ([identity : Positive-Integer]
   [supports : (Option CSS-Feature-Query)]
   [media-list : (Listof CSS-Media-Query)])
  #:transparent
  #:type-name CSS-@Import-Rule)

(struct css-@media-rule
  ([queries : (Listof CSS-Media-Query)]
   [grammars : (Listof CSS-Grammar-Rule)]
   [viewports : (Vectorof (Listof CSS-Declaration))])
  #:transparent
  #:type-name CSS-@Media-Rule)

(struct css-@supports-rule
  ([query : CSS-Feature-Query]
   [grammars : (Listof CSS-Grammar-Rule)])
  #:transparent
  #:type-name CSS-@Supports-Rule)

(struct css-@racket-rule
  ([name : Symbol]
   [sexps : (Listof CSS-Token)])
  #:transparent
  #:type-name CSS-@Racket-Rule)

(struct css-style-rule
  ([selectors : (Listof+ CSS-Complex-Selector)]
   [properties : (Listof CSS-Declaration)])
  #:transparent
  #:type-name CSS-Style-Rule)

(define-preference css-stylesheet : CSS-Stylesheet
  ([location : (U String Symbol)                    #:= '/dev/null]
   [namespaces : (Listof (Pairof Symbol String))    #:= null]
   [imports : (Listof CSS-@Import-Rule)             #:= null]
   [grammars : (Listof CSS-Grammar-Rule)            #:= null]
   [pool : CSS-Stylesheet-Pool                      #:= (make-hash)]
   [timestamp : Integer                             #:= 0]
   [viewports : (Vectorof (Listof CSS-Declaration)) #:= (vector)]
   [modules : (Listof CSS-@Racket-Rule)             #:= null])
  #:transparent)

(define css-stylesheet-placeholder : CSS-Stylesheet (make-css-stylesheet))

(define-css-parser-entry read-css-stylesheet #:-> CSS-Stylesheet
  ;;; https://drafts.csswg.org/css-syntax/#parse-a-css-stylesheet
  ;;; https://drafts.csswg.org/css-syntax/#charset-rule
  ;;; https://drafts.csswg.org/css-namespaces
  ;;; https://drafts.csswg.org/css-cascade/#at-import
  ;;; https://drafts.csswg.org/css-conditional/#processing
  ;;; https://drafts.csswg.org/css-conditional/#contents-of
  ;;; https://drafts.csswg.org/css-device-adapt/#media-queries
  (lambda [/dev/cssin [pool : CSS-Stylesheet-Pool ((inst make-hash Natural CSS-Stylesheet))]]
    (define location : (U String Symbol) (let ([p (object-name /dev/cssin)]) (if (symbol? p) p (~a p))))
    (define identity : Natural (if (string? location) (css-stylesheet-path->identity location) 0))
    (or (and (string? location) (hash-has-key? pool identity)
             (let ([stylesheet (hash-ref pool identity)])
               (and (not (css-stylesheet-outdated? stylesheet))
                    (css-update-imported-stylesheets stylesheet)
                    stylesheet)))
        (let ([rules (css-consume-stylesheet /dev/cssin)])
          (when (positive? identity) (hash-set! pool identity css-stylesheet-placeholder))
          (define-values (namespaces viewports imports grammars modules) (css-syntax-rules->grammar-rules location rules #true #true pool))
          (define timestamp : Integer (if (string? location) (file-or-directory-modify-seconds location) (current-seconds)))
          (define stylesheet : CSS-Stylesheet
            (make-css-stylesheet #:location location #:namespaces namespaces #:imports imports #:grammars grammars
                                 #:pool pool #:timestamp timestamp #:viewports viewports #:modules modules))
          (when (positive? identity) (hash-set! pool identity stylesheet))
          stylesheet))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define css-syntax-rules->grammar-rules
  : (->* ((U String Symbol) (Listof CSS-Syntax-Rule) Boolean Boolean) (CSS-Stylesheet-Pool)
         (Values (Listof (Pairof Symbol String)) (Vectorof (Listof CSS-Declaration)) (Listof CSS-@Import-Rule)
                 (Listof CSS-Grammar-Rule) (Listof CSS-@Racket-Rule)))
  (lambda [src all-syntaxes can-import0? allow-namespace0? [pool ((inst make-hasheq Natural CSS-Stylesheet))]]
    (let syntax->grammar  ([seititnedi : (Listof CSS-@Import-Rule) null]
                           [srammarg : (Listof CSS-Grammar-Rule) null]
                           [srotpircsed : (Listof (Listof CSS-Declaration)) null]
                           [secapseman : (Listof (Pairof Symbol String)) null]
                           [seludom : (Listof CSS-@Racket-Rule) null]
                           [syntaxes : (Listof CSS-Syntax-Rule) all-syntaxes]
                           [can-import? : Boolean can-import0?]
                           [allow-namespace? : Boolean allow-namespace0?])
      (if (null? syntaxes)
          (values (reverse secapseman) (list->vector (reverse srotpircsed)) (reverse seititnedi) (reverse srammarg) (reverse seludom))
          (let-values ([(stx rest) (values (car syntaxes) (cdr syntaxes))])
            (if (css-qualified-rule? stx)
                (let ([?rule (css-qualified-rule->style-rule stx secapseman)])
                  (syntax->grammar seititnedi (if (css-style-rule? ?rule) (cons ?rule srammarg) srammarg) srotpircsed
                                   secapseman seludom rest #false #false))
                (case (css:@keyword-norm (css-@rule-name stx))
                  [(#:@media)
                   (let ([?rule (css-@condition->conditional-rule stx pool)])
                     (syntax->grammar seititnedi (if (css-@media-rule? ?rule) (cons ?rule srammarg) srammarg)
                                      srotpircsed secapseman seludom rest #false #false))]
                  [(#:@supports)
                   (let ([?rule (css-@condition->conditional-rule stx pool)])
                     (syntax->grammar seititnedi (if (css-@supports-rule? ?rule) (cons ?rule srammarg) srammarg)
                                      srotpircsed secapseman seludom rest #false #false))]
                  [(#:@viewport)
                   (let ([?desc (css-@viewport->declarations stx)])
                     (syntax->grammar seititnedi srammarg (if (pair? ?desc) (cons ?desc srotpircsed) srotpircsed)
                                      secapseman seludom rest #false #false))]
                  [(#:@namespace)
                   (let ([?ns (if allow-namespace? (css-@namespace->namespace stx) (make+exn:css:misplaced (css-@rule-name stx)))])
                     (syntax->grammar seititnedi srammarg srotpircsed (if (pair? ?ns) (cons ?ns secapseman) secapseman)
                                      seludom rest #false allow-namespace?))]
                  [(#:@import)
                   (let ([?rule (cond [(false? can-import?) (make+exn:css:misplaced (css-@rule-name stx))]
                                      [else (css-@import->import-rule stx src pool)])])
                     (syntax->grammar (if (css-@import-rule? ?rule) (cons ?rule seititnedi) seititnedi) srammarg
                                      srotpircsed secapseman seludom rest can-import? allow-namespace?))]
                  [(#:@charset)
                   (make+exn:css:misplaced (css-@rule-name stx))
                   (syntax->grammar seititnedi srammarg srotpircsed secapseman seludom rest can-import? allow-namespace?)]
                  [(#:@racket)
                   (let ([?module+ (css-@racket->module+ stx)])
                     (syntax->grammar seititnedi srammarg srotpircsed secapseman
                                      (if (exn:css? ?module+) seludom (cons ?module+ seludom))
                                      rest #false #false))]
                  [else (syntax->grammar seititnedi (cons stx srammarg) srotpircsed secapseman seludom rest #false #false)])))))))

(define css-@viewport->declarations : (-> CSS-@Rule (U (Listof CSS-Declaration) CSS-Syntax-Error))
  (lambda [viewport]
    (define prelude : (Listof CSS-Token) (css-@rule-prelude viewport))
    (define ?block : (Option CSS:Block) (css-@rule-block viewport))
    (cond [(css-pair? prelude) (make+exn:css:overconsumption prelude)]
          [(and ?block) (css-components->declarations (css:block-components ?block))]
          [else (make+exn:css:missing-block (css-@rule-name viewport))])))

(define css-@import->import-rule : (-> CSS-@Rule Any CSS-Stylesheet-Pool (U CSS-@Import-Rule False CSS-Syntax-Error))
  ;;; https://drafts.csswg.org/css-cascade/#at-import
  (lambda [import parent-href pool]
    (define-values (uri ?condition) (css-car (css-@rule-prelude import)))
    (define name : CSS:@Keyword (css-@rule-name import))
    (define ?block : (Option CSS:Block) (css-@rule-block import))
    (define ?target.css : (U Path CSS-Syntax-Error)
      (cond [(not uri) (make+exn:css:empty (css-@rule-name import))]
            [(css:string=<-? uri non-empty-string?) => (λ [url] (css-url-string->path parent-href url))]
            [(css:url=<-? uri non-empty-string?) => (λ [url] (css-url-string->path parent-href url))]
            [(or (css:string? uri) (css:url? uri)) (make+exn:css:empty uri)]
            [else (make+exn:css:type uri)]))
    (cond [(exn? ?target.css) ?target.css]
          [(css:block? ?block) (make+exn:css:overconsumption ?block)]
          [(false? (regexp-match? #px"\\.css$" ?target.css)) (make+exn:css:resource uri)]
          [(false? (file-exists? ?target.css)) (make+exn:css:resource uri)]
          [else (let-values ([(?supports ?media-list) (css-car ?condition)])
                  (define-values (?query ?queries)
                    (if (css:function-norm=:=? ?supports 'supports)
                        (values (css-components->feature-query (css:function-arguments ?supports) #false name)
                                (css-parse-media-queries ?media-list name))
                        (values #false (css-parse-media-queries ?condition name))))
                  (read-css-stylesheet ?target.css pool)
                  (css-@import-rule (css-stylesheet-path->identity ?target.css) ?query ?queries))])))

(define css-@namespace->namespace : (-> CSS-@Rule (U (Pairof Symbol String) CSS-Syntax-Error))
  ;;; https://drafts.csswg.org/css-namespaces/#syntax
  (lambda [ns]
    (define-values (1st rest) (css-car (css-@rule-prelude ns)))
    (define-values (2nd terminal) (css-car rest))
    (define ?block : (Option CSS:Block) (css-@rule-block ns))
    (define namespace : (U String CSS-Syntax-Error)
      (let ([uri (if (not 2nd) 1st 2nd)])
        (cond [(css:string? uri) (css:string-datum uri)]
              [(css:url? uri) (css:url-datum uri)]
              [(not 1st) (make+exn:css:empty (css-@rule-name ns))]
              [else (make+exn:css:type uri)])))
    (cond [(exn? namespace) namespace]
          [(css:block? ?block) (make+exn:css:overconsumption ?block)]
          [(css-pair? terminal) (make+exn:css:overconsumption terminal)]
          [(css:ident? 1st) (cons (css:ident-datum 1st) namespace)]
          [(not 2nd) (cons '|| namespace)]
          [else (make+exn:css:type 1st)])))

(define css-@condition->conditional-rule : (-> CSS-@Rule CSS-Stylesheet-Pool (U CSS-@Media-Rule CSS-@Supports-Rule CSS-Syntax-Error Void))
  ;;; https://drafts.csswg.org/css-conditional/#contents-of
  ;;; https://drafts.csswg.org/css-conditional/#at-supports
  ;;; https://drafts.csswg.org/mediaqueries/#mq-syntax
  (lambda [condition pool]
    (define name : CSS:@Keyword (css-@rule-name condition))
    (define ?block : (Option CSS:Block) (css-@rule-block condition))
    (cond [(false? ?block) (make+exn:css:missing-block name)]
          [(css-null? (css:block-components ?block)) (void)]
          [else (let ([stxes : (Listof CSS-Syntax-Rule) (css-parse-rules (css:block-components ?block))])
                  (when (pair? stxes)
                    (define-values (_ns viewports _vp grammars modules)
                      (css-syntax-rules->grammar-rules 'src stxes #false #false pool))
                    (when (pair? grammars)
                      (if (eq? (css:@keyword-norm name) '#:@media)
                          (css-@media-rule (css-parse-media-queries (css-@rule-prelude condition) name) grammars viewports)
                          (let ([supports (css-parse-feature-query (css-@rule-prelude condition) name)])
                            (if (exn:css? supports) supports (css-@supports-rule supports grammars)))))))])))
  
(define css-qualified-rule->style-rule : (-> CSS-Qualified-Rule (Listof (Pairof Symbol String)) (U CSS-Style-Rule Void CSS-Syntax-Error))
  ;;; https://drafts.csswg.org/css-syntax/#style-rules
  ;;; https://drafts.csswg.org/selectors/#invalid
  (lambda [qr namespaces]
    (define prelude : (Listof+ CSS-Token) (css-qualified-rule-prelude qr))
    (define components : (Listof CSS-Token) (css:block-components (css-qualified-rule-block qr)))
    (define ?selectors : (U (Listof+ CSS-Complex-Selector) CSS-Syntax-Error) (css-components->selectors prelude namespaces))
    (cond [(exn? ?selectors) ?selectors]
          [else (let ([decls (css-components->declarations components)])
                  (when (pair? decls)
                    (css-style-rule ?selectors decls)))])))

(define css-@racket->module+ : (-> CSS-@Rule (U CSS-@Racket-Rule CSS-Syntax-Error))
  ;;; extension rule to define submodules for testing
  (lambda [viewport]
    (define-values (modname terminal) (css-car (css-@rule-prelude viewport)))
    (define ?block : (Option CSS:Block) (css-@rule-block viewport))
    (define name : (U Symbol CSS-Syntax-Error)
      (cond [(css:ident? modname) (css:ident-datum modname)]
            [(not modname) 'test]
            [else (make+exn:css:type modname)]))
    (cond [(exn? name) name]
          [(css-pair? terminal) (make+exn:css:overconsumption terminal)]
          [(and ?block) (css-@racket-rule name (filter-not css:whitespace? (css:block-components ?block)))]
          [else (make+exn:css:missing-block (css-@rule-name viewport))])))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define css-stylesheet-path->identity : (-> Path-String Positive-Integer)
  (lambda [uri.css]
    (file-or-directory-identity uri.css)))

(define css-stylesheet-outdated? : (-> CSS-Stylesheet Boolean)
  (lambda [stylesheet]
    (define location : (U String Symbol) (css-stylesheet-location stylesheet))
    (and (string? location)
         (file-exists? location)
         (< (css-stylesheet-timestamp stylesheet)
            (file-or-directory-modify-seconds location)))))

(define css-update-imported-stylesheets : (-> CSS-Stylesheet Void)
  (lambda [stylesheet]
    (define pool : CSS-Stylesheet-Pool (css-stylesheet-pool stylesheet))
    (for ([children (in-list (css-stylesheet-imports stylesheet))])
      (define id : (Option CSS-Stylesheet) (hash-ref pool (css-@import-rule-identity children) (λ _ #false)))
      (when (css-stylesheet? id)
        (define child.css : (U Symbol String) (css-stylesheet-location id))
        (when (string? child.css)
          (read-css-stylesheet (string->path child.css) pool))))))
  
(define css-url-string->path : (-> Any String Path)
  (lambda [parent-location uri]
    (define uri.css : Path-String
      (cond [(absolute-path? (string->path uri)) uri]
            [else (let ([pwd (or (and (string? parent-location) (path-only parent-location)) (current-directory))])
                    (build-path pwd uri))]))
    (simple-form-path uri.css)))
