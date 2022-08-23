#lang typed/racket/base

(provide (all-defined-out))

(require bitmap/color)
(require bitmap/digitama/color)

(require racket/symbol)

(require digimon/string)
(require digimon/syntax)
(require digimon/dimension)

(require sgml/digitama/grammar)
(require sgml/digitama/digicore)
(require sgml/digitama/datatype)

(require css/digitama/syntax/dimension)

(require (for-syntax syntax/parse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type SVG-Paint-Server-Fallback (U SVG:KW:Paint FlColor (Pairof FlColor SVG-ICCColor)))
(define-type SVG-Paint (U 'inherit SVG-Paint-Server-Fallback String SVG-Paint-Server))

(struct svg-icccolor ([name : Symbol] [components : (Pairof Flonum (Listof Flonum))]) #:type-name SVG-ICCColor #:transparent)
(struct svg-paint-server ([url : String] [fallback : SVG-Paint-Server-Fallback]) #:type-name SVG-Paint-Server #:transparent)

(struct svg-transform () #:type-name SVG-Transform #:transparent)
(struct svg:matrix svg-transform ([a : Flonum] [b : Flonum] [c : Flonum] [d : Flonum] [e : Flonum] [f : Flonum]) #:type-name SVG:Matrix #:transparent)
(struct svg:translate svg-transform ([tx : Flonum] [ty : (Option Flonum)]) #:type-name SVG:Translate #:transparent)
(struct svg:scale svg-transform ([sx : Flonum] [sy : (Option Flonum)]) #:type-name SVG:Scale #:transparent)
(struct svg:rotate svg-transform ([angle : Flonum] [cx : (Option Flonum)] [cy : (Option Flonum)]) #:type-name SVG:Rotate #:transparent)
(struct svg:skewX svg-transform ([angle : Flonum]) #:type-name SVG:SkewX #:transparent)
(struct svg:skewY svg-transform ([angle : Flonum]) #:type-name SVG:SkewY #:transparent)

(define svg-list-separator/comma : Regexp #px"\\s*,\\s*")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (define-svg-dimension stx)
  (syntax-parse stx #:literals [:]
    [(_ dim [canonical-unit unit ...]
        (~optional [suffix ...] #:defaults ([(suffix 1) null]))
        (~optional (~seq #:with [env ...]) #:defaults ([(env 1) null]))
        (~optional (~seq #:alias [alias:id ...]) #:defaults ([(alias 1) null])))
     (with-syntax* ([(svg:attr-value*+>dim svg:attr-value*+>alias ...) (map-identifiers #'(dim alias ...) "svg:attr-value*+>~a")]
                    [(svg:attr-value*->dim svg:attr-value*->alias ...) (map-identifiers #'(dim alias ...) "svg:attr-value*->~a")]
                    [(svg:attr-dim->value  svg:attr-alias->value ...)  (map-identifiers #'(dim alias ...) "svg:attr-~a->value")]
                    [dim-all-units (make-identifier #'dim "~a-units")])
       (syntax/loc stx
         (begin (provide (rename-out [svg:attr-value*+>dim svg:attr-value*+>alias]
                                     [svg:attr-value*->dim svg:attr-value*->alias]
                                     [svg:attr-dim->value  svg:attr-alias->value]))
                ...

                (define dim-units : (Listof Symbol) '(canonical-unit unit ...))

                ;;; NOTE
                ; SVG's units are subsets of CSS's, but client-code shouldn't be bothered by that,
                ;   so here we only report errors, leaving errors to the renderer, which might deal
                ;   with them in a more intelligent way.    
                (define svg:dim-filter : (All (a) (-> XML-Element-Attribute-Value* (Option (Pairof a Symbol)) (XML-Option (Pairof a Symbol))))
                  (lambda [value datum]
                    (cond [(not datum) (make+exn:svg:range value)]
                          [else (let ([u (cdr datum)])
                                  (case (cdr datum)
                                    [(suffix) datum] ...
                                    [else (cond [(memq u dim-units) datum]
                                                [(memq u dim-all-units) (make+exn:svg:unit value) datum]
                                                [else (make+exn:svg:unit value)])]))])))

                (define svg:attr-value*+>dim : (-> XML-Element-Attribute-Value* (XML-Option (Pairof Nonnegative-Flonum Symbol)))
                  (lambda [value]
                    (svg:dim-filter value (xml:attr-value*+>dimension value 'canonical-unit))))

                (define svg:attr-value*->dim : (-> XML-Element-Attribute-Value* (XML-Option (Pairof Flonum Symbol)))
                  (lambda [value]
                    (svg:dim-filter value (xml:attr-value*->dimension value 'canonical-unit))))

                (define svg:attr-dim->value : (-> (Pairof Real Symbol) String)
                  (lambda [datum]
                    (define-values (n u) (values (real->double-flonum (car datum)) (cdr datum)))
                    (xml:attr-dimension->value
                     (cond [(eq? u 'suffix) (cons (car datum) u)] ...
                           [(memq u dim-units) (cons n u)]
                           [else (cons (dim n u env ...) 'canonical-unit)])))))))]))

(define-syntax (define-svg-lists stx)
  (syntax-case stx [:]
    [(_ [name : Type attr->datum] ...)
     (with-syntax* ([(name->datum-list ...) (map-identifiers #'(name ...) "svg:attr-value*->~a-list")])
       (syntax/loc stx
         (begin (define name->datum-list : (-> XML-Element-Attribute-Value* (XML-Option (Listof Type)))
                  (lambda [v]
                    (define ls (xml:attr-value*->listof-type v attr->datum make+exn:svg:range svg-list-separator/comma))

                    (cond [(null? ls) (make+exn:svg:malformed v)]
                          [else ls])))
                ...)))]))

(define-syntax (define-svg-keywords stx)
  (syntax-case stx [:]
    [(_ [name : ID [default keywords ...]] ...)
     (with-syntax* ([(ids ...) (map-identifiers #'(name ...) "svg:kw:~as")]
                    [(id? ...) (map-identifiers #'(name ...) "svg:kw:~a?")]
                    [(Type ...) (map-identifiers #'(ID ...) "SVG:KW:~a")]
                    [(name->value ...) (map-identifiers #'(name ...) "svg:attr-value*->kw:~a")])
       (syntax/loc stx
         (begin (define-type Type (U 'default 'keywords ...)) ...

                (define ids : (Pairof Type (Listof Type)) (list 'default 'keywords ...)) ...

                (define id? : (-> Any Boolean : #:+ Type)
                  (lambda [v]
                    (or (eq? v 'default)
                        (eq? v 'keywords) ...)))
                ...
                
                (define name->value : (-> XML-Element-Attribute-Value* (XML-Option Type))
                  (lambda [v]
                    (define kw : Symbol (xml:attr-value*->symbol v))

                    (and (id? kw) kw)))
                ...)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-svg-dimension dim:angle     [deg rad grad])
(define-svg-dimension dim:frequency [Hz kHz])
(define-svg-dimension dim:length    [px em ex in cm mm pt pc] [%] #:with [css-dimenv] #:alias [coordinate])
(define-svg-dimension dim:time      [s ms])

(define-svg-lists
  [length : XML-Dimension svg:attr-value*->dim:length])

(define-svg-keywords
  [paint : Paint [none currentColor]])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NOTE
; Normalizing the attributes' values require DTD or other schema,
; These APIs are designed to manually normalizing,
; Thus, the input tokens are usually XML:String instances.

(define svg:attr-value*->color : (-> XML-Element-Attribute-Value* (XML-Option FlColor))
  (lambda [v]
    (cond [(xml:string? v) (svg-color-filter v (string-trim (xml:string-datum v)))]
          [(xml:name? v) (svg-color-filter v (symbol->immutable-string (xml:name-datum v)))]
          [else #false])))

(define svg:attr-value*->icc-color : (-> XML-Element-Attribute-Value* (XML-Option SVG-ICCColor))
  (lambda [v]
    (cond [(xml:string? v) (svg-icc-color-filter v (string-trim (xml:string-datum v)))]
          [(xml:name? v) (svg-icc-color-filter v (symbol->immutable-string (xml:name-datum v)))]
          [else #false])))

(define svg:attr-value*->IRI : (-> XML-Element-Attribute-Value* (XML-Option String))
  (lambda [v]
    (cond [(xml:string? v) (svg-IRI-filter v (string-trim (xml:string-datum v)))]
          [(xml:name? v) (svg-IRI-filter v (symbol->immutable-string (xml:name-datum v)))]
          [else #false])))

(define svg:attr-value*->name : (-> XML-Element-Attribute-Value* (XML-Option Symbol))
  (lambda [v]
    (define s (xml:attr-value*->string v))

    (and (not (regexp-match? #px"[,()]|\\s" s))
         (string->symbol s))))

(define svg:attr-value*->number-pair : (-> XML-Element-Attribute-Value* (XML-Option (U (Pairof Real Real) Real)))
  (lambda [v]
    (define ns (xml:attr-value*->listof-type v xml:attr-value*->number make+exn:svg:range svg-comma-wsp-list-split))

    (cond [(null? ns) (make+exn:svg:malformed v)]
          [(null? (cdr ns)) (car ns)]
          [(pair? (cddr ns)) (make+exn:svg:malformed v) (cons (car ns) (cadr ns))]
          [else (cons (car ns) (cadr ns))])))

(define svg:attr-value*->integer-pair : (-> XML-Element-Attribute-Value* (XML-Option (U (Pairof Integer Integer) Integer)))
  (lambda [v]
    (define ns (xml:attr-value*->listof-type v xml:attr-value*->integer make+exn:svg:range svg-comma-wsp-list-split))

    (cond [(null? ns) (make+exn:svg:malformed v)]
          [(null? (cdr ns)) (car ns)]
          [(pair? (cddr ns)) (make+exn:svg:malformed v) (cons (car ns) (cadr ns))]
          [else (cons (car ns) (cadr ns))])))

(define svg:attr-value*->paint : (-> XML-Element-Attribute-Value* (XML-Option SVG-Paint))
  (lambda [v]
    (cond [(xml:string? v) (svg-paint-filter v (svg-wsp-list-split v (xml:string-datum v)) #true)]
          [(xml:name? v) (svg-paint-filter v (list (symbol->immutable-string (xml:name-datum v))) #true)]
          [(pair? v) (svg-paint-filter (car v) (for/list : (Listof String) ([t (in-list v)]) (symbol->immutable-string (xml:name-datum t))) #true)]
          [else #false])))

(define svg:attr-value*->transform-list : (-> XML-Element-Attribute-Value* (XML-Option (Listof SVG-Transform)))
  (lambda [v]
    (and (xml:string? v)
         (svg-transform-list-filter v (xml:string-datum v)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define svg:attr-url->value : (-> String String)
  (lambda [url]
    (string-append "url(" url ")")))

(define svg:attr-icccolor->value : (-> SVG-ICCColor String)
  (lambda [iccc]
    (string-append "icc-color("
                   (symbol->immutable-string (svg-icccolor-name iccc))
                   (string-join (map number->string (svg-icccolor-components iccc)) ", " #:before-first ", ")
                   ")")))

(define svg:attr-color->value : (-> FlColor String)
  (lambda [c]
    (cond [(rgban? c) (symbol->immutable-string (rgban-name c))]
          [(rgba? c)
           (let ([rgb (flcolor->byte-list c)])
             (format "rgb(~a, ~a, ~a)"
               (car rgb) (cadr rgb) (caddr rgb)))]
          [else ; (flcolor? c)
           (let ([hex (flcolor->hex c)])
             (string-append "#" (~r hex #:min-width 6 #:pad-string "0" #:base 16)))])))

(define svg:attr-paint->value : (-> (U SVG-Paint String) String)
  (lambda [p]
    (cond [(symbol? p) (symbol->immutable-string p)]
          [(flcolor? p) (svg:attr-color->value p)]
          [(pair? p) (string-append (svg:attr-color->value (car p)) " " (svg:attr-icccolor->value (cdr p)))]
          [(svg-paint-server? p) (string-append (svg:attr-paint->value (svg-paint-server-url p)) " " (svg:attr-paint->value (svg-paint-server-fallback p)))]
          [else (svg:attr-url->value p)])))

(define svg:attr-transform-list->value : (-> (Listof SVG-Transform) String)
  (lambda [ts]
    (string-join (map svg:attr-transform->value ts) " ")))

(define svg:attr-transform->value : (-> SVG-Transform String)
  (lambda [t]
    (define name : String (substring (symbol->immutable-string (assert (object-name t) symbol?)) 4))
    (define argv : (Listof String)
      (for/list : (Listof String) ([arg (in-vector (struct->vector t))] #:when (flonum? arg))
        (cond [(integer? arg) (number->string (inexact->exact arg))]
              [else (number->string arg)])))
    
    (string-join #:before-first (string-append name "(")
                 #:after-last ")"
                 argv ", ")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define svg-function-filter : (case-> [XML-Token String String True -> (XML-Option String)]
                                      [XML-Token String String False -> (Option String)])
  (lambda [token v func-name report-function-error?]
    (define open-idx (string-length func-name))
    (define clse-idx (- (string-length v) 1))
    
    (cond [(>= open-idx clse-idx) #false]
          [(and (string-prefix? v func-name) (eq? (string-ref v clse-idx) #\)))
           (let skip-wsp ([idx : Nonnegative-Fixnum open-idx])
             (define idx++ (+ idx 1))
             (cond [(eq? (string-ref v idx) #\() (substring v idx++ clse-idx)]
                   [(< idx++ clse-idx) (skip-wsp idx++)]
                   [else #false]))]
          [(regexp-match? #px"^\\w+\\s*[(][^)]*[)]" v)
           (and report-function-error? (make+exn:svg:function token))]
          [else #false])))

(define svg-color-filter : (case-> [XML-Token String -> (XML-Option FlColor)]
                                   [XML-Token String False -> (XML-Option FlColor)]
                                   [XML-Token String True -> (XML-Option (U FlColor SVG:KW:Paint))])
  (lambda [token v [for-paint? #false]]
    (cond [(string=? v "") (make+exn:xml:missing-value token)]
          [(eq? (string-ref v 0) #\#)
           (let ([maybe-rgb (css-#hex-color->rgb (substring v 1))])
             (cond [(symbol? maybe-rgb) (make+exn:svg:digit token)]
                   [(and maybe-rgb) (hexa maybe-rgb 1.0)]
                   [else (make+exn:svg:unrecognized token)]))]
          [else (let ([func-body (svg-function-filter token v "rgb" #true)])
                  (cond [(string? func-body)
                         (let ([cs (svg-comma-wsp-list-split token func-body)])
                           (cond [(and (pair? cs) (pair? (cdr cs)) (pair? (cddr cs)) (null? (cdddr cs)))
                                  (let*-values ([(_r _g _b) (values (car cs) (cadr cs) (caddr cs))]
                                                [(r u) (string->integer-dimension _r)])
                                    (cond [(and r (or (eq? u '||) (eq? u '%)))
                                           (let-values ([(g gu) (string->integer-dimension _g)]
                                                        [(b bu) (string->integer-dimension _b)])
                                             (cond [(not (and g b)) (make+exn:svg:range token)]
                                                   [(not (and (eq? u gu) (eq? u bu))) (make+exn:svg:malformed token)]
                                                   [else (let ([denominator (if (eq? u '%) 100.0 255.0)])
                                                           (rgb (/ (inexact->exact r) denominator)
                                                                (/ (inexact->exact g) denominator)
                                                                (/ (inexact->exact b) denominator)))]))]
                                          [else (make+exn:svg:range token)]))]
                                 [(regexp-match? svg-list-separator/comma func-body) (make+exn:svg:malformed token)]
                                 [else (make+exn:svg:missing-comma token)]))]
                        [(exn:xml? func-body) func-body]
                        [else (let ([name-kw (string->symbol v)])
                                (or (and for-paint? (svg:kw:paint? name-kw) name-kw)
                                    (named-rgba name-kw 1.0 rgb* #true)
                                    (named-rgba (string->symbol (string-downcase v)) 1.0 rgb* #true)))]))])))

(define svg-icc-color-filter : (-> XML-Token String (XML-Option SVG-ICCColor))
  (lambda [token v]
    (define icc-body (svg-function-filter token v "icc-color" #true))

    (cond [(string? icc-body)
           (let ([icccs (svg-comma-wsp-list-split token icc-body)])
             (cond [(and (pair? icccs) (pair? (cdr icccs)))
                    (let ([name (svg:attr-value*->name (syn-remake-token token xml:string (car icccs)))])
                      (cond [(symbol? name)
                             (let collect-component ([comps : (Listof String) (cdr icccs)]
                                                     [spmoc : (Listof Flonum) null])
                               (cond [(pair? comps)
                                      (let-values ([(self rest) (values (car comps) (cdr comps))])
                                        (let* ([<comp> (syn-remake-token token xml:string self)]
                                               [comp (xml:attr-value*->flonum <comp>)])
                                          (cond [(flonum? comp) (collect-component rest (cons comp spmoc))]
                                                [(not comp) (make+exn:svg:range <comp>) (collect-component rest spmoc)]
                                                [else (collect-component rest spmoc)])))]
                                     [else (let ([comps (reverse spmoc)])
                                             (cond [(pair? comps) (svg-icccolor name comps)]
                                                   [(and (null? (cddr icccs)) (regexp-match? #px"\\s+" (cadr icccs)))
                                                    (make+exn:svg:missing-comma token)]
                                                   [else (make+exn:svg:malformed token)]))]))]
                            [(exn? name) name]
                            [else (make+exn:svg:malformed token)]))]
                   [(regexp-match? svg-list-separator/comma icc-body) (make+exn:svg:malformed token)]
                   [else (make+exn:svg:missing-comma token)]))]
          [else icc-body])))

(define svg-paint-filter : (-> XML-Token (Listof String) Boolean (XML-Option SVG-Paint))
  (lambda [token vs allow-inherit?]
    (cond [(null? vs) (make+exn:svg:malformed token)]
          [(null? (cdr vs))
           (let ([maybe-url (svg-function-filter token (car vs) "url" #false)])
             (cond [(string? maybe-url) maybe-url]
                   [else (svg-paint-keyword-color-filter token (car vs) #true)]))]
          [(null? (cddr vs))
           (let ([maybe-url (svg-function-filter token (car vs) "url" #false)])
             (cond [(not maybe-url) (svg-paint-profiled-color-filter token (car vs) (cadr vs))]
                   [else (let ([maybe-fallback (svg-paint-keyword-color-filter token (cadr vs) #false)])
                           (cond [(pair? maybe-fallback) (svg-paint-server maybe-url maybe-fallback)]
                                 [else maybe-fallback]))]))]
          [(null? (cdddr vs))
           (let ([maybe-url (svg-function-filter token (car vs) "url" #true)]
                 [maybe-color (svg-paint-profiled-color-filter token (cadr vs) (caddr vs))])
             (cond [(and (string? maybe-url) (pair? maybe-color)) (svg-paint-server maybe-url maybe-color)]
                   [(exn:xml? maybe-url) maybe-url]
                   [(exn:xml? maybe-color) maybe-color]
                   [else #false]))]
          [else (make+exn:svg:malformed token)])))

(define svg-paint-keyword-color-filter : (case-> [XML-Token String True -> (XML-Option (U SVG-Paint-Server-Fallback 'inherit))]
                                                 [XML-Token String False -> (XML-Option SVG-Paint-Server-Fallback)])
  (lambda [token v allow-inherit?]
    (define maybe-color (svg-color-filter token v #true))

    (cond [(or maybe-color) maybe-color]
          [(and allow-inherit? (string=? v "inherit")) 'inherit]
          [else #false])))

(define svg-paint-profiled-color-filter : (-> XML-Token String String (XML-Option (Pairof FlColor SVG-ICCColor)))
  (lambda [token cv pv]
    (define maybe-color (svg-color-filter token cv #false))
    (define maybe-profile (svg-icc-color-filter token pv))

    (cond [(and (flcolor? maybe-color) (svg-icccolor? maybe-profile)) (cons maybe-color maybe-profile)]
          [(exn:xml? maybe-color) maybe-color]
          [(exn:xml? maybe-profile) maybe-profile]
          [else #false])))

(define svg-transform-list-filter : (-> XML-Token String (XML-Option (Listof SVG-Transform)))
  (lambda [token v]
    (reverse
     (for/fold ([sfrt : (Listof SVG-Transform) null])
               ([self (in-list (svg-list-split token v))])
       (define ts : (Option (Pairof String (Listof (Option String)))) (regexp-match #px"(\\w+)\\s*[(]([^)]*)[)]" self))
       (cond [(and ts (pair? (cdr ts)) (string? (cadr ts)) (pair? (cddr ts)) (string? (caddr ts)))
              (let* ([transform (cadr ts)]
                     [args (syn-remake-token token xml:string (caddr ts))]
                     [nums (xml:attr-value*->listof-type args xml:attr-value*->flonum make+exn:svg:range svg-comma-wsp-list-split)])
                (or (and (pair? nums)
                         (cond [(string=? transform "matrix")
                                (and (pair? (cdr nums)) (pair? (cddr nums)) (pair? (cdddr nums)) (pair? (cddddr nums))
                                     (let ([tail (cddddr nums)])
                                       (and (pair? tail) (pair? (cdr tail)) (null? (cddr tail))
                                            (cons (svg:matrix (car nums) (cadr nums) (caddr nums) (cadddr nums) (car tail) (cadr tail)) sfrt))))]
                               [(string=? transform "translate")
                                (or (and (null? (cdr nums))  (cons (svg:translate (car nums) #false) sfrt))
                                    (and (null? (cddr nums)) (cons (svg:translate (car nums) (cadr nums)) sfrt)))]
                               [(string=? transform "scale")
                                (or (and (null? (cdr nums))  (cons (svg:scale (car nums) #false) sfrt))
                                    (and (null? (cddr nums)) (cons (svg:scale (car nums) (cadr nums)) sfrt)))]
                               [(string=? transform "rotate")
                                (or (and (null? (cdr nums))  (cons (svg:rotate (car nums) #false #false) sfrt))
                                    (and (pair? (cddr nums)) (null? (cdddr nums)) (cons (svg:rotate (car nums) (cadr nums) (caddr nums)) sfrt)))]
                               [(string=? transform "skewX") (and (null? (cdr nums)) (cons (svg:skewX (car nums)) sfrt))]
                               [(string=? transform "skewY") (and (null? (cdr nums)) (cons (svg:skewY (car nums)) sfrt))]
                               [else (make+exn:svg:function token) sfrt]))
                    (and (make+exn:svg:malformed token) sfrt)))]
             [else (make+exn:svg:malformed token) sfrt])))))

(define svg-IRI-filter : (-> XML-Token String (XML-Option String))
  (lambda [token v]
    (define url (svg-function-filter token v "url" #true))
    (cond [(string? url) url]
          [(not url) v]
          [else url])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define svg-list-split : (->* (XML-Token String) (Byte Byte) (Listof String))
  (let ()
    (define (greedy-lookahead [raw : String] [pos : Nonnegative-Fixnum] [size : Index]) : (Values Nonnegative-Fixnum Boolean)
      (let body-lookahead ([idx : Nonnegative-Fixnum pos]
                           [bracket : Natural 1])
        (cond [(>= idx size) (values size #false)]
              [else (let ([bch (string-ref raw idx)]
                          [idx++ (+ idx 1)])
                      (cond [(eq? bch #\)) (if (<= bracket 1) (values idx++ #true) (body-lookahead idx++ (- bracket 1)))]
                            [(eq? bch #\() (body-lookahead idx++ (+ bracket 1))]
                            [else (body-lookahead idx++ bracket)]))])))
    (lambda [token raw [comma/min 0] [comma/max 1]]
      (define size (string-length raw))
      (let split ([start : Nonnegative-Fixnum 0]
                  [end : Nonnegative-Fixnum 0]
                  [idx : Nonnegative-Fixnum 0]
                  [sgnirts : (Listof String) null]
                  [comma : Natural 0])
        (if (>= idx size)
            (let ()
              (when (> comma 0) (make+exn:svg:malformed token))
              (cond [(>= start end) (reverse sgnirts)]
                    [else (reverse (cons (substring raw start end) sgnirts))]))
            
            (let ([ch (string-ref raw idx)]
                  [idx++ (+ idx 1)])
              (cond [(or (char-blank? ch) (eq? ch #\,))
                     (let ([comma++ (if (eq? ch #\,) (+ comma 1) comma)])
                       (if (>= start end)
                           (let ()
                             (when (and (eq? ch #\,) (null? sgnirts))
                               (make+exn:svg:malformed token))
                             (split idx++ idx++ idx++ sgnirts comma++))
                         
                         (let lookahead ([aidx : Nonnegative-Fixnum idx++]
                                         [comma : Index (assert comma++ index?)])
                           (if (>= aidx size)
                               (split start end size sgnirts comma)
                               
                               (let ([ach (string-ref raw aidx)]
                                     [aidx++ (+ aidx 1)])
                                 (cond [(char-blank? ach) (lookahead aidx++ comma)]
                                       [(eq? ach #\,)
                                        (cond [(>= comma comma/max) (make+exn:svg:malformed token) (lookahead aidx++ comma)]
                                              [else (lookahead aidx++ (+ comma 1))])]
                                       [(not (eq? ach #\())
                                        (when (< comma comma/min) (make+exn:svg:missing-comma token))
                                        (split aidx aidx++ aidx++ (cons (substring raw start end) sgnirts) 0)]
                                       [(> comma 0)
                                        (when (< comma comma/min) (make+exn:svg:missing-comma token))
                                        (split aidx aidx aidx (cons (substring raw start end) sgnirts) 0)]
                                       [else (let-values ([(bidx++ okay?) (greedy-lookahead raw aidx++ size)])
                                               (cond [(not okay?) (make+exn:svg:malformed token) (split start size size sgnirts 0)]
                                                     [else (split bidx++ bidx++ bidx++ (cons (substring raw start bidx++) sgnirts) 0)]))]))))))]
                    [(not (eq? ch #\()) (split (if (>= start end) idx start) idx++ idx++ sgnirts 0)]
                    [else (let-values ([(bidx++ okay?) (greedy-lookahead raw idx++ size)])
                            (cond [(not okay?) (make+exn:svg:malformed token) (split start size size sgnirts 0)]
                                  [else (split bidx++ bidx++ bidx++ (cons (substring raw start bidx++) sgnirts) 0)]))])))))))

(define svg-wsp-list-split : (-> XML-Token String (Listof String))
  (lambda [token raw]
    (svg-list-split token raw 0 0)))

(define svg-comma-wsp-list-split : (-> XML-Token String (Listof String))
  (lambda [token raw]
    (svg-list-split token raw 1 1)))
