#lang typed/racket/base

;;; https://drafts.csswg.org/selectors

(provide (all-defined-out))

(require racket/set)
(require racket/string)

(require "digicore.rkt")
(require "misc.rkt")

(require (for-syntax racket/base))

;; https://drafts.csswg.org/selectors/#grammar
;; https://drafts.csswg.org/selectors/#structure
;; https://drafts.csswg.org/selectors/#data-model
(define-syntax (define-selectors stx)
  (syntax-case stx []
    [(_ [id : S-ID rest ...] ...)
     #'(begin (struct id rest ... #:transparent #:type-name S-ID) ...)]))

(define-syntax (define-exclusive-:classes stx)
  (syntax-case stx []
    [(_ id [v1 v2] ...) ; TODO: multiple exclusives
     #'(define id : (HashTable Symbol Symbol)
         (make-hasheq (list (cons 'v1 'v2) ...
                            (cons 'v2 'v1) ...)))]))

(define-type CSS-Selector-Combinator (U '>> '> '+ '~ '||))
(define-type CSS-Namespace-Hint (U (Listof (Pairof Symbol String)) False))
(define-type CSS-Complex-Selector (Listof+ CSS-Compound-Selector))
(define-type CSS-Attribute-Value (U (U String Symbol (Listof (U String Symbol)))
                                    (Vector Symbol (U String Symbol (Listof (U String Symbol))))))

(define-preference css-subject
  ([combinator : CSS-Selector-Combinator                #:= '>]
   [type : Symbol                                       #:= (css-root-element-type)]
   [id : (U Keyword (Listof+ Keyword))                  #:= (css-root-element-id)]
   [namespace : (U Symbol Boolean)                      #:= #true]
   [classes : (Listof Symbol)                           #:= null]
   [attributes : (HashTable Symbol CSS-Attribute-Value) #:= (make-hasheq)]
   [:classes : (Listof Symbol)                          #:= null]
   [lang : (U Symbol String)                            #:= ""])
  #:transparent
  #:type-name CSS-Subject)

(define-selectors
  [css-attribute-selector : CSS-Attribute-Selector ([name : Symbol] [quirk : Symbol] [namespace : (U Symbol Boolean)])]
  [css-attribute~selector : CSS-Attribute~Selector css-attribute-selector ([type : Char] [value : (U Symbol String)] [i? : Boolean])]  

  [css-:class-selector : CSS-:Class-Selector ([name : Symbol] [arguments : (Option (Listof Any))])]
  [css-::element-selector : CSS-::Element-Selector ([name : Symbol] [arguments : (Option (Listof Any))] [:classes : (Listof CSS-:Class-Selector)])]

  [css-compound-selector : CSS-Compound-Selector
                         ([combinator : (Option CSS-Selector-Combinator)]
                          [namespace : (U Symbol Boolean)]
                          [type : (U Symbol True)]
                          [ids : (Listof Keyword)]
                          [classes : (Listof Symbol)]
                          [attributes : (Listof CSS-Attribute-Selector)]
                          [:classes : (Listof CSS-:Class-Selector)]
                          [::element : (Option CSS-::Element-Selector)])])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define css-selector-match : (-> CSS-Complex-Selector (Listof+ CSS-Subject) (Option Natural))
  ;;; https://drafts.csswg.org/selectors/#evaluating-selectors
  ;;; https://github.com/w3c/csswg-drafts/issues/720
  (lambda [srotceles stnemele]
    ; TODO: define a better object model
    (define root : Symbol (css-root-element-type))
    (let evaluate ([selector : CSS-Compound-Selector (car srotceles)]
                   [element : CSS-Subject (car stnemele)]
                   [srotceles : (Listof CSS-Compound-Selector) (cdr srotceles)]
                   [stcejbus : (Listof CSS-Subject) (cdr stnemele)]
                   [specificity : Natural 0])
      (define root? : Boolean (eq? (css-subject-type element) root))
      (define specificity++ : (Option Natural) (css-compound-selector-match selector element root?))
      (and specificity++
           (cond [(null? stcejbus) (and (null? srotceles) (fx+ specificity specificity++))]
                 [(null? srotceles) (fx+ specificity specificity++)]
                 [else (evaluate (car srotceles) (car stcejbus) (cdr srotceles) (cdr stcejbus)
                                 (fx+ specificity specificity++))])))))

(define css-compound-selector-match : (-> CSS-Compound-Selector CSS-Subject Boolean (Option Natural))
  ;;; https://drafts.csswg.org/selectors/#subject-of-a-selector
  ;;; https://drafts.csswg.org/selectors/#case-sensitive
  (lambda [selector element root?]
    (and (css-combinator-match? (css-compound-selector-combinator selector) (css-subject-combinator element))
         (css-namespace-match? (css-compound-selector-namespace selector) (css-subject-namespace element))
         (let ([s:type : (U Symbol True) (css-compound-selector-type selector)])
           (or (eq? s:type #true) (eq? s:type (css-subject-type element))))
         (let ([s:ids : (Listof Keyword) (css-compound-selector-ids selector)]
               [id : (U Keyword (Listof+ Keyword)) (css-subject-id element)])
           (cond [(null? s:ids) #true]
                 [(keyword? id) (and (null? (cdr s:ids)) (eq? (car s:ids) id))]
                 [else (and (list? s:ids) (set=? (list->set s:ids) (list->set id)))]))
         (let ([s:classes : (Listof Symbol) (css-compound-selector-classes selector)]
               [classes : (Listof Symbol) (css-subject-classes element)])
           (for/and : Boolean ([s:c (in-list s:classes)]) (and (memq s:c classes) #true)))
         (let ([s:attrs : (Listof CSS-Attribute-Selector) (css-compound-selector-attributes selector)]
               [attrs : (HashTable Symbol CSS-Attribute-Value) (css-subject-attributes element)])
           (css-attribute-match? s:attrs attrs))
         (let ([s:classes : (Listof CSS-:Class-Selector) (css-compound-selector-:classes selector)]
               [:classes : (Listof Symbol) (css-subject-:classes element)])
           (or (null? s:classes)
               (and root? (css-:classes-match? s:classes (cons 'root :classes)))
               (and (pair? :classes) (css-:classes-match? s:classes :classes))))
         (let-values ([(a b c) (css-compound-selector-abc selector)])
           ((default-css-abc->specificity) a b c)))))

(define css-compound-selector-abc : (-> CSS-Compound-Selector (values Natural Natural Natural))
  ;;; https://drafts.csswg.org/selectors/#specificity-rules
  (lambda [static-unit]
    (values (length (css-compound-selector-ids static-unit))
            (fx+ (length (css-compound-selector-classes static-unit))
                 (fx+ (length (css-compound-selector-:classes static-unit))
                      (length (css-compound-selector-attributes static-unit))))
            (fx+ (if (css-compound-selector-::element static-unit) 1 0)
                 (if (symbol? (css-compound-selector-type static-unit)) 1 0)))))

(define css-declared-namespace : (-> CSS-Namespace-Hint (U CSS:Ident CSS:Delim Symbol) (U Symbol Boolean))
  (lambda [namespaces namespace]
    (or (css:delim? namespace)        ; *
        (let ([ns (if (css:ident? namespace) (css:ident-datum namespace) namespace)])
          (if (or (false? namespaces) ; application does not care namespaces
                  (and (list? namespaces) (assq ns namespaces) #true))
              ns #false)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-exclusive-:classes css-exclusive-:classes
  [link visited]
  [playing paused]
  [enable disable]
  [read-only read-write]
  [valid invalid]
  [in-range out-of-range]
  [required optional])

(define default-css-abc->specificity : (Parameterof (-> Natural Natural Natural Natural))
  (make-parameter (λ [[A : Natural] [B : Natural] [C : Natural]] : Natural
                    (fxior (fxlshift A 16) (fxior (fxlshift B 8) C)))))

(define css-complex-selector-abc : (-> (Listof+ CSS-Compound-Selector) (Values Natural Natural Natural))
  ;;; https://drafts.csswg.org/selectors/#specificity-rules
  (lambda [selectors]
    (for/fold ([A : Natural 0] [B : Natural 0] [C : Natural 0])
              ([selector (in-list selectors)])
      (define-values (a b c) (css-compound-selector-abc selector))
      (values (fx+ A a) (fx+ B b) (fx+ C c)))))

(define css-complex-selector-specificity : (-> CSS-Complex-Selector Natural)
  (lambda [selectors]
    (define-values (A B C) (css-complex-selector-abc selectors))
    ((default-css-abc->specificity) A B C)))

(define css-combinator-match? : (-> (Option CSS-Selector-Combinator) CSS-Selector-Combinator Boolean)
  (lambda [s:combinator combinator]
    (case s:combinator
      [(> +) (eq? s:combinator combinator)]
      [(>>) (or (eq? combinator '>>) (eq? combinator '>))] ; the children are also descendents
      [(~) (or (eq? combinator '~) (eq? combinator '+))]   ; the next sibling is also a following sibling.
      [else (or (eq? combinator '>>) (eq? combinator '>))])))

(define css-namespace-match? : (-> (U Symbol Boolean) (U Symbol Boolean) Boolean)
  (lambda [src ns]
    (cond [(eq? src #true) #true]
          [(false? src) (false? ns)]
          [else (eq? src ns)])))

(define css-attribute-match? : (-> (Listof CSS-Attribute-Selector) (HashTable Symbol CSS-Attribute-Value) Boolean)
  (lambda [s:attrs attrs]
    (for/and : Boolean ([attr : CSS-Attribute-Selector (in-list s:attrs)])
      (and (hash-has-key? attrs (css-attribute-selector-name attr))
           (let*-values ([(ns.val) (hash-ref attrs (css-attribute-selector-name attr))]
                         [(ns datum) (cond [(not (vector? ns.val)) (values #false ns.val)]
                                           [else (values (vector-ref ns.val 0) (vector-ref ns.val 1))])])
             (and (css-namespace-match? (css-attribute-selector-namespace attr) ns)
                  (or (not (css-attribute~selector? attr)) ; [attr]
                      (let* ([px:val : String (regexp-quote (~a (css-attribute~selector-value attr)))]
                             [mode : String (if (css-attribute~selector-i? attr) "i" "-i")]
                             [val : String (if (list? datum) (string-join ((inst map String Any) ~a datum)) (~a datum))])
                        (and (non-empty-string? px:val)
                             (case (css-attribute~selector-type attr)
                               [(#\=) (regexp-match? (pregexp (format "(?~a:^~a$)" mode px:val)) val)]
                               [(#\~) (regexp-match? (pregexp (format "(?~a:\\b~a\\b)" mode px:val)) val)]
                               [(#\|) (regexp-match? (pregexp (format "(?~a:^~a(-|$))" mode px:val)) val)]
                               [(#\^) (regexp-match? (pregexp (format "(?~a:^~a)" mode px:val)) val)]
                               [(#\$) (regexp-match? (pregexp (format "(?~a:~a$)" mode px:val)) val)]
                               [(#\*) (regexp-match? (pregexp (format "(?~a:~a)" mode px:val)) val)]
                               [else #false]))))))))))

(define css-:classes-match? : (-> (Listof+ CSS-:Class-Selector) (Listof+ Symbol) Boolean)
  (lambda [s:classes :classes]
    (define excluded : (Listof Symbol) (filter-map (λ [[:c : Symbol]] (hash-ref css-exclusive-:classes :c (λ _ #false))) :classes))
    (and (or (null? excluded)
             (for/and : Boolean ([s:c (in-list s:classes)])
               (not (memq (css-:class-selector-name s:c) excluded))))
         (for/or : Any ([s:c (in-list s:classes)])
           ; TODO: deal with functional (especially dynamical) pseudo classes
           (memq (css-:class-selector-name s:c) :classes))
         #true)))
