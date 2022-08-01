#lang typed/racket/base

(provide (all-defined-out) svg11*.dtd)

(require racket/string)

(require sgml/digitama/schema)
(require sgml/digitama/digicore)
(require sgml/digitama/dtd)

(require "../../village/svglang/svg11.tdtd")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define attrlists : (HashTable Symbol (Listof Keyword)) (make-hasheq))
(define attribs : (HashTable Keyword (Listof Symbol)) (make-hasheq))

(define registered-groups : (Listof Keyword)
  '(#:SVG.FilterPrimitiveWithIn.attrib #:SVG.FilterPrimitive.attrib
    #:SVG.Presentation.attrib #:SVG.Conditional.attrib #:SVG.External.attrib
    #:SVG.AnimationAttribute.attrib #:SVG.AnimationAddition.attrib
    #:SVG.GraphicalEvents.attrib #:SVG.AnimationEvents.attrib
    #:SVG.XLink.attrib))

(define hidden-groups : (Listof Keyword)
  '(#:SVG.id.attrib #:SVG.lang.attrib #:SVG.base.attrib #:SVG.Core.attrib
    #:SVG.XLinkRequired.attrib #:SVG.XLinkEmbed.attrib #:SVG.XLinkReplace.attrib
    #:SVG.Animation.attrib))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (dtd-prepare-attlist!) : Void
  (for ([(e-name entity) (in-hash (xml-schema-entities svg11*.dtd))])
    (when (and (keyword? e-name) (xsch-token-entity? entity) (string-suffix? (keyword->string e-name) ".attrib"))
      (let collect-attlist : Void ([body : (Listof XML-Token) (or (xsch-token-entity-body entity) null)])
        (when (and (pair? body) (xml:name? (car body)))
          (let*-values ([(atype rest) (xml-dtd-extract-attribute-type* (car body) (cdr body))]
                        [(avalue fixed? rest) (xml-dtd-extract-attribute-default* (car body) rest)])
            (define a-name (xml:name-datum (car body)))
            (hash-set! attrlists a-name (cons e-name (hash-ref attrlists a-name (inst list Keyword))))
            (hash-set! attribs e-name (cons a-name (hash-ref attribs e-name (inst list Symbol))))
            (collect-attlist rest)))))))

(define (dtd-list-all-elements) : (Listof Symbol)
  (sort (hash-keys (xml-schema-elements svg11*.dtd)) symbol<?))

(define (dtd-attribute-list [tag-name : Symbol]) : (Listof Symbol)
  (hash-keys
   (hash-ref (xml-schema-attributes svg11*.dtd) tag-name
             (inst make-hasheq Symbol XSch-Attribute))))

(define (dtd-element-list [attr-name : Symbol]) : (Listof Symbol)
  (for/list ([(e as) (xml-schema-attributes svg11*.dtd)]
             #:when (hash-has-key? as attr-name))
    e))

(define (dtd-children [tag-name : Symbol]) : (U False Keyword (Listof Symbol) (Pairof Schema-Element-Children Char))
  (define e (hash-ref (xml-schema-elements svg11*.dtd) tag-name (λ [] #false)))

  (cond [(xsch-mixed-element? e) (xsch-mixed-element-children e)]
        [(xsch-element+children? e) (xsch-element+children-content e)]
        [(xsch-element? e) '#:empty]
        [else #false]))

(define (dtd-attr-group-displayln [attrs : (Listof Symbol)] [indent : String ""]) : Void
  (define-values (groups extra)
    (for/fold ([groups : (Listof Keyword) null] [extra : (Listof Symbol) null])
              ([attr (in-list attrs)])
      (define group (hash-ref attrlists attr (λ [] #false)))
      (cond [(not group) (values groups (cons attr extra))]
            [else (values (append group groups) extra)])))
  
  (for ([group (in-list (remove-duplicates groups))])
    (define attrs (sort (hash-ref attribs group (inst list Symbol)) symbol<?))

    (unless (memq group hidden-groups)
      (printf "~a~a[~a]:~n" indent group (length attrs))
      (unless (memq group registered-groups)
        (for ([attr (in-list (sort attrs symbol<?))])
          (printf "~a    [~a : (Option String) #:=> xml-attribute-value->string #false]~n" indent attr)))))

  (when (pair? extra)
    (printf "~aREST[~a]:~n" indent (length extra))
    (for ([attr (in-list (sort extra symbol<?))])
      (printf "~a    [~a : (Option String) #:=> xml-attribute-value->string #false]~n" indent attr))))

(define (dtd-info-displayln [argv : (Listof Symbol)] [type : Symbol] [svg-list : (-> Symbol (Listof Symbol))]) : Void
    (define lists : (Listof (Pairof (Listof Symbol) Symbol))
      (for/list ([tag (in-list argv)])
        (cons (svg-list tag) tag)))

    (printf "=================== ~a ===================" type)
    
    (for ([ls (in-list lists)])
      (when (pair? (car ls))
        (printf "~n~a[~a, ~a]: ~a~n"
                (cdr ls) type (length (car ls))
                (sort (car ls) symbol<?))))

    (let ([dict : (HashTable Symbol Natural) (make-hasheq)]
          [n (length argv)])
      (for ([ls (in-list lists)])
        (for ([t (in-list (car ls))])
          (hash-set! dict t (+ (hash-ref dict t (λ [] 0)) 1))))

      (when (> n 1)       
        (define-values (e-share e-diff)
          (for/fold ([cs : (Listof Symbol) null] [ds : (Listof Symbol) null])
                    ([(t c) (in-hash dict)])
            (if (= c n)
                (values (cons t cs) ds)
                (values cs (cons t ds)))))
        (printf "~n+[~a]: ~a~n" (length e-share) (sort e-share symbol<?))
        (printf "-[~a]: ~a~n" (length e-diff) (sort e-diff symbol<?)))))
