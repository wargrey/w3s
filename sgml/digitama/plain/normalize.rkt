#lang typed/racket/base

(provide (all-defined-out))

(require racket/list)

(require "grammar.rkt")
(require "whitespace.rkt")

(require "../misc.rkt")
(require "../tokenizer/port.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-normalize : (-> (Listof XML-Content) String Symbol (Option XML:Space-Filter) (Listof XML-Content))
  (lambda [content xml:lang xml:space xml:space-filter]
    (parameterize ([default-xml:space-signal xml:space])
      (let xml-content-normalize ([rest : (Listof XML-Content) content]
                                  [clear-content : (Listof XML-Content) null])
        (cond [(null? rest) (reverse clear-content)]
              [else (let-values ([(self rest++) (values (car rest) (cdr rest))])
                      (cond [(list? self)
                             (let ([elem (xml-element-normalize self (xml:lang-ref xml:lang) xml:space xml:space-filter 0)])
                               (xml-content-normalize rest++ (cons elem clear-content)))]
                            [else (xml-content-normalize rest++ (cons self clear-content))]))])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-element-normalize : (-> XML-Element (Option String) Symbol (Option XML:Space-Filter) Index XML-Element)
  (lambda [e xml:lang xml:space xml:filter depth]
    (define tagname : Symbol (car e))
    
    (define-values (xml:this:lang xml:this:space)
      (let attribute-filter-map : (Values (Option String) Symbol)
        ([rest : (Listof XML-Element-Attribute) (cadr e)]
         [lang : (Option String) xml:lang]
         [space : Symbol xml:space])
        (if (pair? rest)
            (let-values ([(self rest++) (values (car rest) (cdr rest))])
              (case (car self)
                [(xml:lang) (attribute-filter-map rest++ (xml:lang-ref tagname (cdr self)) space)]
                [(xml:space) (attribute-filter-map rest++ lang (xml:space-ref (cdr self) space))]
                [else (attribute-filter-map rest++ lang space)]))
            
            (values lang space))))

    (define ?children : (Listof (U XML-Subdatum XML-Element))
      (xml-subelement-normalize tagname (caddr e)
                                xml:this:lang xml:this:space xml:filter
                                (assert (+ depth 1) index?)))
    
    (list tagname (cadr e) ?children)))

(define xml-subelement-normalize : (-> Symbol (Listof (U XML-Subdatum XML-Element)) (Option String) Symbol (Option XML:Space-Filter) Index
                                       (Listof (U XML-Subdatum XML-Element)))
  (lambda [tagname children xml:lang xml:space xml:filter depth]
    (let normalize-subelement ([rest : (Listof (U XML-Subdatum XML-Element)) children]
                               [nerdlihc : (Listof (U XML-Subdatum XML-Element)) null]
                               [secaps : (Listof XML-White-Space) null])
      (if (pair? rest)
          (let-values ([(self rest++) (values (car rest) (cdr rest))])
            (cond [(list? self)
                   (let ([?elem (xml-element-normalize self xml:lang xml:space xml:filter depth)])
                     (cond [(list? ?elem) (normalize-subelement rest++ (cons ?elem (xml-child-cons secaps nerdlihc xml:filter tagname xml:lang)) null)]
                           [else ?elem]))]

                  [(symbol? self) ; entity reference, ignored
                   (normalize-subelement rest++ nerdlihc null)]

                  [(index? self)
                   (normalize-subelement rest++
                                         (cons (string (integer->char self))
                                               (xml-child-cons secaps nerdlihc xml:filter tagname xml:lang))
                                         null)]

                  [(not (xml-white-space? self)) (normalize-subelement rest++ (cons self (xml-child-cons secaps nerdlihc xml:filter tagname xml:lang)) null)]
                  [(xml-comment? self) (normalize-subelement rest++ nerdlihc secaps)]
                  [(eq? xml:space 'preserve) (normalize-subelement rest++ (cons (xml:space=preserve tagname self xml:filter xml:lang) nerdlihc) secaps)]
                  [else (normalize-subelement rest++ nerdlihc (cons self secaps))]))

          (let cdata-reverse ([rest : (Listof (U XML-Subdatum XML-Element)) (xml-child-cons secaps nerdlihc xml:filter tagname xml:lang #true)]
                              [children : (Listof (U XML-Subdatum XML-Element)) null]
                              [cdatas : (Listof (U String XML-White-Space)) null])
            (cond [(null? rest) (xml-cdata-cons cdatas children)]
                  [else (let-values ([(self rest++) (values (car rest) (cdr rest))])
                          (cond [(or (string? self) (xml-white-space? self)) (cdata-reverse rest++ children (cons self cdatas))]
                                [else (cdata-reverse rest++ (cons self (xml-cdata-cons cdatas children)) null)]))]))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
(define xml-cdata-cons : (-> (Listof (U String XML-White-Space)) (Listof (U XML-Subdatum XML-Element)) (Listof (U XML-Subdatum XML-Element)))
  (lambda [cdatas children]
    (cond [(null? cdatas) children]
          [(null? (cdr cdatas)) (cons (car cdatas) children)]
          [else (let ([cdata (apply string-append (filter-map xml-cdata->datum cdatas))])
                  (cons cdata children))])))
