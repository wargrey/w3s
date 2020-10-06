#lang typed/racket/base

(provide (all-defined-out))

(require "dtd.rkt")
(require "digicore.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-type->racket-types : (-> XML-Type (Listof Any))
  (lambda [dtd]
    (define attlists : DTD-Attributes (xml-type-attributes dtd))
    
    (append
     (for/list : (Listof Any) ([(elem attrs) (in-hash (xml-type-attributes dtd))])
              (define ELEMENT (xml-element-type-name elem))
              (define ATTLIST (xml-attlist-type-name elem))
              
              `(define-type ,ATTLIST
                 (Listof (U ,@(for/list : (Listof Any) ([(name attr) (in-hash attrs)])
                                (define atype (dtd-attribute-type attr))
                                
                                `(Pairof ',name
                                         ,(cond [(dtd-attribute-token-type? atype)
                                                 (if (dtd-attribute-token-type-names? atype) '(Listof Symbol) 'Symbol)]
                                                [(dtd-attribute-enum-type? atype)
                                                 (xml-options->type (dtd-attribute-enum-type-options atype))]
                                                [else 'String])))))))

     (for/list : (Listof Any) ([(name elem) (in-hash (xml-type-elements dtd))])
       (define ELEMENT (xml-element-type-name name))
       (define ATTLIST (xml-attlist-type-name name attlists))
       
       (cond [(dtd-empty-element? elem)
              `(define-type ,ELEMENT (List ',name ,ATTLIST Null))]
             [(dtd-mixed-element? elem)
              (let ([children (dtd-mixed-element-children elem)])
                (cond [(null? children) `(define-type ,ELEMENT (List ',name ,ATTLIST (Listof String)))]
                      [else `(define-type ,ELEMENT (List ',name ,ATTLIST (Listof (U String ,@(xml-elements->type children)))))]))]
             [(dtd-element+children? elem)
              (let ([subnames (reverse (xml-subelements-flatten (car (dtd-element+children-content elem))))])
                `(define-type ,ELEMENT (List ',name ,ATTLIST (Listof (U ,@(xml-elements->type subnames))))))]
             [else `(define-type ,ELEMENT (List ',name ,ATTLIST (Listof Any)))])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-options->type : (-> (Listof XML:Name) (Pairof 'U (Listof Any)))
  (lambda [options]
    `(U ,@(map (λ [[opt : XML:Name]] `',(xml:name-datum opt))
               options))))

(define xml-elements->type : (-> (Listof XML:Name) (Listof Any))
  (lambda [subelems]
    (map (λ [[opt : XML:Name]] (xml-element-type-name (xml:name-datum opt)))
         subelems)))

(define xml-subelements-flatten : (-> DTD-Element-Children (Listof XML:Name))
  (lambda [subelems]
    (for/fold ([subnames : (Listof XML:Name) null])
              ([sub.particle (if (vector? subelems) (in-vector subelems) (in-list subelems))])
      (define sub (car sub.particle))
      (cond [(xml:name? sub) (values (cons sub subnames))]
            [else (append (xml-subelements-flatten sub) subnames)]))))

(define xml-element-type-name : (-> Symbol Symbol)
  (lambda [elem]
    (string->symbol (string-append "Element:"
                                   (string-titlecase (symbol->string elem))))))

(define xml-attlist-type-name : (case-> [Symbol -> Symbol]
                                        [Symbol DTD-Attributes -> Symbol])
  (case-lambda
    [(elem)
     (string->symbol (string-append "AttList:"
                                    (string-titlecase (symbol->string elem))))]
    [(elem attlists)
     (cond [(hash-has-key? attlists elem) (xml-attlist-type-name elem)]
           [else 'XML-Element-Attribute])]))
