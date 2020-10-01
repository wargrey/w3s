#lang typed/racket/base

(provide (all-defined-out))

(require sgml/digitama/doctype)
(require sgml/digitama/grammar)
(require sgml/digitama/document)
(require sgml/digitama/digicore)

(require css/digitama/syntax/w3s)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type XML-Name (W3S-Token-Datumof Symbol))
(define-type XML-String (W3S-Token-Datumof String))
(define-type XML-Char (W3S-Token-Datumof Index))
(define-type XML-Reference (W3S-Token-Datumof Symbol))

(define-type XML-PI-Datum (MPairof XML-Name (Option XML-String)))
(define-type XML-Element-Attribute-Value-Datum (U XML-String XML-Name (Listof XML-Name)))
(define-type XML-Element-Attribute-Datum (Pairof XML-Name XML-Element-Attribute-Value-Datum))
(define-type XML-Subdatum-Datum (U XML-String XML-Char XML-Reference XML-PI-Datum))
(define-type XML-Element-Datum (Rec elem (List XML-Name (Listof XML-Element-Attribute-Datum) (Listof (U elem XML-Subdatum-Datum)))))

(define-type XML-Content-Datum (U XML-PI-Datum XML-Element-Datum))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-document->location+datum : (-> XML-Document* (Values (U String Symbol) (Option Nonnegative-Flonum) (Option String) Boolean Any))
  (lambda [doc.xml]
    (define prolog (xml-document*-prolog doc.xml))

    (values (xml-prolog-location prolog)
            (xml-prolog-version prolog) (xml-prolog-encoding prolog)
            (xml-prolog-standalone? prolog)

            (map xml-content->location+datum (xml-document*-contents doc.xml)))))

(define xml-location+datum->document : (-> (U String Symbol) (Option Nonnegative-Flonum) (Option String) Boolean (Listof XML-Content-Datum) XML-Document*)
  (lambda [location version encoding standalone? contents]
    (make-xml-document* location version encoding standalone?
                        #false #false #false #false #false
                        (map xml-location+datum->content contents))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-content->location+datum : (-> XML-Content* XML-Content-Datum)
  (lambda [c]
    (cond [(mpair? c) (xml-pi->location+datum c)]
          [else (xml-element->location+datum c)])))

(define xml-pi->location+datum : (-> XML-Processing-Instruction* XML-PI-Datum)
  (lambda [pi]
    (define pi-name (mcar pi))
    (define pi-body (mcdr pi))
    
    (mcons (w3s-token->location+datum pi-name (xml:name-datum pi-name))
           (and pi-body
                (w3s-token->location+datum pi-body (xml:string-datum pi-body))))))

(define xml-element->location+datum : (-> XML-Element* XML-Element-Datum)
  (lambda [e]
    (define tagname (car e))
    
    (list (w3s-token->location+datum tagname (xml:name-datum tagname))
          (map xml-attribute->location+datum (cadr e))
          (map xml-subdatum->location+datum (caddr e)))))

(define xml-attribute->location+datum : (-> XML-Element-Attribute* XML-Element-Attribute-Datum)
  (lambda [a]
    (define attr-name (car a))
    (define attr-value (cdr a))
   
    (cons (w3s-token->location+datum attr-name (xml:name-datum attr-name))
          (cond [(xml:string? attr-value) (w3s-token->location+datum attr-value (xml:string-datum attr-value))]
                [(xml:name? attr-value) (w3s-token->location+datum attr-value (xml:name-datum attr-value))]
                [else (for/list : (Listof XML-Name) ([name (in-list attr-value)])
                        (w3s-token->location+datum name (xml:name-datum name)))]))))

(define xml-subdatum->location+datum : (-> (U XML-Content* XML-Subdatum*) (U XML-Content-Datum XML-Subdatum-Datum))
  (lambda [c]
    (cond [(list? c) (xml-element->location+datum c)]
          [(xml-cdata-token? c) (w3s-token->location+datum c (assert (xml-cdata-token->datum c)))]
          [(xml:char? c) (w3s-token->location+datum c (xml:char-datum c))]
          [(xml:reference? c) (w3s-token->location+datum c (xml:reference-datum c))]
          [(mpair? c) (xml-pi->location+datum c)]
          [else (w3s-token->location+datum c "DEADCODE")])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-location+datum->content : (-> XML-Content-Datum XML-Content*)
  (lambda [c]
    (cond [(mpair? c) (xml-location+datum->pi c)]
          [else (xml-location+datum->element c)])))

(define xml-location+datum->pi : (-> XML-PI-Datum XML-Processing-Instruction*)
  (lambda [pi]
    (define pi-name (mcar pi))
    (define pi-body (mcdr pi))
    
    (mcons (w3s-location+datum->token xml:name pi-name)
           (and pi-body
                (xml-datum->string-token pi-body)))))

(define xml-location+datum->element : (-> XML-Element-Datum XML-Element*)
  (lambda [e]
    (define tagname (car e))
    
    (list (w3s-location+datum->token xml:name tagname)
          (map xml-location+datum->attribute (cadr e))
          (filter-map xml-location+datum->subdatum (caddr e)))))

(define xml-location+datum->attribute : (-> XML-Element-Attribute-Datum XML-Element-Attribute*)
  (lambda [a]
    (define attr-name (car a))
    (define attr-value (cdr a))

    (cons (w3s-location+datum->token xml:name attr-name)
          (if (list? attr-value)
              (for/list : (Listof XML:Name) ([name (in-list attr-value)])
                (w3s-location+datum->token xml:name name))
              (let ([payload (w3s-datum-payload attr-value)])
                (cond [(symbol? payload) (w3s-location+datum->token xml:name attr-value payload)]
                      [else (or (xml-datum->string-token* attr-value)
                                (w3s-location+datum->token xml:string attr-value "DEADCODE"))]))))))

(define xml-location+datum->subdatum : (-> (U XML-Content-Datum XML-Subdatum-Datum) (U XML-Content* XML-Subdatum*))
  (lambda [c]
    (cond [(list? c) (xml-location+datum->element c)]
          [(mpair? c) (xml-location+datum->pi c)]
          [else (or (xml-datum->whitespace-token* c)
                    (xml-datum->string-token* c)
                    (w3s-location+datum->token* xml:char c index?)
                    (w3s-location+datum->token* xml:reference c symbol?)
                    (w3s-location+datum->token xml:string c "DEADCODE"))])))

(define xml-datum->string-token : (-> XML-String XML:String)
  (lambda [s]
    (or (w3s-location+datum->token* xml:&string s)
        (w3s-location+datum->token xml:string s))))

(define xml-datum->string-token* : (-> (W3S-Token-Datumof Any) (Option XML:String))
  (lambda [s]
    (or (w3s-location+datum->token* xml:&string s string?)
        (w3s-location+datum->token* xml:string s string?))))

(define xml-datum->whitespace-token* : (-> (W3S-Token-Datumof Any) (Option XML:WhiteSpace))
  (lambda [s]
    (or (w3s-location+datum->token* xml:newline s string?)
        (w3s-location+datum->token* xml:comment s string?)
        (w3s-location+datum->token* xml:whitespace s string?))))
