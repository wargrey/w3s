#lang typed/racket/base

(provide (all-defined-out))

(require "port.rkt")
(require "../delimiter.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type XML-Grammar (U XML-Declaration XML-Processing-Instruction XML-Element))

(define-type XML-Processing-Instruction (Boxof (Pairof Symbol String)))
(define-type XML-Declaration (Rec body (Vector Symbol (Listof (U XML-Datum body XML-Processing-Instruction)))))
(define-type XML-Element-Attribute (Pairof Symbol String))
(define-type XML-Element-Plain-Children (U String XML-Processing-Instruction XML-White-Space Index Symbol))
(define-type XML-Element (Rec elem (List Symbol (Listof XML-Element-Attribute) (Listof (U elem XML-Element-Plain-Children)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-syntax->grammar : (-> (Listof XML-Datum) (Listof XML-Grammar))
  (lambda [tokens]
    (let syntax->grammar ([rest : (Listof XML-Datum) tokens]
                          [srammarg : (Listof XML-Grammar) null])
      (cond [(null? rest) (reverse srammarg)]
            [else (let-values ([(self rest++) (values (car rest) (cdr rest))])
                    (cond [(eq? self <!)
                           (let-values ([(d r) (xml-syntax-extract-declaration rest++)])
                             (syntax->grammar r (if (not d) srammarg (cons d srammarg))))]
                          [(eq? self <?)
                           (let-values ([(p r) (xml-syntax-extract-pi rest++)])
                             (syntax->grammar r (if (not p) srammarg (cons p srammarg))))]
                          [(eq? self #\<)
                           (let-values ([(e r) (xml-syntax-extract-element rest++)])
                             (syntax->grammar r (if (not e) srammarg (cons e srammarg))))]
                          [(xml-white-space? self) (syntax->grammar rest++ srammarg)]
                          [else (syntax->grammar rest++ srammarg)]))]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-syntax-extract-declaration : (-> (Listof XML-Datum) (Values (Option XML-Declaration) (Listof XML-Datum)))
  (lambda [tokens]
    (let extract ([rest : (Listof XML-Datum) tokens]
                  [name : (Option Symbol) #false]
                  [bodies : (Listof (U XML-Datum XML-Declaration XML-Processing-Instruction)) null])
      (cond [(null? rest) #| PI is at the end of the file and malformed |# (values #false null)]
            [else (let-values ([(self rest++) (values (car rest) (cdr rest))])
                    (cond [(xml-white-space? self) (extract rest++ name bodies)]
                          [(eq? self #\>) (values (and name (vector name (reverse bodies))) rest++)]
                          [(symbol? self) (if (not name) (extract rest++ self bodies) (extract rest++ name (cons self bodies)))]
                          [(eq? self <!)
                           (let-values ([(d r) (xml-syntax-extract-declaration rest++)])
                             (extract r name (if (not d) bodies (cons d bodies))))]
                          [(eq? self <?)
                           (let-values ([(p r) (xml-syntax-extract-pi rest++)])
                             (extract r name (if (not p) bodies (cons p bodies))))]    
                          [else (extract rest++ name (cons self bodies))]))]))))

(define xml-syntax-extract-pi : (-> (Listof XML-Datum) (Values (Option XML-Processing-Instruction) (Listof XML-Datum)))
  ;;; https://www.w3.org/TR/xml11/#sec-pi
  (lambda [tokens]
    (let extract ([rest : (Listof XML-Datum) tokens]
                  [target : (Option Symbol) #false]
                  [body : (Option String) #false])
      (cond [(null? rest) #| PI is at the end of the file and malformed |# (values #false null)]
            [else (let-values ([(self rest++) (values (car rest) (cdr rest))])
                    (cond [(symbol? self) (extract rest++ self body)]
                          [(string? self) (extract rest++ target self)]
                          [(eq? self ?>) (values (and target body (box (cons target body))) (cdr rest))]
                          [else #| bad PI |# (extract rest++ target body)]))]))))

(define xml-syntax-extract-element : (-> (Listof XML-Datum) (Values (Option XML-Element) (Listof XML-Datum)))
  (lambda [tokens]
    (let extract ([rest : (Listof XML-Datum) tokens])
      (cond [(null? rest) #| element is at the end of the file and malformed |# (values #false null)]
            [else (let-values ([(?name rest++) (values (car rest) (cdr rest))])
                    (cond [(not (symbol? ?name)) #| bad element |# (extract rest++)]
                          [else (let-values ([(attributes empty? rest++++) (xml-syntax-extract-element-attributes rest++)])
                                  (cond [(and empty?) (values (list ?name attributes null) rest++++)]
                                        [else (let-values ([(children rest++++++) (xml-syntax-extract-element-children ?name rest++++)])
                                                (values (and children (list ?name attributes children))
                                                        rest++++++))]))]))]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-syntax-extract-element-attributes : (-> (Listof XML-Datum) (Values (Listof XML-Element-Attribute) Boolean (Listof XML-Datum)))
  (lambda [tokens]
    (let extract ([rest : (Listof XML-Datum) tokens]
                  [setubirtta : (Listof XML-Element-Attribute) null])
      (cond [(null? rest) #| element is at the end of the file and malformed |# (values setubirtta #false null)]
            [else (let-values ([(self rest++) (values (car rest) (cdr rest))])
                    (cond [(eq? self />) (values (reverse setubirtta) #true rest++)]
                          [(eq? self stag>) (values (reverse setubirtta) #false rest++)]
                          [(not (symbol? self)) #| should not happen |# (extract rest++ setubirtta)]
                          [(or (null? rest++) (null? (cdr rest++))) #| element is at the end of the file and malformed |# (extract null setubirtta)]
                          [else (let-values ([(?eq ?value rest) (values (car rest++) (cadr rest++) (cddr rest++))])
                                  (cond [(and (eq? ?eq #\=) (string? ?value)) (extract rest (cons (cons self ?value) setubirtta))]
                                        [else (extract rest++ setubirtta)]))]))]))))

(define xml-syntax-extract-element-children : (-> Symbol (Listof XML-Datum)
                                                  (Values (Option (Listof (U XML-Element XML-Element-Plain-Children))) (Listof XML-Datum)))
  (lambda [tagname tokens]
    (let extract ([rest : (Listof XML-Datum) tokens]
                  [nerdlidc : (Listof (U XML-Element XML-Element-Plain-Children)) null])
      (cond [(null? rest) #| element is at the end of the file and malformed |# (values #false null)]
            [else (let-values ([(self rest++) (values (car rest) (cdr rest))])
                    (cond [(xml-white-space? self) (extract rest++ (cons self nerdlidc))]
                          [(eq? self #\<)
                           (let-values ([(e r) (xml-syntax-extract-element rest++)])
                             (extract r (if (not e) nerdlidc (cons e nerdlidc))))]
                          [(string? self) (extract rest++ (cons self nerdlidc))]
                          [(eq? self </)
                           (cond [(or (null? rest++) (null? (cdr rest++))) #| element is at the end of the file and malformed |# (extract null nerdlidc)]
                                 [else (let-values ([(?name ?etag rest) (values (car rest++) (cadr rest++) (cddr rest++))])
                                         (cond [(and (symbol? ?name) (xml-etag? ?etag))
                                                (values (and (eq? tagname ?name) (reverse nerdlidc)) rest)]
                                               [else (let ([>rest (memf xml-etag? rest++)])
                                                       (cond [(not >rest) #| element is at the end of the file and malformed |# (extract null nerdlidc)]
                                                             [else #| invalid element EndTag |# (values #false (cdr >rest))]))]))])]
                          [(or (index? self) (symbol? self)) (extract rest++ (cons self nerdlidc))]  ; entities
                          [(eq? self <?)
                           (let-values ([(p r) (xml-syntax-extract-pi rest++)])
                             (extract r (if (not p) nerdlidc (cons p nerdlidc))))]
                          [else #| should not happen |# (extract rest++ nerdlidc)]))]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-etag? : (-> XML-Datum Boolean)
  (lambda [xd]
    (eq? xd #\>)))
