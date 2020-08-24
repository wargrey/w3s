#lang typed/racket/base

(provide (all-defined-out) XML-Token)

(require "digicore.rkt")
(require "delimiter.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type XML-Grammar* (U XML-Declaration* XML-Processing-Instruction* XML-Element*))

(define-type XML-Processing-Instruction* (Boxof (Pairof XML:Name XML:String)))
(define-type XML-Declaration* (Rec body (Vector XML:Name (Listof (U XML-Token body XML-Processing-Instruction*)))))
(define-type XML-Element-Attribute* (Pairof XML:Name XML:String))
(define-type XML-Element-Plain-Children* (U XML:String XML-Processing-Instruction* XML:WhiteSpace XML:Entity))
(define-type XML-Element* (Rec elem (List XML:Name (Listof XML-Element-Attribute*) (Listof (U elem XML-Element-Plain-Children*)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-syntax->grammar* : (-> (Listof XML-Token) (Listof XML-Grammar*))
  (lambda [tokens]
    (let syntax->grammar ([rest : (Listof XML-Token) tokens]
                          [srammarg : (Listof XML-Grammar*) null])
      (cond [(null? rest) (reverse srammarg)]
            [else (let-values ([(self rest++) (values (car rest) (cdr rest))])
                    (cond [(xml:decl? self)
                           (let-values ([(d r) (xml-syntax-extract-declaration* rest++)])
                             (syntax->grammar r (if (not d) srammarg (cons d srammarg))))]
                          [(xml:pi? self)
                           (let-values ([(p r) (xml-syntax-extract-pi* rest++)])
                             (syntax->grammar r (if (not p) srammarg (cons p srammarg))))]
                          [(xml:stag? self)
                           (let-values ([(e r) (xml-syntax-extract-element* rest++)])
                             (syntax->grammar r (if (not e) srammarg (cons e srammarg))))]
                          [(xml:whitespace? self) (syntax->grammar rest++ srammarg)]
                          [else (syntax->grammar rest++ srammarg)]))]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-syntax-extract-declaration* : (-> (Listof XML-Token) (Values (Option XML-Declaration*) (Listof XML-Token)))
  (lambda [tokens]
    ; NOTE: the tokenizer ensures the sequence of declaration tokens
    ;  either <! name tokens ... > or <! error >
    ;  but for the later case, the error token may not span the whole declaration
    ;   and hence some unmatched close tags
    (let extract ([rest : (Listof XML-Token) tokens]
                  [name : (Option XML:Name) #false]
                  [bodies : (Listof (U XML-Token XML-Declaration* XML-Processing-Instruction*)) null])
      (cond [(null? rest) #| PI is at the end of the file and malformed |# (values #false null)]
            [else (let-values ([(self rest++) (values (car rest) (cdr rest))])
                    (cond [(xml:whitespace? self) (extract rest++ name bodies)]
                          [(xml:etag? self) (values (and name (vector name (reverse bodies))) rest++)]
                          [(xml:name? self) (if (not name) (extract rest++ self bodies) (extract rest++ name (cons self bodies)))]
                          [(xml:decl? self)
                           (let-values ([(d r) (xml-syntax-extract-declaration* rest++)])
                             (extract r name (if (not d) bodies (cons d bodies))))]
                          [(xml:pi? self)
                           (let-values ([(p r) (xml-syntax-extract-pi* rest++)])
                             (extract r name (if (not p) bodies (cons p bodies))))]    
                          [else (extract rest++ name (cons self bodies))]))]))))

(define xml-syntax-extract-pi* : (-> (Listof XML-Token) (Values (Option XML-Processing-Instruction*) (Listof XML-Token)))
  ;;; https://www.w3.org/TR/xml11/#sec-pi
  (lambda [tokens]
    ; NOTE: the tokenizer ensures the sequence of PI tokens
    ;  either <? name body ?> or <? error ?> 
    (let extract ([rest : (Listof XML-Token) tokens]
                  [target : (Option XML:Name) #false]
                  [body : (Option XML:String) #false])
      (cond [(null? rest) #| PI is at the end of the file and malformed |# (values #false null)]
            [else (let-values ([(self rest++) (values (car rest) (cdr rest))])
                    (cond [(xml:name? self) (extract rest++ self body)]
                          [(xml:string? self) (extract rest++ target self)]
                          [(xml:delim=:=? self ?>) (values (and target body (box (cons target body))) (cdr rest))]
                          [else #| bad PI |# (extract rest++ target body)]))]))))

(define xml-syntax-extract-element* : (-> (Listof XML-Token) (Values (Option XML-Element*) (Listof XML-Token)))
  ;;; https://www.w3.org/TR/xml11/#NT-STag
  ;;; https://www.w3.org/TR/xml11/#NT-ETag
  ;;; https://www.w3.org/TR/xml11/#NT-content
  (lambda [tokens]
    (let extract ([rest : (Listof XML-Token) tokens])
      (cond [(null? rest) #| element is at the end of the file and malformed |# (values #false null)]
            [else (let-values ([(?name rest++) (values (car rest) (cdr rest))])
                    (cond [(not (xml:name? ?name)) #| bad element |# (extract rest++)]
                          [else (let-values ([(attributes empty? rest++++) (xml-syntax-extract-element-attributes* rest++)])
                                  (cond [(and empty?) (values (list ?name attributes null) rest++++)]
                                        [else (let-values ([(children rest++++++) (xml-syntax-extract-element-children* ?name rest++++)])
                                                (values (and children (list ?name attributes children))
                                                        rest++++++))]))]))]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-syntax-extract-element-attributes* : (-> (Listof XML-Token) (Values (Listof XML-Element-Attribute*) Boolean (Listof XML-Token)))
  ;;; https://www.w3.org/TR/xml11/#NT-Attribute
  (lambda [tokens]
    ; NOTE: the tokenizer ensures the sequence of StartTag token
    ;   < name attrname=value* /?>
    ;   no whitespaces and comments among them
    ;   error spans the rest part of the StartTag
    (let extract ([rest : (Listof XML-Token) tokens]
                  [setubirtta : (Listof XML-Element-Attribute*) null])
      (cond [(null? rest) #| element is at the end of the file and malformed |# (values setubirtta #false null)]
            [else (let-values ([(self rest++) (values (car rest) (cdr rest))])
                    (cond [(xml:etag? self) (values (reverse setubirtta) #true rest++)]
                          [(xml:cstag? self) (values (reverse setubirtta) #false rest++)]
                          [(not (xml:name? self)) #| should not happen |# (extract rest++ setubirtta)]
                          [(or (null? rest++) (null? (cdr rest++))) #| element is at the end of the file and malformed |# (extract null setubirtta)]
                          [else (let-values ([(?eq ?value rest*) (values (car rest++) (cadr rest++) (cdr rest++))])
                                  (cond [(and (xml:eq? ?eq) (xml:string? ?value)) (extract rest* (cons (cons self ?value) setubirtta))]
                                        [else (extract rest++ setubirtta)]))]))]))))

(define xml-syntax-extract-element-children* : (-> XML:Name (Listof XML-Token)
                                                   (Values (Option (Listof (U XML-Element* XML-Element-Plain-Children*))) (Listof XML-Token)))
  ;;; https://www.w3.org/TR/xml11/#NT-content
  (lambda [tagname tokens]
    (let extract ([rest : (Listof XML-Token) tokens]
                  [nerdlidc : (Listof (U XML-Element* XML-Element-Plain-Children*)) null])
      (cond [(null? rest) #| element is at the end of the file and malformed |# (values #false null)]
            [else (let-values ([(self rest++) (values (car rest) (cdr rest))])
                    (cond [(xml:whitespace? self) (extract rest++ (cons self nerdlidc))]
                          [(xml:stag? self)
                           (let-values ([(e r) (xml-syntax-extract-element* rest++)])
                             (extract r (if (not e) nerdlidc (cons e nerdlidc))))]
                          [(xml:string? self) (extract rest++ (cons self nerdlidc))]
                          [(xml:oetag? self)
                           ; NOTE: the tokenizer ensures the sequence of EndTag token
                           ;   </ name >
                           ;   no whitespaces and comments among them
                           ;   error spans the rest part of the EndTag
                           (cond [(or (null? rest++) (null? (cdr rest++))) #| element is at the end of the file and malformed |# (extract null nerdlidc)]
                                 [else (let-values ([(?name ?etag rest*) (values (car rest++) (cadr rest++) (cdr rest++))])
                                         (cond [(and (xml:name? ?name) (xml:etag? ?etag))
                                                (values (and (eq? (xml:name-datum tagname) (xml:name-datum ?name)) (reverse nerdlidc)) rest*)]
                                               [else (let ([>rest (memf xml:etag? rest++)])
                                                       (cond [(not >rest) #| element is at the end of the file and malformed |# (extract null nerdlidc)]
                                                             [else #| invalid element EndTag |# (values #false (cdr >rest))]))]))])]
                          [(xml:entity? self) (extract rest++ (cons self nerdlidc))]
                          [(xml:pi? self)
                           (let-values ([(p r) (xml-syntax-extract-pi* rest++)])
                             (extract r (if (not p) nerdlidc (cons p nerdlidc))))]
                          [else #| should not happen |# (extract rest++ nerdlidc)]))]))))
