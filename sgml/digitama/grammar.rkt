#lang typed/racket/base

(provide (all-defined-out) XML-Token)

(require "digicore.rkt")
(require "delimiter.rkt")
(require "doctype.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type XML-Grammar* (U XML-Processing-Instruction* XML-Element*))
(define-type XML-Declaration* (Rec body (Vector XML:Name (Listof (U XML-Token body XML-Processing-Instruction*)))))
(define-type XML-Doctype-Body* (U XML-Token XML-Declaration* XML-Processing-Instruction*))

(define-type XML-Processing-Instruction* (Boxof (Pairof XML:Name XML:String)))
(define-type XML-Internal-Entities* (HashTable Symbol XML:String))
(define-type XML-Element-Attribute* (Pairof XML:Name XML:String))
(define-type XML-Element-Plain-Children* (U XML:String XML-Processing-Instruction* XML:WhiteSpace XML:Entity))
(define-type XML-Element* (Rec elem (List XML:Name (Listof XML-Element-Attribute*) (Listof (U elem XML-Element-Plain-Children*)))))

(define empty-internal-entities* : (Immutable-HashTable Symbol XML:String) (hasheq))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-syntax->grammar* : (-> (Listof XML-Token) (Values (Option XML-DocType-Metadata) XML-Internal-Entities* (Listof XML-Grammar*)))
  (lambda [tokens]
    (define entities : XML-Internal-Entities* (make-hasheq))
    
    (let syntax->grammar ([rest : (Listof XML-Token) tokens]
                          [doctype : (Option XML-DocType-Metadata) #false]
                          [srammarg : (Listof XML-Grammar*) null])
      (cond [(null? rest) (values doctype entities (reverse srammarg))]
            [else (let-values ([(self rest++) (values (car rest) (cdr rest))])
                    (cond [(xml:whitespace? self) (syntax->grammar rest++ doctype srammarg)]
                          [(xml:decl? self)
                           (let-values ([(d r) (xml-syntax-extract-declaration* rest++)])
                             (cond [(not d) (syntax->grammar r doctype srammarg)]
                                   [(xml:name=:=? (vector-ref d 0) 'DOCTYPE)
                                    (let-values ([(metadata sPI) (xml-grammar-parse-doctype d entities)])
                                      (syntax->grammar r metadata (append sPI srammarg)))]
                                   [else (make+exn:xml:unimplemented (vector-ref d 0)) (syntax->grammar r doctype srammarg)]))]
                          [(xml:pi? self)
                           (let-values ([(p r) (xml-syntax-extract-pi* rest++)])
                             (syntax->grammar r doctype (if (not p) srammarg (cons p srammarg))))]
                          [(xml:stag? self)
                           (let-values ([(e r) (xml-syntax-extract-element* rest++)])
                             (syntax->grammar r doctype (if (not e) srammarg (cons e srammarg))))]
                          [else (make+exn:xml:unrecognized self) (syntax->grammar rest++ doctype srammarg)]))]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-syntax-extract-declaration* : (-> (Listof XML-Token) (Values (Option XML-Declaration*) (Listof XML-Token)))
  (lambda [tokens]
    ; NOTE: the tokenizer ensures the sequence of declaration tokens
    ;  either <! name tokens ... > or <! error >
    ;  but for the later case, the error token may not span the whole declaration
    ;   and hence some unmatched close tags
    ;  no whitespaces except comments
    (let extract ([rest : (Listof XML-Token) tokens]
                  [name : (Option XML:Name) #false]
                  [bodies : (Listof (U XML-Token XML-Declaration* XML-Processing-Instruction*)) null])
      (cond [(null? rest) (make+exn:xml:eof eof) (values #false null)]
            [else (let-values ([(self rest++) (values (car rest) (cdr rest))])
                    (cond [(xml:etag? self) (values (and name (vector name (reverse bodies))) rest++)]
                          [(xml:name? self) (if (not name) (extract rest++ self bodies) (extract rest++ name (cons self bodies)))]
                          [(xml:decl? self)
                           (let-values ([(d r) (xml-syntax-extract-declaration* rest++)])
                             (extract r name (if (not d) bodies (cons d bodies))))]
                          [(xml:pi? self)
                           (let-values ([(p r) (xml-syntax-extract-pi* rest++)])
                             (extract r name (if (not p) bodies (cons p bodies))))]    
                          [(xml:whitespace? self) (extract rest++ name bodies)]
                          [else (extract rest++ name (cons self bodies))]))]))))

(define xml-syntax-extract-pi* : (-> (Listof XML-Token) (Values (Option XML-Processing-Instruction*) (Listof XML-Token)))
  ;;; https://www.w3.org/TR/xml11/#sec-pi
  (lambda [tokens]
    ; NOTE: the tokenizer ensures the sequence of PI tokens
    ;  either <? name body ?> or <? error ?> 
    (let extract ([rest : (Listof XML-Token) tokens]
                  [target : (Option XML:Name) #false]
                  [body : (Option XML:String) #false])
      (cond [(null? rest) (make+exn:xml:eof eof) (values #false null)]
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
      (cond [(null? rest) (make+exn:xml:eof eof) (values #false null)]
            [else (let-values ([(?name rest++) (values (car rest) (cdr rest))])
                    ; broken start tag should not affect its parent and sibling elements.
                    (define tagname : (Option XML:Name)
                      (cond [(xml:name? ?name) ?name]
                            [else (make+exn:xml:missing-name ?name) #false]))
                    (let-values ([(attributes empty? rest++++) (xml-syntax-extract-element-attributes* tagname rest++)])
                      (cond [(and empty?) (values (and tagname (list tagname attributes null)) rest++++)]
                            [else (let-values ([(children rest++++++) (xml-syntax-extract-element-children* tagname rest++++)])
                                    (values (and children tagname (list tagname attributes children))
                                            rest++++++))])))]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-syntax-extract-element-attributes* : (-> (Option XML:Name) (Listof XML-Token) (Values (Listof XML-Element-Attribute*) Boolean (Listof XML-Token)))
  ;;; https://www.w3.org/TR/xml11/#NT-Attribute
  (lambda [tagname tokens]
    ; NOTE: the tokenizer ensures the sequence of StartTag token
    ;   < name attrname=value* /?>
    ;   no whitespaces and comments among them
    ;   error spans the rest part of the StartTag
    (let extract ([rest : (Listof XML-Token) tokens]
                  [setubirtta : (Listof XML-Element-Attribute*) null])
      (cond [(null? rest) (make+exn:xml:eof eof tagname) (values setubirtta #false null)]
            [else (let-values ([(self rest++) (values (car rest) (cdr rest))])
                    (cond [(xml:etag? self) (values (reverse setubirtta) #true rest++)]
                          [(xml:cstag? self) (values (reverse setubirtta) #false rest++)]
                          [(not (xml:name? self)) (make+exn:xml:missing-name self tagname) (extract rest++ setubirtta)]
                          [(or (null? rest++) (null? (cdr rest++))) (make+exn:xml:eof rest tagname) (extract null setubirtta)]
                          [else (let-values ([(?eq ?value rest*) (values (car rest++) (cadr rest++) (cddr rest++))])
                                  (cond [(and (xml:eq? ?eq) (xml:string? ?value)) (extract rest* (cons (cons self ?value) setubirtta))]
                                        [(xml:eq? ?eq) (make+exn:xml:malformed (list self ?eq ?value) tagname) (extract (cdr rest++) setubirtta)]
                                        [else (make+exn:xml:malformed (list self ?eq ?value) tagname) (extract rest* setubirtta)]))]))]))))

(define xml-syntax-extract-element-children* : (-> (Option XML:Name) (Listof XML-Token)
                                                   (Values (Option (Listof (U XML-Element* XML-Element-Plain-Children*))) (Listof XML-Token)))
  ;;; https://www.w3.org/TR/xml11/#NT-content
  (lambda [tagname tokens]
    (let extract ([rest : (Listof XML-Token) tokens]
                  [nerdlidc : (Listof (U XML-Element* XML-Element-Plain-Children*)) null])
      (cond [(null? rest) (make+exn:xml:eof eof tagname) (values #false null)]
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
                           (cond [(or (null? rest++) (null? (cdr rest++))) (make+exn:xml:eof rest tagname) (extract null nerdlidc)]
                                 [else (let-values ([(?name ?etag rest*) (values (car rest++) (cadr rest++) (cddr rest++))])
                                         (if (and (xml:name? ?name) (xml:etag? ?etag))
                                             (let ([well-endtag? (and tagname (eq? (xml:name-datum tagname) (xml:name-datum ?name)))])
                                               (when (not well-endtag?) (make+exn:xml:end-tag ?name tagname))
                                               (values (and well-endtag? (reverse nerdlidc)) rest*))
                                             (let ([>rest (memf xml:etag? rest++)])
                                               (cond [(not >rest) (make+exn:xml:eof rest++ tagname) (extract null nerdlidc)]
                                                     [else (make+exn:xml:malformed (take rest++ (- (length rest++) (length >rest) 1)) tagname)
                                                           (values #false (cdr >rest))]))))])]
                          [(xml:entity? self) (extract rest++ (cons self nerdlidc))]
                          [(xml:pi? self)
                           (let-values ([(p r) (xml-syntax-extract-pi* rest++)])
                             (extract r (if (not p) nerdlidc (cons p nerdlidc))))]
                          [(xml:delim=:=? self <!$CDATA$)
                           ; NOTE: the tokenizer ensures the sequence of CDATA token
                           ;   <![CDATA[ text ]]>
                           ;   no whitespaces, errors or comments among them
                           (cond [(or (null? rest++) (null? (cdr rest++))) (make+exn:xml:eof rest tagname) (extract null nerdlidc)]
                                 [else (extract (cddr rest++) (cons (assert (car rest++) xml:string?) nerdlidc))])]
                          [else (make+exn:xml:unrecognized self tagname) (extract rest++ nerdlidc)]))]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-grammar-throw : (-> XML:Name (U XML-Doctype-Body* (Listof XML-Doctype-Body*)) (U Void XML-Syntax-Error))
  (lambda [declname bad]
    (cond [(list? bad) (for ([b (in-list bad)]) (xml-grammar-throw declname bad))]
          [(xml-token? bad) (make+exn:xml:unrecognized bad declname)])))

(define xml-grammar-parse-doctype : (-> XML-Declaration* XML-Internal-Entities* (Values (Option XML-DocType-Metadata) (Listof XML-Processing-Instruction*)))
  (lambda [doctype entities]
    ; Whitespaces have already been filtered out.
    (define-values (declname body) (values (vector-ref doctype 0) (vector-ref doctype 1)))
    (cond [(null? body) (make+exn:xml:malformed declname) (values #false null)]
          [else (let-values ([(self rest) (values (car body) (cdr body))])
                  (cond [(not (xml:name? self)) (xml-grammar-throw declname self) (values #false null)]
                        [else (let* ([ext (xml-grammar-extract-external (vector-ref doctype 0) rest)]
                                     [metainfo (xml-doctype-metadata (xml:name-datum self) (car ext) (cadr ext))])
                                (values metainfo (xml-grammar-extract-internal self (cddr ext) entities)))]))])))

(define xml-grammar-extract-external : (-> XML:Name (Listof XML-Doctype-Body*) (List* (Option String) (Option String) (Listof XML-Doctype-Body*)))
  (lambda [declname doctype]
    (or (and (pair? doctype)
             (let-values ([(self rest) (values (car doctype) (cdr doctype))])
               (and (xml:name? self)
                    (case (xml:name-datum self)
                      [(SYSTEM)
                       (and (pair? rest)
                            (let ([system (car rest)])
                              (list* #false (and (xml:string? system) (xml:string-datum system)) (cdr rest))))]
                      [(PUBLIC)
                       (and (pair? rest) (pair? (cdr rest))
                            (let ([public (car rest)]
                                  [system (cadr rest)])
                              ; do not mess up contents after ExternalID
                              (cond [(and (xml:string? public) (xml:string? system))
                                     (list* (xml:string-datum public) (xml:string-datum system) (cddr rest))]
                                    [(xml:string? public)
                                     (list* (xml:string-datum public) #false (cdr rest))]
                                    [else (list* #false #false rest)])))]
                      [else #false]))))
        #| no external definition, not an error |#
        (list* #false #false doctype))))

(define xml-grammar-extract-internal : (-> XML:Name (Listof XML-Doctype-Body*) XML-Internal-Entities* (Listof XML-Processing-Instruction*))
  (lambda [declname subset0 entities]
    (define subset : (Listof XML-Doctype-Body*)
      (let trim ([rest : (Listof XML-Doctype-Body*) subset0])
        (cond [(null? rest) null]
              [else (let-values ([(self rest++) (values (car rest) (cdr rest))])
                      (cond [(xml:delim=:=? self #\[) rest++]
                            [else (xml-grammar-throw declname self) (trim rest++)]))])))
    null))
