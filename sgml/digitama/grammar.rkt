#lang typed/racket/base

(provide (all-defined-out) XML-Token)

(require "digicore.rkt")
(require "delimiter.rkt")
(require "doctype.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type XML-Content* (U XML-Processing-Instruction* XML-Element*))
(define-type XML-Definition* (U XML:Entity XML-Processing-Instruction* XML-Declaration* XML-Section))

(define-type XML-Declaration* (Rec body (Vector XML:Name (Listof (U XML-Token body XML-Processing-Instruction* XML-Section)))))
(define-type XML-Doctype-Body* (U XML-Token XML-Declaration* XML-Processing-Instruction* XML-Section))

(define-type XML-Processing-Instruction* (MPairof XML:Name XML:String))
(define-type XML-Element-Attribute* (Pairof XML:Name XML:String))
(define-type XML-Element-Plain-Children* (U XML:String XML-Processing-Instruction* XML:WhiteSpace XML:Entity))
(define-type XML-Element* (Rec elem (List XML:Name (Listof XML-Element-Attribute*) (Listof (U elem XML-Element-Plain-Children*)))))

(struct xml-section
  ([condition : (U XML:Name XML:Entity)]
   [declarations : (Listof XML-Definition*)])
  #:transparent
  #:type-name XML-Section)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-syntax->content* : (-> (Listof XML-Token) (Values (Option XML-DocType-Metadata) (Option XML:Name) (Listof XML-Definition*) (Listof XML-Content*)))
  (lambda [tokens]
    (let syntax->grammar ([rest : (Listof XML-Token) tokens]
                          [doctype : (Option XML-DocType-Metadata) #false]
                          [doctype-name : (Option XML:Name) #false]
                          [definitions : (Listof XML-Definition*) null]
                          [srammarg : (Listof XML-Content*) null])
      (cond [(null? rest) (values doctype doctype-name definitions (reverse srammarg))]
            [else (let-values ([(self rest++) (values (car rest) (cdr rest))])
                    (cond [(xml:whitespace? self) (syntax->grammar rest++ doctype doctype-name definitions srammarg)]
                          [(xml:decl? self)
                           (let-values ([(d r) (xml-syntax-extract-declaration* rest++)])
                             (cond [(not d) (syntax->grammar r doctype doctype-name definitions srammarg)]
                                   [else (let ([declname (vector-ref d 0)])
                                           (cond [(not (xml:name=:=? declname 'DOCTYPE))
                                                  (make+exn:xml:unrecognized declname)
                                                  (syntax->grammar r doctype declname definitions srammarg)]
                                                 [(and doctype)
                                                  (make+exn:xml:duplicate declname)
                                                  (syntax->grammar r doctype declname definitions srammarg)]
                                                 [else (let-values ([(metadata defs) (xml-grammar-parse-doctype* d)])
                                                         (syntax->grammar r metadata declname defs srammarg))]))]))]
                          [(xml:pi? self)
                           (let-values ([(p r) (xml-syntax-extract-pi* rest++)])
                             (syntax->grammar r doctype doctype-name definitions (if (not p) srammarg (cons p srammarg))))]
                          [(xml:stag? self)
                           (let-values ([(e r) (xml-syntax-extract-element* rest++)])
                             (syntax->grammar r doctype doctype-name definitions (if (not e) srammarg (cons e srammarg))))]
                          [else (make+exn:xml:unrecognized self) (syntax->grammar rest++ doctype doctype-name definitions srammarg)]))]))))

(define xml-syntax->definition* : (-> (Listof XML-Token) (Listof XML-Definition*))
  (lambda [tokens]
    (let syntax->grammar ([rest : (Listof XML-Token) tokens]
                          [snoitinifed : (Listof XML-Definition*) null])
      (cond [(null? rest) (reverse snoitinifed)]
            [else (let-values ([(self rest++) (values (car rest) (cdr rest))])
                    (cond [(xml:whitespace? self) (syntax->grammar rest++ snoitinifed)]
                          [(xml:decl? self)
                           (let-values ([(d r) (xml-syntax-extract-declaration* rest++)])
                             (syntax->grammar r (if (not d) snoitinifed (cons d snoitinifed))))]
                          [(xml:csec? self)
                           (let-values ([(s r) (xml-syntax-extract-section* rest++)])
                             (syntax->grammar r (if (not s) snoitinifed (cons s snoitinifed))))]
                          [(xml:pi? self)
                           (let-values ([(p r) (xml-syntax-extract-pi* rest++)])
                             (syntax->grammar r (if (not p) snoitinifed (cons p snoitinifed))))]
                          [(xml:entity? self) (syntax->grammar rest++ (cons self snoitinifed))]
                          [(xml:stag? self)
                           (let-values ([(e r) (xml-syntax-extract-element* rest++)])
                             (unless (not e) (make+exn:xml:misplaced (car e)))
                             (syntax->grammar r snoitinifed))]
                          [else (make+exn:xml:unrecognized self) (syntax->grammar rest++ snoitinifed)]))]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-syntax-extract-declaration* : (-> (Listof XML-Token) (Values (Option XML-Declaration*) (Listof XML-Token)))
  (lambda [tokens]
    ; NOTE: the tokenizer ensures the sequence of declaration tokens
    ;  either <! name tokens ... > or <! error >
    ;  but for the later case, the error token may not span the whole declaration
    ;   and hence some unmatched close tags
    (let extract ([rest : (Listof XML-Token) tokens]
                  [name : (Option XML:Name) #false]
                  [seidob : (Listof XML-Doctype-Body*) null])
      (cond [(null? rest) (make+exn:xml:eof eof) (values #false null)]
            [else (let-values ([(self rest++) (values (car rest) (cdr rest))])
                    (cond [(xml:whitespace? self) (extract rest++ name seidob)]
                          [(xml:etag? self) (values (and name (vector name (reverse seidob))) rest++)]
                          [(xml:name? self) (if (not name) (extract rest++ self seidob) (extract rest++ name (cons self seidob)))]
                          [(xml:decl? self)
                           (let-values ([(d r) (xml-syntax-extract-declaration* rest++)])
                             (extract r name (if (not d) seidob (cons d seidob))))]
                          [(xml:csec? self)
                           (let-values ([(s r) (xml-syntax-extract-section* rest++)])
                             (extract r name (if (not s) seidob (cons s seidob))))]
                          [(xml:pi? self)
                           (let-values ([(p r) (xml-syntax-extract-pi* rest++)])
                             (extract r name (if (not p) seidob (cons p seidob))))]    
                          [else (extract rest++ name (cons self seidob))]))]))))

(define xml-syntax-extract-section* : (-> (Listof XML-Token) (Values (Option XML-Section) (Listof XML-Token)))
  (lambda [tokens]
    ; NOTE: the tokenizer ensures the sequence of declaration tokens
    ;  either <! condition error? [ body ]]>

    (define-values (condition body-tokens)
      (let extract-condition : (Values (U XML:Name XML:Entity False) (Listof XML-Token))
        ([rest : (Listof XML-Token) tokens]
         [condition : (U XML:Name XML:Entity False) #false])
        (cond [(null? rest) (make+exn:xml:eof eof) (values #false null)]
              [else (let-values ([(self rest++) (values (car rest) (cdr rest))])
                      (cond [(xml:whitespace? self) (extract-condition rest++ condition)]
                            [(xml:delim=:=? self csec&) (values condition rest++)]
                            [(or (xml:name? self) (xml:entity? self)) (extract-condition rest++ self)]
                            [else (make+exn:xml:malformed self #false) (extract-condition rest++ condition)]))])))
    
    (let extract ([rest : (Listof XML-Token) body-tokens]
                  [bodies : (Listof XML-Definition*) null])
      (cond [(null? rest) (make+exn:xml:eof eof) (values #false null)]
            [else (let-values ([(self rest++) (values (car rest) (cdr rest))])
                    (cond [(xml:whitespace? self) (extract rest++ bodies)]
                          [(xml:decl? self)
                           (let-values ([(d r) (xml-syntax-extract-declaration* rest++)])
                             (extract r (if (not d) bodies (cons d bodies))))]
                          [(xml:pi? self)
                           (let-values ([(p r) (xml-syntax-extract-pi* rest++)])
                             (extract r (if (not p) bodies (cons p bodies))))]
                          [(xml:entity? self) (extract rest++ (cons self bodies))]
                          [(xml:delim=:=? self $$>) (values (and condition (xml-section condition (reverse bodies))) rest++)]
                          [else (make+exn:xml:unrecognized self condition) (extract rest++ bodies)]))]))))

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
                          [(xml:delim=:=? self ?>) (values (and target body (mcons target body)) (cdr rest))]
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
    ;   < name (attrname=value)* /?>
    ;   no whitespaces and comments among them
    ;   error spans the end of the StartTag or first whitespaces
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
                                        [(xml:eq? ?eq) (make+exn:xml:malformed (list self ?eq ?value) tagname) (extract rest* setubirtta)]
                                        [(xml:name? ?value) (make+exn:xml:malformed (list self ?eq) tagname) (extract (cdr rest++) setubirtta)]
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
                          [(xml:delim=:=? self <!&CDATA&)
                           ; NOTE: the tokenizer ensures the sequence of CDATA token
                           ;   <![CDATA[ text ]]>
                           (cond [(or (null? rest++) (null? (cdr rest++))) (make+exn:xml:eof rest tagname) (extract null nerdlidc)]
                                 [else (extract (cddr rest++) (cons (assert (car rest++) xml:string?) nerdlidc))])]
                          [else (make+exn:xml:unrecognized self tagname) (extract rest++ nerdlidc)]))]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-grammar-throw : (-> (U XML:Name XML:Entity False) (U XML-Doctype-Body* (Listof XML-Doctype-Body*)) (U Void XML-Syntax-Error))
  (lambda [declname bad]
    (cond [(list? bad) (for ([b (in-list bad)]) (xml-grammar-throw declname bad))]
          [(xml-token? bad) (make+exn:xml:malformed bad declname)]
          [(vector? bad) (make+exn:xml:misplaced (vector-ref bad 0) declname)]
          [(box? bad) (make+exn:xml:misplaced (car (unbox bad)) declname)]
          [(xml-section? bad) (make+exn:xml:misplaced (xml-section-condition bad) declname)])))

(define xml-grammar-parse-doctype* : (-> XML-Declaration* (Values (Option XML-DocType-Metadata) (Listof XML-Definition*)))
  (lambda [doctype]
    ; Whitespaces have already been filtered out.
    (define-values (declname body) (values (vector-ref doctype 0) (vector-ref doctype 1)))
    (cond [(null? body) (make+exn:xml:malformed declname) (values #false null)]
          [else (let-values ([(self rest) (values (car body) (cdr body))])
                  (cond [(not (xml:name? self)) (xml-grammar-throw declname self) (values #false null)]
                        [else (let*-values ([(ext) (xml-grammar-extract-external* (vector-ref doctype 0) rest)])
                                (values (xml-doctype-metadata (xml:name-datum self) (car ext) (cadr ext))
                                        (xml-grammar-extract-internal* self (cddr ext))))]))])))

(define xml-grammar-extract-external* : (-> XML:Name (Listof XML-Doctype-Body*) (List* (Option String) (Option String) (Listof XML-Doctype-Body*)))
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

(define xml-grammar-extract-internal* : (-> XML:Name (Listof XML-Doctype-Body*) (Listof XML-Definition*))
  (lambda [declname subset0]
    (define subset : (Listof XML-Doctype-Body*)
      (let trim ([rest : (Listof XML-Doctype-Body*) subset0])
        (cond [(null? rest) null]
              [else (let-values ([(self rest++) (values (car rest) (cdr rest))])
                      (cond [(xml:delim=:=? self #\[) rest++]
                            [else (xml-grammar-throw declname self) (trim rest++)]))])))

    (let extract-definition ([rest : (Listof XML-Doctype-Body*) subset]
                             [snoitinifed : (Listof XML-Definition*) null])
      (cond [(null? rest) #| missing ']', inset is malformed |# (make+exn:xml:eof eof declname) null]
            [else (let-values ([(self rest++) (values (car rest) (cdr rest))])
                    (cond [(xml:entity? self) (extract-definition rest++ (cons self snoitinifed))]
                          [(xml-section? self) (extract-definition rest++ (cons self snoitinifed))]
                          [(vector? self) (extract-definition rest++ (cons self snoitinifed))]
                          [(mpair? self) (extract-definition rest++ (cons self snoitinifed))]
                          [(xml:delim=:=? self #\]) (reverse snoitinifed)]
                          [else (xml-grammar-throw declname self) (extract-definition rest++ snoitinifed)]))]))))
