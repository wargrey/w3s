#lang typed/racket/base

(provide (all-defined-out) XML-Token)

(require "digicore.rkt")
(require "doctype.rkt")

(require "tokenizer/errno.rkt")
(require "tokenizer/delimiter.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type XML-Content* (U XML-Processing-Instruction* XML-Element*))
(define-type XML-Definition* (U XML:PEReference XML-Processing-Instruction* XML-Declaration* XML-Section))

(define-type XML-Declaration* (Rec body (Vector XML:Name (Listof (U XML-Token body XML-Processing-Instruction* XML-Section)))))
(define-type XML-Doctype-Body* (U XML-Token XML-Declaration* XML-Processing-Instruction* XML-Section))

(define-type XML-Processing-Instruction* (MPairof XML:Name (Option XML:String)))
(define-type XML-Element-Attribute-Value* (U XML:String XML:Name (Listof XML:Name)))
(define-type XML-Element-Attribute* (Pairof XML:Name XML-Element-Attribute-Value*))
(define-type XML-Subdatum* (U XML-CDATA-Token XML-Reference-Token XML-Processing-Instruction*))
(define-type XML-Element* (Rec elem (List XML:Name (Listof XML-Element-Attribute*) (Listof (U elem XML-Subdatum*)))))

(struct xml-section
  ([condition : (U XML:Name XML:PEReference)]
   [body : (Listof XML-Definition*)])
  #:transparent
  #:type-name XML-Section)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-syntax->content* : (-> (Listof XML-Token) (Values (Option XML-DocType-Metadata*) (Listof XML-Definition*) (Listof XML-Content*)))
  (lambda [tokens]
    (let syntax->grammar ([rest : (Listof XML-Token) tokens]
                          [doctype : (Option XML-DocType-Metadata*) #false]
                          [definitions : (Listof XML-Definition*) null]
                          [srammarg : (Listof XML-Content*) null])
      (cond [(null? rest) (values doctype definitions (reverse srammarg))]
            [else (let-values ([(self rest++) (values (car rest) (cdr rest))])
                    (cond [(xml:whitespace? self) (syntax->grammar rest++ doctype definitions srammarg)]
                          [(xml:decl? self)
                           (let-values ([(d r) (xml-syntax-extract-declaration* rest++)])
                             (cond [(not d) (syntax->grammar r doctype definitions srammarg)]
                                   [else (let ([declname (vector-ref d 0)])
                                           (cond [(not (xml:name=:=? declname 'DOCTYPE))
                                                  (make+exn:xml:unrecognized declname)
                                                  (syntax->grammar r doctype definitions srammarg)]
                                                 [(and doctype)
                                                  (make+exn:xml:duplicate declname)
                                                  (syntax->grammar r doctype definitions srammarg)]
                                                 [else (let-values ([(metadata defs) (xml-grammar-parse-doctype* d)])
                                                         (syntax->grammar r metadata defs srammarg))]))]))]
                          [(xml:pi? self)
                           (let-values ([(p r) (xml-syntax-extract-pi* rest++)])
                             (syntax->grammar r doctype definitions (if (not p) srammarg (cons p srammarg))))]
                          [(xml:stag? self)
                           (let-values ([(e r) (xml-syntax-extract-element* rest++)])
                             (syntax->grammar r doctype definitions (if (not e) srammarg (cons e srammarg))))]
                          [else (xml-bad-token-throw #false self) (syntax->grammar rest++ doctype definitions srammarg)]))]))))

(define xml-syntax->definition* : (-> (Listof XML-Token) (Listof XML-Definition*))
  (lambda [tokens]
    (let syntax->definition ([rest : (Listof XML-Token) tokens]
                             [snoitinifed : (Listof XML-Definition*) null])
      (cond [(null? rest) (reverse snoitinifed)]
            [else (let-values ([(self rest++) (values (car rest) (cdr rest))])
                    (cond [(xml:whitespace? self) (syntax->definition rest++ snoitinifed)]
                          [(xml:decl? self)
                           (let-values ([(d r) (xml-syntax-extract-declaration* rest++)])
                             (syntax->definition r (if (not d) snoitinifed (cons d snoitinifed))))]
                          [(xml:csec? self)
                           (let-values ([(s r) (xml-syntax-extract-section* rest++)])
                             (syntax->definition r (if (not s) snoitinifed (cons s snoitinifed))))]
                          [(xml:pi? self)
                           (let-values ([(p r) (xml-syntax-extract-pi* rest++)])
                             (syntax->definition r (if (not p) snoitinifed (cons p snoitinifed))))]
                          [(xml:pereference? self) (syntax->definition rest++ (cons self snoitinifed))]
                          [(xml:stag? self)
                           (let-values ([(e r) (xml-syntax-extract-element* rest++)])
                             (unless (not e) (make+exn:xml:misplaced (car e)))
                             (syntax->definition r snoitinifed))]
                          [else (xml-bad-token-throw #false self) (syntax->definition rest++ snoitinifed)]))]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-syntax-extract-declaration* : (-> (Listof XML-Token) (Values (Option XML-Declaration*) (Listof XML-Token)))
  (lambda [tokens]
    ; NOTE: the tokenizer ensures the sequence of declaration tokens
    ;  either <! name tokens ... > or <! error >
    ;  but for the later case, the error token may not span the whole declaration
    ;   and hence some unmatched close tags
    (let extract-declaration ([rest : (Listof XML-Token) tokens]
                              [name : (Option XML:Name) #false]
                              [seidob : (Listof XML-Doctype-Body*) null])
      (cond [(null? rest) (make+exn:xml:eof eof) (values #false null)]
            [else (let-values ([(self rest++) (values (car rest) (cdr rest))])
                    (cond [(xml:whitespace? self) (extract-declaration rest++ name seidob)]
                          [(xml:etag? self) (values (and name (vector name (reverse seidob))) rest++)]
                          [(xml:name? self)
                           (cond [(not name) (extract-declaration rest++ self seidob)]
                                 [else (extract-declaration rest++ name (cons self seidob))])]
                          [(xml:decl? self)
                           (let-values ([(d r) (xml-syntax-extract-declaration* rest++)])
                             (extract-declaration r name (if (not d) seidob (cons d seidob))))]
                          [(xml:csec? self)
                           (let-values ([(s r) (xml-syntax-extract-section* rest++)])
                             (extract-declaration r name (if (not s) seidob (cons s seidob))))]
                          [(xml:pi? self)
                           (let-values ([(p r) (xml-syntax-extract-pi* rest++)])
                             (extract-declaration r name (if (not p) seidob (cons p seidob))))]    
                          [else (extract-declaration rest++ name (cons self seidob))]))]))))

(define xml-syntax-extract-section* : (-> (Listof XML-Token) (Values (Option XML-Section) (Listof XML-Token)))
  (lambda [tokens]
    ; NOTE: the tokenizer ensures the sequence of declaration tokens
    ;  either <! condition error? [ body ]]>

    (define-values (condition body-tokens)
      (let extract-condition : (Values (U XML:Name XML:PEReference False) (Listof XML-Token))
        ([rest : (Listof XML-Token) tokens]
         [condition : (U XML:Name XML:PEReference False) #false])
        (cond [(null? rest) (make+exn:xml:eof eof) (values #false null)]
              [else (let-values ([(self rest++) (values (car rest) (cdr rest))])
                      (cond [(xml:whitespace? self) (extract-condition rest++ condition)]
                            [(or (xml:name? self) (xml:pereference? self)) (extract-condition rest++ self)]
                            [(xml:delim=:=? self csec&) (values condition rest++)]                           
                            [else (xml-bad-token-throw #false self) (extract-condition rest++ condition)]))])))
    
    (let extract-section ([rest : (Listof XML-Token) body-tokens]
                          [seidob : (Listof XML-Definition*) null])
      (cond [(null? rest) (make+exn:xml:eof eof condition) (values #false null)]
            [else (let-values ([(self rest++) (values (car rest) (cdr rest))])
                    (cond [(xml:whitespace? self) (extract-section rest++ seidob)]
                          [(xml:decl? self)
                           (let-values ([(d r) (xml-syntax-extract-declaration* rest++)])
                             (extract-section r (if (not d) seidob (cons d seidob))))]
                          [(xml:csec? self)
                           (let-values ([(s r) (xml-syntax-extract-section* rest++)])
                             (extract-section r (if (not s) seidob (cons s seidob))))]
                          [(xml:pi? self)
                           (let-values ([(p r) (xml-syntax-extract-pi* rest++)])
                             (extract-section r (if (not p) seidob (cons p seidob))))]
                          [(xml:pereference? self) (extract-section rest++ (cons self seidob))]
                          [(xml:delim=:=? self $$>) (values (and condition (xml-section condition (reverse seidob))) rest++)]
                          [else (xml-bad-token-throw #false self) (extract-section rest++ seidob)]))]))))

(define xml-syntax-extract-pi* : (-> (Listof XML-Token) (Values (Option XML-Processing-Instruction*) (Listof XML-Token)))
  ;;; https://www.w3.org/TR/xml/#sec-pi
  (lambda [tokens]
    ; NOTE: the tokenizer ensures the sequence of PI tokens
    ;  either <? name body ?> or <? error ?> 
    (let extract-pi ([rest : (Listof XML-Token) tokens]
                     [target : (Option XML:Name) #false]
                     [body : (Option XML:String) #false])
      (cond [(null? rest) (make+exn:xml:eof eof) (values #false null)]
            [else (let-values ([(self rest++) (values (car rest) (cdr rest))])
                    (cond [(xml:name? self) (extract-pi rest++ self body)]
                          [(xml:string? self) (extract-pi rest++ target self)]
                          [(xml:delim=:=? self ?>) (values (and target (mcons target body)) (cdr rest))]
                          [else (xml-bad-token-throw #false self) (extract-pi rest++ target body)]))]))))

(define xml-syntax-extract-element* : (-> (Listof XML-Token) (Values (Option XML-Element*) (Listof XML-Token)))
  ;;; https://www.w3.org/TR/xml/#NT-STag
  ;;; https://www.w3.org/TR/xml/#NT-ETag
  ;;; https://www.w3.org/TR/xml/#NT-content
  (lambda [tokens]
    (cond [(null? tokens) (make+exn:xml:eof eof) (values #false null)]
          [else (let-values ([(?name rest++) (values (car tokens) (cdr tokens))])
                  ; broken start tag should not affect its parent and sibling elements.
                  (define tagname : (Option XML:Name)
                    (cond [(xml:name? ?name) ?name]
                          [else (xml-bad-token-throw #false ?name) #false]))
                  (let-values ([(attributes empty? rest++++) (xml-syntax-extract-element-attributes* tagname rest++)])
                    (cond [(and empty?) (values (and tagname (list tagname attributes null)) rest++++)]
                          [else (let-values ([(children rest++++++) (xml-syntax-extract-subelement* tagname rest++++ #false)])
                                  (values (and children tagname (list tagname attributes children))
                                          rest++++++))])))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-syntax-extract-element-attributes* : (-> (Option XML:Name) (Listof XML-Token) (Values (Listof XML-Element-Attribute*) Boolean (Listof XML-Token)))
  ;;; https://www.w3.org/TR/xml/#NT-Attribute
  (lambda [tagname tokens]
    ; NOTE: the tokenizer ensures the sequence of StartTag token
    ;   < name (attrname=value)* /?>
    ;   no whitespaces and comments among them
    ;   error spans the end of the StartTag or first whitespaces
    (let extract-element-attributes ([rest : (Listof XML-Token) tokens]
                                     [setubirtta : (Listof XML-Element-Attribute*) null])
      (cond [(null? rest) (make+exn:xml:eof eof tagname) (values setubirtta #false null)]
            [else (let-values ([(self rest++) (values (car rest) (cdr rest))])
                    (cond [(xml:etag? self) (values (reverse setubirtta) #true rest++)]
                          [(xml:cstag? self) (values (reverse setubirtta) #false rest++)]
                          [(not (xml:name? self)) (xml-bad-token-throw tagname self) (extract-element-attributes rest++ setubirtta)]
                          [(or (null? rest++) (null? (cdr rest++))) (make+exn:xml:eof rest tagname) (extract-element-attributes null setubirtta)]
                          [else (let-values ([(?eq ?value rest*) (values (car rest++) (cadr rest++) (cddr rest++))])
                                  (cond [(and (xml:eq? ?eq) (xml:string? ?value)) (extract-element-attributes rest* (cons (cons self ?value) setubirtta))]
                                        [(xml:eq? ?eq) (extract-element-attributes (cdr rest++) setubirtta)]
                                        [else (extract-element-attributes rest++ setubirtta)]))]))]))))

(define xml-syntax-extract-subelement* : (-> (Option XML:Name) (Listof XML-Token) Boolean (Values (Option (Listof (U XML-Element* XML-Subdatum*))) (Listof XML-Token)))
  ;;; https://www.w3.org/TR/xml/#NT-content
  (lambda [tagname tokens body-only?]
    (let extract-subelement ([rest : (Listof XML-Token) tokens]
                             [nerdlidc : (Listof (U XML-Element* XML-Subdatum*)) null])
      (cond [(null? rest)
             (cond [(not body-only?) (make+exn:xml:eof eof tagname) (values #false null)]
                   [else #| end expanding general entity |# (values (reverse nerdlidc) null)])]
            [else (let-values ([(self rest++) (values (car rest) (cdr rest))])
                    (cond [(xml-cdata-token? self) (extract-subelement rest++ (cons self nerdlidc))]
                          [(xml:stag? self)
                           (let-values ([(e r) (xml-syntax-extract-element* rest++)])
                             (extract-subelement r (if (not e) nerdlidc (cons e nerdlidc))))]
                          [(xml:oetag? self)
                           ; NOTE: the tokenizer ensures the sequence of EndTag token
                           ;   </ name >
                           ;   no whitespaces and comments among them
                           ;   error spans the rest part of the EndTag
                           (cond [(or (null? rest++) (null? (cdr rest++))) (make+exn:xml:eof rest tagname) (extract-subelement null nerdlidc)]
                                 [else (let-values ([(?name ?etag rest*) (values (car rest++) (cadr rest++) (cddr rest++))])
                                         (if (and (xml:name? ?name) (xml:etag? ?etag))
                                             (let ([well-endtag? (and tagname (eq? (xml:name-datum tagname) (xml:name-datum ?name)))])
                                               (when (not well-endtag?) (make+exn:xml:mismatch ?name tagname))
                                               (values (and well-endtag? (reverse nerdlidc)) rest*))
                                             (let ([>rest (memf xml:etag? rest++)])
                                               (cond [(not >rest) (make+exn:xml:eof rest++ tagname) (extract-subelement null nerdlidc)]
                                                     [else (let ([bads (take rest++ (- (length rest++) (length >rest) 1))])
                                                             (for ([bad (in-list bads)]) (xml-bad-token-throw tagname bad))
                                                             (values #false (cdr >rest)))]))))])]
                          [(xml-reference-token? self) (extract-subelement rest++ (cons self nerdlidc))]
                          [(xml:pi? self)
                           (let-values ([(p r) (xml-syntax-extract-pi* rest++)])
                             (extract-subelement r (if (not p) nerdlidc (cons p nerdlidc))))]
                          [(xml:delim=:=? self <!&CDATA&)
                           ; NOTE: the tokenizer ensures the sequence of CDATA token
                           ;   <![CDATA[ text ]]>
                           (cond [(or (null? rest++) (null? (cdr rest++))) (make+exn:xml:eof rest tagname) (extract-subelement null nerdlidc)]
                                 [else (extract-subelement (cddr rest++) (cons (assert (car rest++) xml:string?) nerdlidc))])]
                          [else (xml-bad-token-throw tagname self) (extract-subelement rest++ nerdlidc)]))]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-grammar-throw : (-> (Option XML-Token) (U XML-Doctype-Body* (Listof XML-Doctype-Body*)) (U Void XML-Syntax-Error))
  (lambda [declname bad]
    (cond [(list? bad) (for ([b (in-list bad)]) (xml-grammar-throw declname bad))]
          [(xml-token? bad) (xml-bad-token-throw declname bad)]
          [(vector? bad) (make+exn:xml:misplaced (vector-ref bad 0) declname)]
          [(box? bad) (make+exn:xml:misplaced (car (unbox bad)) declname)]
          [(xml-section? bad) (make+exn:xml:misplaced (xml-section-condition bad) declname)])))

(define xml-bad-token-throw : (-> (Option XML-Token) XML-Token (U Void XML-Syntax-Error))
  (lambda [declname bad]
    (cond [(xml:bad? bad)
           (let ([errno (cdr (xml:bad-datum bad))])
             (cond [(eq? errno !eof) (make+exn:xml:eof bad declname)]
                   [(eq? errno !char) (make+exn:xml:char bad declname)]
                   [(eq? errno !space) (make+exn:xml:space bad declname)]
                   [(eq? errno !name) (make+exn:xml:missing-name bad declname)]
                   [(eq? errno !value) (make+exn:xml:missing-value bad declname)]
                   [(eq? errno !comment) (make+exn:xml:wfc bad declname)]))]
          [else (make+exn:xml:unrecognized bad declname)])))

(define xml-grammar-parse-doctype* : (-> XML-Declaration* (Values (Option XML-DocType-Metadata*) (Listof XML-Definition*)))
  (lambda [doctype]
    ; Whitespaces have already been filtered out.
    (define-values (declname body) (values (vector-ref doctype 0) (vector-ref doctype 1)))
    (cond [(null? body) (make+exn:xml:malformed declname) (values #false null)]
          [else (let-values ([(self rest) (values (car body) (cdr body))])
                  (cond [(not (xml:name? self)) (xml-grammar-throw declname self) (values #false null)]
                        [else (let*-values ([(ext) (xml-grammar-extract-external* rest)]
                                            [(public system) (values (car ext) (cadr ext))])
                                (values (xml-doctype-metadata* self public system)
                                        (xml-grammar-extract-internal* self (cddr ext))))]))])))

(define xml-grammar-extract-external* : (All (T) (-> (Listof T) (List* (Option XML:String) (Option XML:String) (Listof T))))
  (lambda [doctype]
    (or (and (pair? doctype)
             (let-values ([(self rest) (values (car doctype) (cdr doctype))])
               (and (xml:name? self)
                    (case (xml:name-datum self)
                      [(SYSTEM)
                       (and (pair? rest)
                            (let ([system (car rest)])
                              (list* #false (and (xml:string? system) system) (cdr rest))))]
                      [(PUBLIC)
                       (and (pair? rest) (pair? (cdr rest))
                            (let ([public (car rest)]
                                  [system (cadr rest)])
                              ; do not mess up contents after ExternalID
                              (cond [(and (xml:string? public) (xml:string? system))
                                     (list* public system (cddr rest))]
                                    [(xml:string? public)
                                     (list* public #false (cdr rest))]
                                    [else (list* #false #false rest)])))]
                      [else #false]))))
        #| no external definition, not an error |#
        (list* #false #false doctype))))

(define xml-grammar-extract-internal* : (-> XML:Name (Listof XML-Doctype-Body*) (Listof XML-Definition*))
  (lambda [declname subset0]
    (let trim ([rest : (Listof XML-Doctype-Body*) subset0])
      (cond [(null? rest) null]
            [else (let-values ([(self rest++) (values (car rest) (cdr rest))])
                    (cond [(not (xml:delim=:=? self #\[)) (xml-grammar-throw declname self) (trim rest++)]
                          [else (let extract-intsubset ([rest : (Listof XML-Doctype-Body*) rest++]
                                                        [snoitinifed : (Listof XML-Definition*) null])
                                  (cond [(null? rest) #| missing ']', inset is malformed |# (make+exn:xml:eof eof declname) null]
                                        [else (let-values ([(self rest++) (values (car rest) (cdr rest))])
                                                (cond [(xml:pereference? self) (extract-intsubset rest++ (cons self snoitinifed))]
                                                      [(xml-section? self) (extract-intsubset rest++ (cons self snoitinifed))]
                                                      [(vector? self) (extract-intsubset rest++ (cons self snoitinifed))]
                                                      [(mpair? self) (extract-intsubset rest++ (cons self snoitinifed))]
                                                      [(xml:delim=:=? self #\]) (reverse snoitinifed)]
                                                      [else (xml-grammar-throw declname self) (extract-intsubset rest++ snoitinifed)]))]))]))]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-entity-reference-exists? : (-> (Listof XML-Content*) Boolean)
  (lambda [body]
    (for/or ([elem (in-list body)])
      (and (list? elem)
           (let exists? : Boolean ([elem : XML-Element* elem])
             (or (for/or : Boolean ([attr (in-list (cadr elem))])
                   (xml:&string? (cdr attr)))
                 (for/or : Boolean ([child (in-list (caddr elem))])
                   (or (and (xml:reference? child))
                       (and (list? child) (exists? child))))))))))
