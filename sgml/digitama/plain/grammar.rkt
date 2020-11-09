#lang typed/racket/base

(provide (all-defined-out))

(require racket/list)

(require "../tokenizer/port.rkt")
(require "../tokenizer/delimiter.rkt")

(require "../doctype.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type XML-Content (U XML-Processing-Instruction XML-Element))

(define-type XML-Declaration (Rec body (Vector Symbol (Listof (U XML-Datum body XML-Processing-Instruction)))))
(define-type XML-Doctype-Body (U XML-Datum XML-Declaration XML-Processing-Instruction))

(define-type XML-Processing-Instruction (MPairof Symbol (Option String)))
(define-type XML-Element-Attribute-Value (U String (Boxof String) Symbol (Listof Symbol)))
(define-type XML-Element-Attribute (Pairof Symbol XML-Element-Attribute-Value))
(define-type XML-Subdatum (U String XML-Processing-Instruction XML-White-Space Index Symbol))
(define-type XML-Element (Rec elem (List Symbol (Listof XML-Element-Attribute) (Listof (U elem XML-Subdatum)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-syntax->content : (-> (Listof XML-Datum) (Values (Option XML-Doctype-Metadata) (Listof XML-Content)))
  (lambda [tokens]
    (let syntax->grammar ([rest : (Listof XML-Datum) tokens]
                          [doctype : (Option XML-Doctype-Metadata) #false]
                          [srammarg : (Listof XML-Content) null])
      (cond [(null? rest) (values doctype (reverse srammarg))]
            [else (let-values ([(self rest++) (values (car rest) (cdr rest))])
                    (cond [(eq? self <!)
                           (let-values ([(d r) (xml-syntax-extract-declaration rest++)])
                             (cond [(not d) (syntax->grammar r doctype srammarg)]
                                   [(eq? (vector-ref d 0) 'DOCTYPE)
                                    (cond [(not doctype) (syntax->grammar r doctype srammarg)]
                                          [else (let-values ([(?name ?public ?system sPI) (xml-grammar-parse-doctype d)])
                                                  (syntax->grammar r (xml-doctype-metadata ?name ?public ?system) (append sPI srammarg)))])]
                                   [else (syntax->grammar r doctype srammarg)]))]
                          [(eq? self <?)
                           (let-values ([(p r) (xml-syntax-extract-pi rest++)])
                             (syntax->grammar r doctype (if (not p) srammarg (cons p srammarg))))]
                          [(eq? self #\<)
                           (let-values ([(e r) (xml-syntax-extract-element rest++)])
                             (syntax->grammar r doctype (if (not e) srammarg (cons e srammarg))))]
                          [(xml-white-space? self) (syntax->grammar rest++ doctype srammarg)]
                          [else (syntax->grammar rest++ doctype srammarg)]))]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-syntax-extract-declaration : (-> (Listof XML-Datum) (Values (Option XML-Declaration) (Listof XML-Datum)))
  (lambda [tokens]
    (let extract-declaration ([rest : (Listof XML-Datum) tokens]
                              [name : (Option Symbol) #false]
                              [bodies : (Listof XML-Doctype-Body) null])
      (cond [(null? rest) #| unexpected EOF |# (values #false null)]
            [else (let-values ([(self rest++) (values (car rest) (cdr rest))])
                    (cond [(xml-white-space? self) (extract-declaration rest++ name bodies)]
                          [(eq? self #\>) (values (and name (vector name (reverse bodies))) rest++)]
                          [(eq? self <!)
                           (let-values ([(d rest++++) (xml-syntax-extract-declaration rest++)])
                             (extract-declaration rest++++ name (if (not d) bodies (cons d bodies))))]
                          [(eq? self <?)
                           (let-values ([(p rest++++) (xml-syntax-extract-pi rest++)])
                             (extract-declaration rest++++ name (if (not p) bodies (cons p bodies))))]
                          [(symbol? self) ; WARNING: do not move up this clause since `<!` and `<?` are also symbols
                           (cond [(not name) (extract-declaration rest++ self bodies)]
                                 [else (extract-declaration rest++ name (cons self bodies))])]
                          [else (extract-declaration rest++ name (cons self bodies))]))]))))

(define xml-syntax-extract-pi : (-> (Listof XML-Datum) (Values (Option XML-Processing-Instruction) (Listof XML-Datum)))
  ;;; https://www.w3.org/TR/xml/#sec-pi
  (lambda [tokens]
    (let extract-pi ([rest : (Listof XML-Datum) tokens]
                     [target : (Option Symbol) #false]
                     [body : (Option String) #false])
      (cond [(null? rest) #| PI is at the end of the file and malformed |# (values #false null)]
            [else (let-values ([(self rest++) (values (car rest) (cdr rest))])
                    (cond [(eq? self ?>) (values (and target (mcons target body)) (cdr rest))]
                          [(symbol? self) (extract-pi rest++ self body)] ; WARNING: do not move up this clause since `?>` is also a symbol
                          [(string? self) (extract-pi rest++ target self)]
                          [else #| bad PI |# (extract-pi rest++ target body)]))]))))

(define xml-syntax-extract-element : (-> (Listof XML-Datum) (Values (Option XML-Element) (Listof XML-Datum)))
  (lambda [tokens]
    (cond [(null? tokens) #| unexpected EOF |# (values #false null)]
          [else (let-values ([(?name rest++) (values (car tokens) (cdr tokens))])
                  ; broken start tag should not affect its parent and sibling elements
                  (define tagname : (Option Symbol) (and (symbol? ?name) ?name))
                  (let-values ([(attributes empty? rest++++) (xml-syntax-extract-element-attributes rest++)])
                    (cond [(and empty?) (values (and tagname (list tagname attributes null)) rest++++)]
                          [else (let-values ([(children rest++++++) (xml-syntax-extract-subelement tagname rest++++)])
                                  (values (and children tagname (list tagname attributes children))
                                          rest++++++))])))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-syntax-extract-element-attributes : (-> (Listof XML-Datum) (Values (Listof XML-Element-Attribute) Boolean (Listof XML-Datum)))
  (lambda [tokens]
    (let extract-element-attributes ([rest : (Listof XML-Datum) tokens]
                                     [setubirtta : (Listof XML-Element-Attribute) null])
      (cond [(null? rest) #| unexpected EOF |# (values setubirtta #false null)]
            [else (let-values ([(self rest++) (values (car rest) (cdr rest))])
                    (cond [(eq? self />) (values (reverse setubirtta) #true rest++)]
                          [(eq? self stag>) (values (reverse setubirtta) #false rest++)]
                          [(not (symbol? self)) #| missing name |# (extract-element-attributes rest++ setubirtta)]
                          [(or (null? rest++) (null? (cdr rest++))) #| unexpected EOF |# (extract-element-attributes null setubirtta)]
                          [else (let-values ([(?eq ?value rest) (values (car rest++) (cadr rest++) (cddr rest++))])
                                  (cond [(and (eq? ?eq #\=) (xml-value-string? ?value)) (extract-element-attributes rest (cons (cons self ?value) setubirtta))]
                                        [else (extract-element-attributes rest++ setubirtta)]))]))]))))

(define xml-syntax-extract-subelement : (-> (Option Symbol) (Listof XML-Datum) (Values (Option (Listof (U XML-Element XML-Subdatum))) (Listof XML-Datum)))
  (lambda [tagname tokens]
    (let extract-subelement ([rest : (Listof XML-Datum) tokens]
                             [nerdlidc : (Listof (U XML-Element XML-Subdatum)) null])
      (cond [(null? rest) #| unexpected EOF |# (values #false null)]
            [else (let-values ([(self rest++) (values (car rest) (cdr rest))])
                    (cond [(xml-white-space? self) (extract-subelement rest++ (cons self nerdlidc))]
                          [(eq? self #\<)
                           (let-values ([(e r) (xml-syntax-extract-element rest++)])
                             (extract-subelement r (if (not e) nerdlidc (cons e nerdlidc))))]
                          [(string? self) (extract-subelement rest++ (cons self nerdlidc))]
                          [(eq? self </)
                           (cond [(or (null? rest++) (null? (cdr rest++))) #| unexpected EOF |# (extract-subelement null nerdlidc)]
                                 [else (let-values ([(?name ?etag rest) (values (car rest++) (cadr rest++) (cddr rest++))])
                                         (cond [(and (symbol? ?name) (xml-etag? ?etag))
                                                (values (and (eq? tagname ?name) (reverse nerdlidc)) rest)]
                                               [else (let ([>rest (memf xml-etag? rest++)])
                                                       (cond [(not >rest) #| unexpected EOF |# (extract-subelement null nerdlidc)]
                                                             [else #| invalid element EndTag |# (values #false (cdr >rest))]))]))])]
                          [(eq? self <?)
                           (let-values ([(p r) (xml-syntax-extract-pi rest++)])
                             (extract-subelement r (if (not p) nerdlidc (cons p nerdlidc))))]
                          [(eq? self <!&CDATA&)
                           (cond [(or (null? rest++) (null? (cdr rest++))) #| unexpected EOF |# (extract-subelement null nerdlidc)]
                                 [else (extract-subelement (cddr rest++) (cons (assert (car rest++) string?) nerdlidc))])]
                          [(or (index? self) (symbol? self)) (extract-subelement rest++ (cons self nerdlidc))]  ; entities
                          [else #| should not happen |# (extract-subelement rest++ nerdlidc)]))]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-grammar-parse-doctype : (-> XML-Declaration (Values (Option Symbol) (Option String) (Option String) (Listof XML-Processing-Instruction)))
  (lambda [doctype]
    ; Whitespaces have already been filtered out.
    (define-values (declname body) (values (vector-ref doctype 0) (vector-ref doctype 1)))

    (cond [(null? body) (values #false #false #false null)]
          [else (let-values ([(self rest) (values (car body) (cdr body))])
                  (cond [(not (symbol? self)) (values #false #false #false null)]
                        [else (let ([ext (xml-grammar-extract-external rest)])
                                (values self (car ext) (cadr ext)
                                        (xml-grammar-extract-internal self (cddr ext))))]))])))

(define xml-grammar-extract-external : (-> (Listof XML-Doctype-Body) (List* (Option String) (Option String) (Listof XML-Doctype-Body)))
  (lambda [doctype]
    (or (and (pair? doctype)
             (let-values ([(self rest) (values (car doctype) (cdr doctype))])
               (case self
                 [(SYSTEM)
                  (and (pair? rest)
                       (let ([system (car rest)])
                         (list* #false (and (string? system) system) (cdr rest))))]
                 [(PUBLIC)
                  (and (pair? rest) (pair? (cdr rest))
                       (let ([public (car rest)]
                             [system (cadr rest)])
                         ; do not mess up contents after ExternalID
                         (cond [(and (string? public) (string? system))
                                (list* public system (cddr rest))]
                               [(string? public)
                                (list* public #false (cdr rest))]
                               [else (list* #false #false rest)])))]
                 [else #false])))
        #| no external definition, not an error |#
        (list* #false #false doctype))))

(define xml-grammar-extract-internal : (-> Symbol (Listof XML-Doctype-Body) (Listof XML-Processing-Instruction))
  (lambda [declname subset0]
    (define subset : (Listof XML-Doctype-Body)
      (let trim ([rest : (Listof XML-Doctype-Body) subset0])
        (cond [(null? rest) null]
              [else (let-values ([(self rest++) (values (car rest) (cdr rest))])
                      (cond [(eq? self #\[) rest++]
                            [else (trim rest++)]))])))
    (let extract-intsubset ([rest : (Listof XML-Doctype-Body) subset]
                            [sIP : (Listof XML-Processing-Instruction) null])
      (cond [(null? rest) sIP]
            [else (let-values ([(self rest++) (values (car rest) (cdr rest))])
                    (cond [(eq? self #\]) sIP]
                          [(vector? self) (extract-intsubset rest++ sIP)]
                          [(mpair? self) (extract-intsubset rest++ (cons self sIP))]
                          [else (extract-intsubset rest++ sIP)]))]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-etag? : (-> (U XML-Datum EOF) Boolean)
  (lambda [xd]
    (eq? xd #\>)))

(define xml-value-string? : (-> (U XML-Doctype-Body XML-Datum) Boolean : (U String (Boxof String)))
  (lambda [xd]
    (or (string? xd)
        (box? xd))))
