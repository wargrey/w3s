#lang typed/racket/base

(provide (all-defined-out))

(require racket/list)

(require "port.rkt")
(require "../doctype.rkt")
(require "../delimiter.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type XML-Content (U XML-Processing-Instruction XML-Element))

(define-type XML-Declaration (Rec body (Vector Symbol (Listof (U XML-Datum body XML-Processing-Instruction)))))
(define-type XML-Doctype-Body (U XML-Datum XML-Declaration XML-Processing-Instruction))

(define-type XML-Processing-Instruction (MPairof Symbol String))
(define-type XML-Element-Attribute (Pairof Symbol (U String (Boxof String))))
(define-type XML-Element-Plain-Children (U String XML-Processing-Instruction XML-White-Space Index Symbol))
(define-type XML-Element (Rec elem (List Symbol (Listof XML-Element-Attribute) (Listof (U elem XML-Element-Plain-Children)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-syntax->content : (-> (Listof XML-Datum) (Values (Option XML-DocType-Metadata) XML-DTD (Listof XML-Content)))
  (lambda [tokens]
    (define dtd : XML-DTD (make-xml-dtd))
    
    (let syntax->grammar ([rest : (Listof XML-Datum) tokens]
                          [doctype : (Option XML-DocType-Metadata) #false]
                          [srammarg : (Listof XML-Content) null])
      (cond [(null? rest) (values doctype dtd (reverse srammarg))]
            [else (let-values ([(self rest++) (values (car rest) (cdr rest))])
                    (cond [(eq? self <!)
                           (let-values ([(d r) (xml-syntax-extract-declaration rest++)])
                             (cond [(not d) (syntax->grammar r doctype srammarg)]
                                   [(eq? (vector-ref d 0) 'DOCTYPE)
                                    (cond [(not doctype) (syntax->grammar r doctype srammarg)]
                                          [else (let-values ([(metadata sPI) (xml-grammar-parse-doctype d dtd)])
                                                  (syntax->grammar r metadata (append sPI srammarg)))])]
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
    (let extract ([rest : (Listof XML-Datum) tokens]
                  [name : (Option Symbol) #false]
                  [bodies : (Listof XML-Doctype-Body) null])
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
                          [else (extract rest++ name bodies)]))]))))

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
                          [(eq? self ?>) (values (and target body (mcons target body)) (cdr rest))]
                          [else #| bad PI |# (extract rest++ target body)]))]))))

(define xml-syntax-extract-element : (-> (Listof XML-Datum) (Values (Option XML-Element) (Listof XML-Datum)))
  (lambda [tokens]
    (let extract ([rest : (Listof XML-Datum) tokens])
      (cond [(null? rest) #| unexpected EOF |# (values #false null)]
            [else (let-values ([(?name rest++) (values (car rest) (cdr rest))])
                    ; broken start tag should not affect its parent and sibling elements
                    (define tagname : (Option Symbol) (and (symbol? ?name) ?name))
                    (let-values ([(attributes empty? rest++++) (xml-syntax-extract-element-attributes rest++)])
                      (cond [(and empty?) (values (and tagname (list tagname attributes null)) rest++++)]
                            [else (let-values ([(children rest++++++) (xml-syntax-extract-element-children tagname rest++++)])
                                    (values (and children tagname (list tagname attributes children))
                                            rest++++++))])))]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-syntax-extract-element-attributes : (-> (Listof XML-Datum) (Values (Listof XML-Element-Attribute) Boolean (Listof XML-Datum)))
  (lambda [tokens]
    (let extract ([rest : (Listof XML-Datum) tokens]
                  [setubirtta : (Listof XML-Element-Attribute) null])
      (cond [(null? rest) #| unexpected EOF |# (values setubirtta #false null)]
            [else (let-values ([(self rest++) (values (car rest) (cdr rest))])
                    (cond [(eq? self />) (values (reverse setubirtta) #true rest++)]
                          [(eq? self stag>) (values (reverse setubirtta) #false rest++)]
                          [(not (symbol? self)) #| missing name |# (extract rest++ setubirtta)]
                          [(or (null? rest++) (null? (cdr rest++))) #| unexpected EOF |# (extract null setubirtta)]
                          [else (let-values ([(?eq ?value rest) (values (car rest++) (cadr rest++) (cddr rest++))])
                                  (cond [(and (eq? ?eq #\=) (xml-value-string? ?value)) (extract rest (cons (cons self ?value) setubirtta))]
                                        [else (extract rest++ setubirtta)]))]))]))))

(define xml-syntax-extract-element-children : (-> (Option Symbol) (Listof XML-Datum)
                                                  (Values (Option (Listof (U XML-Element XML-Element-Plain-Children))) (Listof XML-Datum)))
  (lambda [tagname tokens]
    (let extract ([rest : (Listof XML-Datum) tokens]
                  [nerdlidc : (Listof (U XML-Element XML-Element-Plain-Children)) null])
      (cond [(null? rest) #| unexpected EOF |# (values #false null)]
            [else (let-values ([(self rest++) (values (car rest) (cdr rest))])
                    (cond [(xml-white-space? self) (extract rest++ (cons self nerdlidc))]
                          [(eq? self #\<)
                           (let-values ([(e r) (xml-syntax-extract-element rest++)])
                             (extract r (if (not e) nerdlidc (cons e nerdlidc))))]
                          [(string? self) (extract rest++ (cons self nerdlidc))]
                          [(eq? self </)
                           (cond [(or (null? rest++) (null? (cdr rest++))) #| unexpected EOF |# (extract null nerdlidc)]
                                 [else (let-values ([(?name ?etag rest) (values (car rest++) (cadr rest++) (cddr rest++))])
                                         (cond [(and (symbol? ?name) (xml-etag? ?etag))
                                                (values (and (eq? tagname ?name) (reverse nerdlidc)) rest)]
                                               [else (let ([>rest (memf xml-etag? rest++)])
                                                       (cond [(not >rest) #| unexpected EOF |# (extract null nerdlidc)]
                                                             [else #| invalid element EndTag |# (values #false (cdr >rest))]))]))])]
                          [(or (index? self) (symbol? self)) (extract rest++ (cons self nerdlidc))]  ; entities
                          [(eq? self <?)
                           (let-values ([(p r) (xml-syntax-extract-pi rest++)])
                             (extract r (if (not p) nerdlidc (cons p nerdlidc))))]
                          [(eq? self <!$CDATA$)
                           (cond [(or (null? rest++) (null? (cdr rest++))) #| unexpected EOF |# (extract null nerdlidc)]
                                 [else (extract (cddr rest++) (cons (assert (car rest++) string?) nerdlidc))])]
                          [else #| should not happen |# (extract rest++ nerdlidc)]))]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-grammar-parse-doctype : (-> XML-Declaration XML-DTD (Values (Option XML-DocType-Metadata) (Listof XML-Processing-Instruction)))
  (lambda [doctype dtd]
    ; Whitespaces have already been filtered out.
    (define-values (declname body) (values (vector-ref doctype 0) (vector-ref doctype 1)))
    (cond [(null? body) (values #false null)]
          [else (let-values ([(self rest) (values (car body) (cdr body))])
                  (cond [(not (symbol? self)) (values #false null)]
                        [else (let* ([ext (xml-grammar-extract-external (vector-ref doctype 0) rest)]
                                     [metainfo (xml-doctype-metadata self (car ext) (cadr ext))])
                                (values metainfo (xml-grammar-extract-internal self (cddr ext) dtd)))]))])))

(define xml-grammar-extract-external : (-> Symbol (Listof XML-Doctype-Body) (List* (Option String) (Option String) (Listof XML-Doctype-Body)))
  (lambda [declname doctype]
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

(define xml-grammar-extract-internal : (-> Symbol (Listof XML-Doctype-Body) XML-DTD (Listof XML-Processing-Instruction))
  (lambda [declname subset0 dtd]
    (define subset : (Listof XML-Doctype-Body)
      (let trim ([rest : (Listof XML-Doctype-Body) subset0])
        (cond [(null? rest) null]
              [else (let-values ([(self rest++) (values (car rest) (cdr rest))])
                      (cond [(eq? self #\[) rest++]
                            [else (trim rest++)]))])))
    (let extract-entity ([rest : (Listof XML-Doctype-Body) subset]
                         [sIP : (Listof XML-Processing-Instruction) null])
      (cond [(null? rest) sIP]
            [else (let-values ([(self rest++) (values (car rest) (cdr rest))])
                    (cond [(eq? self #\]) sIP]
                          [(vector? self)
                           (let ([DECL (vector-ref self 0)])
                             (case DECL
                               [(ENTITY) (xml-grammar-extract-entity declname DECL (vector-ref self 1) dtd)])
                             (extract-entity rest++ sIP))]
                          [(mpair? self) (extract-entity rest++ (cons self sIP))]
                          [else (extract-entity rest++ sIP)]))]))))

(define xml-grammar-extract-entity : (-> Symbol Symbol (Listof XML-Doctype-Body) XML-DTD Void)
  (lambda [declname ENTITY body dtd]
    (define-values (decls others) (partition vector? body))
    (define-values (PIs tokens) (partition mpair? others))
    (cond [(or (pair? others) (pair? PIs)) (void)]
          [(or (null? tokens) (null? (cdr tokens))) (void)]
          [else (let-values ([(?name ?value rest) (values (car tokens) (cadr tokens) (cddr tokens))])
                  (when (and (symbol? ?name) (xml-value-string? ?value) (null? rest))
                    (hash-set! (xml-dtd-entities dtd) ?name ?value)))])
    (void)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-etag? : (-> XML-Datum Boolean)
  (lambda [xd]
    (eq? xd #\>)))

(define xml-value-string? : (-> (U XML-Doctype-Body XML-Datum) Boolean : (U String (Boxof String)))
  (lambda [xd]
    (or (string? xd)
        (box? xd))))
