#lang typed/racket/base

(provide (all-defined-out))

(require racket/list)

(require "../tokenizer/port.rkt")
(require "../tokenizer/delimiter.rkt")

(require "../plain/grammar.rkt")
(require "../plain/prompt.rkt")

(require "../stdin.rkt")
(require "../prentity.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type XML-Element-Event (U 'open 'close 'close-tag 'close-empty))

(define-type (XML-Prolog-Handler seed) (-> (U String Symbol) Nonnegative-Flonum (Option String) Boolean XML-Element-Event seed seed))
(define-type (XML-Doctype-Handler seed) (-> (Option Symbol) (Option String) (Option String) seed seed))
(define-type (XML-PI-Handler seed) (-> (Option Symbol) Symbol (Option String) seed seed))
(define-type (XML-Element-Handler seed) (-> Symbol Index XML-Element-Event seed seed))
(define-type (XML-Attribute-Handler seed) (-> Symbol Symbol (U String (Boxof String)) seed seed))
(define-type (XML-AttriList-Handler seed) (-> Symbol (Listof XML-Element-Attribute) seed seed))
(define-type (XML-PCData-Handler seed) (-> Symbol String Boolean seed seed))
(define-type (XML-Space-Handler seed) (-> Symbol String Boolean seed seed))
(define-type (XML-GEReference-Handler seed) (-> (U Symbol Index) (Option Char) seed seed))
(define-type (XML-Comment-Handler seed) (-> (Option Symbol) String seed seed))

(struct (seed) xml-event-handler
  ([prolog : (XML-Prolog-Handler seed)]
   [doctype : (XML-Doctype-Handler seed)]
   [pi : (XML-PI-Handler seed)]
   [element : (XML-Element-Handler seed)]
   [attribute : (XML-Attribute-Handler seed)]
   [attrilist : (XML-AttriList-Handler seed)]
   [pcdata : (XML-PCData-Handler seed)]
   [space : (XML-Space-Handler seed)]
   [geref : (XML-GEReference-Handler seed)]
   [comment : (XML-Comment-Handler seed)])
  #:type-name XML-Event-Handler
  #:transparent)

(define #:forall (seed) (make-xml-event-handler
                         #:prolog [document : (U (XML-Prolog-Handler seed) False Void) (void)]
                         #:doctype [doctype : (U (XML-Doctype-Handler seed) False Void) (void)]
                         #:pi [pi : (U (XML-PI-Handler seed) False Void) (void)]
                         #:element [element : (U (XML-Element-Handler seed) False Void) (void)]
                         #:attribute [attr : (U (XML-Attribute-Handler seed) False Void) (void)]
                         #:attrilist [attrlist : (U (XML-AttriList-Handler seed) False Void) (void)]
                         #:pcdata [pcdata : (U (XML-PCData-Handler seed) False Void) (void)]
                         #:space [space : (U (XML-Space-Handler seed) False Void) (void)]
                         #:gereference [geref : (U (XML-GEReference-Handler seed) False Void) (void)]
                         #:comment [comment : (U (XML-Comment-Handler seed) False Void) (void)]
                         [src : (Option (XML-Event-Handler seed)) #false]) : (XML-Event-Handler seed)
  (xml-event-handler (or (if (void? document) (and src (xml-event-handler-prolog src)) document) sax-arity6-identity)
                     (or (if (void? doctype) (and src (xml-event-handler-doctype src)) doctype) sax-arity4-identity)
                     (or (if (void? pi) (and src (xml-event-handler-pi src)) pi) sax-arity4-identity)
                     (or (if (void? element) (and src (xml-event-handler-element src)) element) sax-arity4-identity)
                     (or (if (void? attr) (and src (xml-event-handler-attribute src)) attr) sax-arity4-identity)
                     (or (if (void? attrlist) (and src (xml-event-handler-attrilist src)) attrlist) sax-arity3-identity)
                     (or (if (void? pcdata) (and src (xml-event-handler-pcdata src)) pcdata) sax-arity4-identity)
                     (or (if (void? space) (and src (xml-event-handler-space src)) space) sax-arity4-identity)
                     (or (if (void? geref) (and src (xml-event-handler-geref src)) geref) sax-arity3-identity)
                     (or (if (void? comment) (and src (xml-event-handler-comment src)) comment) sax-arity3-identity)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (seed) read-xml-datum : (case-> [SGML-StdIn (XML-Event-Handler Void) -> Void]
                                                 [SGML-StdIn (XML-Event-Handler seed) seed -> seed])
  (case-lambda
    [(/dev/rawin saxcb) (read-xml-datum /dev/rawin saxcb (void))]
    [(/dev/rawin saxcb datum0)
     (parameterize ([current-custodian (make-custodian)])
       (define-values (/dev/xmlin version encoding standalone?) (xml-open-input-port /dev/rawin #false))
       (define pname : (U String Symbol) (sgml-port-name /dev/xmlin))
       (define datum : (U seed exn)
         (with-handlers ([exn? (λ [[e : exn]] e)])
           (sax-start (if (string? pname) (string->symbol pname) pname)
                      (λ [] (xml-sax /dev/xmlin pname version encoding standalone? saxcb datum0)))))
       (custodian-shutdown-all (current-custodian))
       (cond [(exn? datum) (raise datum)]
             [else datum]))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (seed) xml-sax : (-> Input-Port (U String Symbol) Nonnegative-Flonum (Option String) Boolean (XML-Event-Handler seed) seed seed)
  (lambda [/dev/xmlin pname version encoding standalone? saxcb datum0]
    (define fprolog : (XML-Prolog-Handler seed) (xml-event-handler-prolog saxcb))
    (define fdoctype : (XML-Doctype-Handler seed) (xml-event-handler-doctype saxcb))
    (define fprocess : (XML-PI-Handler seed) (xml-event-handler-pi saxcb))
    (define fcomment : (XML-Comment-Handler seed) (xml-event-handler-comment saxcb))
    
    (let sax ([consume : XML-Token-Consumer xml-consume-token:*]
              [scope : XML-Scope xml-initial-scope]
              [datum : seed (fprolog pname version encoding standalone? 'open datum0)])
      (let-values ([(self consume++ scope++) (xml-consume-token /dev/xmlin consume scope)])
        (cond [(eof-object? self) (fprolog pname version encoding standalone? 'close datum)]
              [(eq? self <!)
               (let-values ([(d consume++++ scope++++) (xml-sax-declaration /dev/xmlin consume++ scope++)])
                 (cond [(not d) (sax consume++++ scope++++ datum)]
                       [(eq? (vector-ref d 0) 'DOCTYPE)
                        (let-values ([(?name ?public ?system sPI) (xml-grammar-parse-doctype d)])
                          (sax consume++++ scope++++ (fdoctype ?name ?public ?system datum)))]
                       [else (sax consume++++ scope++++ datum)]))]
              [(eq? self <?)
               (let-values ([(p consume++++ scope++++) (xml-sax-pi /dev/xmlin consume++ scope++)])
                 (sax consume++++ scope++++ (if (mpair? p) (fprocess #false (mcar p) (mcdr p) datum) datum)))]
              [(eq? self #\<)
               (let-values ([(consume++++ scope++++ datum++) (xml-sax-element /dev/xmlin consume++ scope++ saxcb datum)])
                 (sax consume++++ scope++++ (or datum++ datum)))]
              [(xml-comment? self) (sax consume++ scope++ (fcomment #false (xml-white-space-raw self) datum))]
              [else (sax consume++ scope++ datum)])))))

(define xml-sax-declaration : (-> Input-Port XML-Token-Consumer XML-Scope (Values (Option XML-Declaration) XML-Token-Consumer XML-Scope))
  (lambda [/dev/xmlin consume scope]
    (let sax-decl ([consume : XML-Token-Consumer consume]
                   [scope : XML-Scope scope]
                   [name : (Option Symbol) #false]
                   [bodies : (Listof XML-Doctype-Body) null]
                   [need? : Boolean #false])
      (define-values (self consume++ scope++) (xml-consume-token /dev/xmlin consume scope))
      
      (cond [(eof-object? self) (values #false consume++ scope++)]
            [(xml-white-space? self) (sax-decl consume++ scope++ name bodies need?)]
            [(eq? self #\>) (values (and name (vector name (reverse bodies))) consume++ scope++)]
            [(eq? self <!)
             (let-values ([(d consume++++ scope++++) (xml-sax-declaration /dev/xmlin consume++ scope++)])
               (sax-decl consume++++ scope++++ name bodies #| drop internal DTD declarations |# need?))]
            [(eq? self <?)
             (let-values ([(p consume++++ scope++++) (xml-sax-pi /dev/xmlin consume++ scope++)])
               (sax-decl consume++++ scope++++ name bodies #| drop processing instructions inside the internal DTD |# need?))]
            [(symbol? self) ; WARNING: do not move up this clause since `<!` and `<?` are also symbols
             (cond [(not name) (sax-decl consume++ scope++ self bodies (eq? self 'DOCTYPE))]
                   [else (sax-decl consume++ scope++ name (if (not need?) bodies (cons self bodies)) need?)])]
            [else (let ([need?++ (and need? (not (eq? self #\[)))]) ; only tokens between `DOCTYPE` and `[` are collected
                    (sax-decl consume++ scope++ name (if (not need?++) bodies (cons self bodies)) need?++))]))))

(define xml-sax-pi : (-> Input-Port XML-Token-Consumer XML-Scope (Values (Option XML-Processing-Instruction) XML-Token-Consumer XML-Scope))
  ;;; https://www.w3.org/TR/xml/#sec-pi
  (lambda [/dev/xmlin consume scope]
    (let sax-pi ([consume : XML-Token-Consumer consume]
                 [scope : XML-Scope scope]
                 [target : (Option Symbol) #false]
                 [body : (Option String) #false])
      (define-values (self consume++ scope++) (xml-consume-token /dev/xmlin consume scope))

      (cond [(eof-object? self) (values #false consume++ scope++)]
            [(eq? self ?>) (values (and target (mcons target body)) consume++ scope++)]
            [(symbol? self) (sax-pi consume++ scope++ self body)] ; WARNING: do not move up this clause since `?>` is also a symbol
            [(string? self) (sax-pi consume++ scope++ target self)]
            [else (sax-pi consume++ scope++ target body)]))))

(define xml-sax-element : (All (seed) (-> Input-Port XML-Token-Consumer XML-Scope (XML-Event-Handler seed) (Option seed)
                                          (Values XML-Token-Consumer XML-Scope (Option seed))))
  (lambda [/dev/xmlin consume scope saxcb datum0]
    (define-values (self consume++ scope++) (xml-consume-token /dev/xmlin consume scope))
    
    (cond [(eof-object? self) (values consume++ scope++ datum0)]
          [else (let* ([?tagname (and (symbol? self) self)]
                       [felement (xml-event-handler-element saxcb)]
                       [datum (and ?tagname datum0 (felement ?tagname (assert scope++ index?) 'open datum0))])
                  ; broken start tag should not affect its parent and sibling elements
                  (let-values ([(empty? consume++++ scope++++ ?datum) (xml-sax-element-attributes /dev/xmlin consume++ scope++ saxcb ?tagname datum)])
                    (cond [(and empty?) (values consume++++ scope++++ (and ?tagname ?datum (felement ?tagname (assert scope++++ index?) 'close-empty ?datum)))]
                          [else (xml-sax-subelement ?tagname /dev/xmlin consume++++ scope++++ saxcb
                                                    (and ?tagname ?datum (felement ?tagname (assert scope++++ index?) 'close-tag ?datum)))])))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-sax-element-attributes : (All (seed) (-> Input-Port XML-Token-Consumer XML-Scope (XML-Event-Handler seed) (Option Symbol) (Option seed)
                                                     (Values Boolean XML-Token-Consumer XML-Scope (Option seed))))
  (lambda [/dev/xmlin consume scope saxcb ?tagname datum0]
    (define fattribute : (XML-Attribute-Handler seed) (xml-event-handler-attribute saxcb))
    (define fattrilist : (XML-AttriList-Handler seed) (xml-event-handler-attrilist saxcb))
    
    (let sax-element-attributes ([consume : XML-Token-Consumer consume]
                                 [scope : XML-Scope scope]
                                 [setubirtta : (Listof XML-Element-Attribute) null]
                                 [datum : (Option seed) datum0])
      (define-values (self consume++ scope++) (xml-consume-token /dev/xmlin consume scope))
      
      (cond [(eof-object? self) (values #false consume++ scope++ datum0)]
            [(eq? self />) (values #true consume++ scope++ (and ?tagname datum (fattrilist ?tagname (reverse setubirtta) datum)))]
            [(eq? self stag>) (values #false consume++ scope++ (and ?tagname datum (fattrilist ?tagname (reverse setubirtta) datum)))]
            [(not (symbol? self)) #| missing name |# (sax-element-attributes consume++ scope++ setubirtta datum)]
            [else (let*-values ([(?eq consume++++ scope++++) (xml-consume-token /dev/xmlin consume++ scope++)]
                                [(?value consume** scope**) (xml-consume-token /dev/xmlin consume++++ scope++++)])
                    (cond [(eof-object? ?value) (sax-element-attributes consume** scope** setubirtta datum)] ; Maybe Typed Racket is buggy here
                          [(not (and (eq? ?eq #\=) (xml-value-string? ?value))) (sax-element-attributes consume** scope** setubirtta datum)]
                          [else (sax-element-attributes consume** scope** (cons (cons self ?value) setubirtta)
                                                        (and ?tagname datum (fattribute ?tagname self ?value datum)))]))]))))

(define xml-sax-subelement : (All (seed) (-> (Option Symbol) Input-Port XML-Token-Consumer XML-Scope (XML-Event-Handler seed) (Option seed)
                                             (Values XML-Token-Consumer XML-Scope (Option seed))))
  (lambda [tagname /dev/xmlin consume scope saxcb datum0]
    (define felement : (XML-Element-Handler seed) (xml-event-handler-element saxcb))
    (define fcomment : (XML-Comment-Handler seed) (xml-event-handler-comment saxcb))
    (define fpcdata : (XML-PCData-Handler seed) (xml-event-handler-pcdata saxcb))
    (define fspace : (XML-Space-Handler seed) (xml-event-handler-space saxcb))
    (define fgeref : (XML-GEReference-Handler seed) (xml-event-handler-geref saxcb))
    
    (let sax-subelement ([consume : XML-Token-Consumer consume]
                         [scope : XML-Scope scope]
                         [datum : (Option seed) datum0])
      (define-values (self consume++ scope++) (xml-consume-token /dev/xmlin consume scope))
      
      (cond [(eof-object? rest) (values consume++ scope++ datum0)]
            [(xml-comment? self) (sax-subelement consume++ scope++ (and datum (fcomment tagname (xml-white-space-raw self) datum)))]
            [(xml-white-space? self)
             (sax-subelement consume++ scope++
                             (and tagname datum
                                  (fspace tagname (xml-white-space-raw self) (xml-new-line? self) datum)))]
            [(eq? self #\<)
             (let-values ([(consume++++ scope++++ datum++) (xml-sax-element /dev/xmlin consume++ scope++ saxcb datum)])
               (sax-subelement consume++++ scope++++ datum++))]
            [(string? self) (sax-subelement consume++ scope++ (and tagname datum (fpcdata tagname self #false datum)))]
            [(eq? self </)
             (let*-values ([(?name consume++++ scope++++) (xml-consume-token /dev/xmlin consume++ scope++)]
                           [(?etag consume** scope**) (xml-consume-token /dev/xmlin consume++++ scope++++)])
               (values consume** scope**
                       (and datum (eq? tagname ?name)
                            (symbol? ?name) (xml-etag? ?etag) 
                            (felement ?name (assert scope++ index?) 'close datum))))]
            [(eq? self <?)
             (let-values ([(p consume++++ scope++++) (xml-sax-pi /dev/xmlin consume++ scope++)])
               (sax-subelement consume++++ scope++++ datum))]
            [(eq? self <!&CDATA&)
             (let*-values ([(?cdata consume++++ scope++++) (xml-consume-token /dev/xmlin consume++ scope++)]
                           [(?ecdata consume** scope**) (xml-consume-token /dev/xmlin consume++++ scope++++)])
               (cond [(not (string? ?cdata)) (sax-subelement consume** scope** #false)]
                     [else (sax-subelement consume** scope** (and tagname datum (fpcdata tagname ?cdata #true datum)))]))]
            [(index? self) (sax-subelement consume++ scope++ (and tagname datum (fgeref self (integer->char self) datum)))]
            [(symbol? self) (sax-subelement consume++ scope++ (and tagname datum (fgeref self (xml-prentity-value-ref self) datum)))]
            [else #| should not happen |# (sax-subelement consume++ scope++ datum)]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define sax-arity3-identity : (All (seed) (-> Any Any seed seed))
  (lambda [a1 a2 datum]
    datum))

(define sax-arity4-identity : (All (seed) (-> Any Any Any seed seed))
  (lambda [a1 a2 a3 datum]
    datum))

(define sax-arity5-identity : (All (seed) (-> Any Any Any Any seed seed))
  (lambda [a1 a2 a3 a4 datum]
    datum))

(define sax-arity6-identity : (All (seed) (-> Any Any Any Any Any seed seed))
  (lambda [a1 a2 a3 a4 a5 datum]
    datum))
