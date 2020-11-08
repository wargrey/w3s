#lang typed/racket/base

(provide (all-defined-out))

(require racket/list)

(require "../tokenizer/port.rkt")
(require "../tokenizer/delimiter.rkt")

(require "../plain/grammar.rkt")
(require "../plain/prompt.rkt")

(require "../stdin.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type XML-Element-Event (U 'open 'close))

(define-type (XML-Prolog-Handler seed) (-> (U String Symbol) Nonnegative-Flonum (Option String) Boolean XML-Element-Event seed seed))
(define-type (XML-Doctype-Handler seed) (-> (Option Symbol) (Option String) (Option String) seed seed))
(define-type (XML-PI-Handler seed) (-> (Option Symbol) Symbol (Option String) seed seed))
(define-type (XML-Element-Handler seed) (-> Symbol Index XML-Element-Event seed seed))
(define-type (XML-Attribute-Handler seed) (-> Symbol Symbol String seed seed))
(define-type (XML-AttrList-Handler seed) (-> Symbol (Listof (Pairof Symbol String)) seed seed))
(define-type (XML-PCData-Handler seed) (-> Symbol String Boolean seed seed))
(define-type (XML-Comment-Handler seed) (-> (Option Symbol) String seed seed))

(struct (seed) xml-event-handler
  ([prolog : (XML-Prolog-Handler seed)]
   [doctype : (XML-Doctype-Handler seed)]
   [pi : (XML-PI-Handler seed)]
   [element : (XML-Element-Handler seed)]
   [attribute : (XML-Attribute-Handler seed)]
   [attrlist : (XML-AttrList-Handler seed)]
   [pcdata : (XML-PCData-Handler seed)]
   [comment : (XML-Comment-Handler seed)])
  #:type-name XML-Event-Handler
  #:transparent)

(define #:forall (seed) (make-xml-event-handler
                         #:prolog [document : (U (XML-Prolog-Handler seed) False Void) (void)]
                         #:doctype [doctype : (U (XML-Doctype-Handler seed) False Void) (void)]
                         #:pi [pi : (U (XML-PI-Handler seed) False Void) (void)]
                         #:element [element : (U (XML-Element-Handler seed) False Void) (void)]
                         #:attribute [attr : (U (XML-Attribute-Handler seed) False Void) (void)]
                         #:attrlist [attrlist : (U (XML-AttrList-Handler seed) False Void) (void)]
                         #:pcdata [pcdata : (U (XML-PCData-Handler seed) False Void) (void)]
                         #:comment [comment : (U (XML-Comment-Handler seed) False Void) (void)]
                         [src : (Option (XML-Event-Handler seed)) #false]) : (XML-Event-Handler seed)
  (xml-event-handler (or (if (void? document) (and src (xml-event-handler-prolog src)) document) sax-arity6-identity)
                     (or (if (void? doctype) (and src (xml-event-handler-doctype src)) doctype) sax-arity4-identity)
                     (or (if (void? pi) (and src (xml-event-handler-pi src)) pi) sax-arity4-identity)
                     (or (if (void? element) (and src (xml-event-handler-element src)) element) sax-arity4-identity)
                     (or (if (void? attr) (and src (xml-event-handler-attribute src)) attr) sax-arity4-identity)
                     (or (if (void? attrlist) (and src (xml-event-handler-attrlist src)) attrlist) sax-arity3-identity)
                     (or (if (void? pcdata) (and src (xml-event-handler-pcdata src)) pcdata) sax-arity4-identity)
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
              [datum : seed (if (not fprolog) datum0 (fprolog pname version encoding standalone? 'open datum0))])
      (let-values ([(self consume++ scope++) (xml-consume-token /dev/xmlin consume scope)])
        (cond [(eof-object? self) (if (not fprolog) datum (fprolog pname version encoding standalone? 'close datum))]
              [(eq? self <!)
               (let-values ([(d consume++++ scope++++) (xml-sax-declaration /dev/xmlin consume++ scope++)])
                 (cond [(not d) (sax consume++++ scope++++ datum)]
                       [(eq? (vector-ref d 0) 'DOCTYPE)
                        (let-values ([(?name ?public ?system sPI) (xml-grammar-parse-doctype d)])
                          (sax consume++++ scope++++ (fdoctype ?name ?public ?system datum)))]
                       [else (sax consume++++ scope++++ datum)]))]
              [(eq? self <?)
               (let-values ([(p consume++++ scope++++) (xml-sax-pi /dev/xmlin consume++ scope++)])
                 (displayln p)
                 (sax consume++++ scope++++ (if (mpair? p) (fprocess #false (mcar p) (mcdr p) datum) datum)))]
              [(eq? self #\<)
               (let-values ([(e consume++++ scope++++) (xml-sax-element /dev/xmlin consume++ scope++)])
                 (sax consume++++ scope++++ (if (not e) datum datum)))]
              [(xml-comment? self) (sax consume++ scope++ (fcomment #false (xml-white-space-raw self) datum))]
              [else (sax consume++ scope++ datum)])))))

(define xml-sax-declaration : (-> Input-Port XML-Token-Consumer XML-Scope (Values (Option XML-Declaration) XML-Token-Consumer XML-Scope))
  (lambda [/dev/xmlin consume scope]
    (let sax-decl ([consume : XML-Token-Consumer consume]
                   [scope : XML-Scope scope]
                   [name : (Option Symbol) #false]
                   [bodies : (Listof XML-Doctype-Body) null])
      (define-values (self consume++ scope++) (xml-consume-token /dev/xmlin consume scope))
      
      (cond [(eof-object? self) (values #false consume++ scope++)]
            [(xml-white-space? self) (sax-decl consume++ scope++ name bodies)]
            [(eq? self #\>) (values (and name (vector name (reverse bodies))) consume++ scope++)]
            [(symbol? self) (if (not name) (sax-decl consume++ scope++ self bodies) (sax-decl consume++ scope++ name (cons self bodies)))]
            [(eq? self <!)
             (let-values ([(d consume++++ scope++++) (xml-sax-declaration /dev/xmlin consume++ scope++)])
               (sax-decl consume++++ scope++++ name (if (not d) bodies (cons d bodies))))]
            [(eq? self <?)
             (let-values ([(p consume++++ scope++++) (xml-sax-pi /dev/xmlin consume++ scope++)])
               (sax-decl consume++++ scope++++ name (if (not p) bodies (cons p bodies))))]
            [else (sax-decl consume++ scope++ name (cons self bodies))]))))

(define xml-sax-pi : (-> Input-Port XML-Token-Consumer XML-Scope (Values (Option XML-Processing-Instruction) XML-Token-Consumer XML-Scope))
  ;;; https://www.w3.org/TR/xml/#sec-pi
  (lambda [/dev/xmlin consume scope]
    (let sax-pi ([consume : XML-Token-Consumer consume]
                 [scope : XML-Scope scope]
                 [target : (Option Symbol) #false]
                 [body : (Option String) #false])
      (define-values (self consume++ scope++) (xml-consume-token /dev/xmlin consume scope))
      
      (cond [(eof-object? self) (values #false consume++ scope++)]
            [(symbol? self) (sax-pi consume++ scope++ self body)]
            [(string? self) (sax-pi consume++ scope++ target self)]
            [(eq? self ?>) (values (and target (mcons target body)) consume++ scope++)]
            [else (sax-pi consume++ scope++ target body)]))))

(define xml-sax-element : (-> Input-Port XML-Token-Consumer XML-Scope (Values (Option XML-Element) XML-Token-Consumer XML-Scope))
  (lambda [/dev/xmlin consume scope]
    (define-values (self consume++ scope++) (xml-consume-token /dev/xmlin consume scope))
    
    (cond [(eof-object? self) (values #false consume++ scope++)]
          [else (let ([tagname (and (symbol? self) self)])
                  ; broken start tag should not affect its parent and sibling elements
                  (let-values ([(attributes empty? consume++++ scope++++) (xml-sax-element-attributes /dev/xmlin consume++ scope++)])
                    (cond [(and empty?) (values (and tagname (list tagname attributes null)) consume++++ scope++++)]
                          [else (let-values ([(children consume* scope*) (xml-sax-subelement tagname /dev/xmlin consume++++ scope++++)])
                                  (values (and children tagname (list tagname attributes children)) consume* scope*))])))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-sax-element-attributes : (-> Input-Port XML-Token-Consumer XML-Scope (Values (Listof XML-Element-Attribute) Boolean XML-Token-Consumer XML-Scope))
  (lambda [/dev/xmlin consume scope]
    (let sax-element-attributes ([consume : XML-Token-Consumer consume]
                                 [scope : XML-Scope scope]
                                 [setubirtta : (Listof XML-Element-Attribute) null])
      (define-values (self consume++ scope++) (xml-consume-token /dev/xmlin consume scope))
      
      (cond [(eof-object? self) (values setubirtta #false consume++ scope++)]
            [(eq? self />) (values (reverse setubirtta) #true consume++ scope++)]
            [(eq? self stag>) (values (reverse setubirtta) #false consume++ scope++)]
            [(not (symbol? self)) #| missing name |# (sax-element-attributes consume++ scope++ setubirtta)]
            [else (let*-values ([(?eq consume++++ scope++++) (xml-consume-token /dev/xmlin consume++ scope++)]
                                [(?value consume** scope**) (xml-consume-token /dev/xmlin consume++++ scope++++)])
                    (cond [(eof-object? ?value) (sax-element-attributes consume** scope** setubirtta)] ; Maybe Typed Racket is buggy here
                          [(and (eq? ?eq #\=) (xml-value-string? ?value)) (sax-element-attributes consume** scope** (cons (cons self ?value) setubirtta))]
                          [else (sax-element-attributes consume** scope** setubirtta)]))]))))

(define xml-sax-subelement : (-> (Option Symbol) Input-Port XML-Token-Consumer XML-Scope
                                 (Values (Option (Listof (U XML-Element XML-Subdatum))) XML-Token-Consumer XML-Scope))
  (lambda [tagname /dev/xmlin consume scope]
    (let extract-subelement ([consume : XML-Token-Consumer consume]
                             [scope : XML-Scope scope]
                             [nerdlidc : (Listof (U XML-Element XML-Subdatum)) null])
      (define-values (self consume++ scope++) (xml-consume-token /dev/xmlin consume scope))
      
      (cond [(eof-object? rest) (values #false consume++ scope++)]
            [(xml-white-space? self) (extract-subelement consume++ scope++ (cons self nerdlidc))]
            [(eq? self #\<)
             (let-values ([(e consume++++ scope++++) (xml-sax-element /dev/xmlin consume++ scope++)])
               (extract-subelement consume++++ scope++++ (if (not e) nerdlidc (cons e nerdlidc))))]
            [(string? self) (extract-subelement consume++ scope++ (cons self nerdlidc))]
            [(eq? self </)
             (let*-values ([(?name consume++++ scope++++) (xml-consume-token /dev/xmlin consume++ scope++)]
                           [(?etag consume** scope**) (xml-consume-token /dev/xmlin consume++++ scope++++)])
               (values (and (symbol? ?name) (xml-etag? ?etag) (eq? tagname ?name) (reverse nerdlidc))
                       consume** scope**))]
            [(or (index? self) (symbol? self)) (extract-subelement consume++ scope++ (cons self nerdlidc))]  ; entities
            [(eq? self <?)
             (let-values ([(p consume++++ scope++++) (xml-sax-pi /dev/xmlin consume++ scope++)])
               (extract-subelement consume++++ scope++++ (if (not p) nerdlidc (cons p nerdlidc))))]
            [(eq? self <!&CDATA&)
             (let*-values ([(?cdata consume++++ scope++++) (xml-consume-token /dev/xmlin consume++ scope++)]
                           [(?ecdata consume** scope**) (xml-consume-token /dev/xmlin consume++++ scope++++)])
               (cond [(eof-object? ?cdata) (extract-subelement consume** scope** null)]
                     [else (extract-subelement consume** scope** (cons (assert ?cdata string?) nerdlidc))]))]
            [else #| should not happen |# (extract-subelement consume++ scope++ nerdlidc)]))))

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
