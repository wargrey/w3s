#lang typed/racket/base

(provide (all-defined-out))

(require racket/list)

(require "whitespace.rkt")

(require "../tokenizer/port.rkt")
(require "../tokenizer/delimiter.rkt")

(require "../plain/grammar.rkt")
(require "../plain/prompt.rkt")

(require "../misc.rkt")
(require "../stdin.rkt")
(require "../prentity.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type XML-Event-Handler (XML-Event-Handlerof Void))
(define-type SAX-Attribute-Value (U String (Boxof String)))
(define-type SAX-Attributes (Listof (Pairof Symbol SAX-Attribute-Value)))

(define-type (XML-Prolog-Handler seed) (-> (U String Symbol) Nonnegative-Flonum (Option String) Boolean Boolean seed seed))
(define-type (XML-Doctype-Handler seed) (-> (Option Symbol) (Option String) (Option String) seed seed))
(define-type (XML-PI-Handler seed) (-> (Option Symbol) Symbol (Option String) seed seed))
(define-type (XML-Element-Handler seed) (-> Symbol Index (Option SAX-Attributes) Boolean seed seed))
(define-type (XML-PCData-Handler seed) (-> Symbol String Boolean seed seed))
(define-type (XML-GEReference-Handler seed) (-> (U Symbol Index) (Option Char) seed seed))
(define-type (XML-Comment-Handler seed) (-> (Option Symbol) String seed seed))

(struct (seed) xml-event-handler
  ([prolog : (XML-Prolog-Handler seed)]
   [doctype : (XML-Doctype-Handler seed)]
   [pi : (XML-PI-Handler seed)]
   [element : (XML-Element-Handler seed)]
   [pcdata : (XML-PCData-Handler seed)]
   [geref : (XML-GEReference-Handler seed)]
   [comment : (XML-Comment-Handler seed)])
  #:type-name XML-Event-Handlerof
  #:transparent)

(define #:forall (seed) (make-xml-event-handler
                         #:prolog [document : (U (XML-Prolog-Handler seed) False Void) (void)]
                         #:doctype [doctype : (U (XML-Doctype-Handler seed) False Void) (void)]
                         #:pi [pi : (U (XML-PI-Handler seed) False Void) (void)]
                         #:element [element : (U (XML-Element-Handler seed) False Void) (void)]
                         #:pcdata [pcdata : (U (XML-PCData-Handler seed) False Void) (void)]
                         #:gereference [geref : (U (XML-GEReference-Handler seed) False Void) (void)]
                         #:comment [comment : (U (XML-Comment-Handler seed) False Void) (void)]
                         [src : (Option (XML-Event-Handlerof seed)) #false]) : (XML-Event-Handlerof seed)
  (xml-event-handler (or (if (void? document) (and src (xml-event-handler-prolog src)) document) sax-arity6-identity)
                     (or (if (void? doctype) (and src (xml-event-handler-doctype src)) doctype) sax-arity4-identity)
                     (or (if (void? pi) (and src (xml-event-handler-pi src)) pi) sax-arity4-identity)
                     (or (if (void? element) (and src (xml-event-handler-element src)) element) sax-arity5-identity)
                     (or (if (void? pcdata) (and src (xml-event-handler-pcdata src)) pcdata) sax-arity4-identity)
                     (or (if (void? geref) (and src (xml-event-handler-geref src)) geref) sax-arity3-identity)
                     (or (if (void? comment) (and src (xml-event-handler-comment src)) comment) sax-arity3-identity)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (seed) load-xml-datum : (->* (SGML-StdIn XML-Event-Handler)
                                              (#:xml:lang String #:xml:space Symbol #:xml:space-filter (Option XML:Space-Filter))
                                              Void)
  (lambda [#:xml:lang [xml:lang ""] #:xml:space [xml:space 'default] #:xml:space-filter [xml:filter #false]
           /dev/rawin saxcb]
    (read-xml-datum #:xml:lang xml:lang #:xml:space xml:space #:xml:space-filter xml:filter
                    /dev/rawin saxcb (void))))

(define #:forall (seed) read-xml-datum : (->* (SGML-StdIn (XML-Event-Handlerof seed) seed)
                                              (#:xml:lang String #:xml:space Symbol #:xml:space-filter (Option XML:Space-Filter))
                                              seed)
  (lambda [#:xml:lang [xml:lang ""] #:xml:space [xml:space 'default] #:xml:space-filter [xml:filter #false]
           /dev/rawin saxcb datum0]
    (parameterize ([current-custodian (make-custodian)]
                   [default-xml:space-signal xml:space])
      (define-values (/dev/xmlin version encoding standalone?) (xml-open-input-port /dev/rawin #false))
      (define pname : (U String Symbol) (sgml-port-name /dev/xmlin))
      (define datum : (U seed exn)
        (sax-start (if (string? pname) (string->symbol pname) pname)
                   (λ [] (with-handlers ([exn? (λ [[e : exn]] e)])
                           (xml-sax /dev/xmlin pname version encoding standalone? saxcb datum0
                                    (xml:lang-ref xml:lang) xml:space xml:filter)))))
      (custodian-shutdown-all (current-custodian))
      (cond [(exn? datum) (raise datum)]
            [else datum]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (seed) xml-sax : (-> Input-Port (U String Symbol) Nonnegative-Flonum (Option String) Boolean (XML-Event-Handlerof seed) seed
                                      (Option String) Symbol (Option XML:Space-Filter) seed)
  (lambda [/dev/xmlin pname version encoding standalone? saxcb datum0 xml:lang xml:space xml:filter]
    (define fprolog : (XML-Prolog-Handler seed) (xml-event-handler-prolog saxcb))
    (define fdoctype : (XML-Doctype-Handler seed) (xml-event-handler-doctype saxcb))
    (define fprocess : (XML-PI-Handler seed) (xml-event-handler-pi saxcb))
    (define fcomment : (XML-Comment-Handler seed) (xml-event-handler-comment saxcb))
    
    (let sax ([consume : XML-Token-Consumer xml-consume-token:*]
              [scope : XML-Scope xml-initial-scope]
              [datum : seed (fprolog pname version encoding standalone? #true datum0)])
      (let-values ([(self consume++ scope++) (xml-consume-token /dev/xmlin consume scope)])
        (cond [(eof-object? self) (fprolog pname version encoding standalone? #false datum)]
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
               (let-values ([(consume++++ scope++++ datum++) (xml-sax-element /dev/xmlin consume++ scope++ saxcb datum xml:lang xml:space xml:filter)])
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

(define xml-sax-element : (All (seed) (-> Input-Port XML-Token-Consumer XML-Scope (XML-Event-Handlerof seed) (Option seed)
                                          (Option String) Symbol (Option XML:Space-Filter) (Values XML-Token-Consumer XML-Scope (Option seed))))
  (lambda [/dev/xmlin consume scope saxcb datum0 xml:lang xml:space xml:filter]
    (define-values (self consume++ scope++) (xml-consume-token /dev/xmlin consume scope))
    
    (cond [(eof-object? self) (values consume++ scope++ datum0)]
          [else (let* ([?tagname (and (symbol? self) self)]
                       [felement (xml-event-handler-element saxcb)])
                  ; broken start tag should not affect its parent and sibling elements
                  (let*-values ([(?attrs empty? consume++++ scope++++ lang space) (xml-sax-element-attributes /dev/xmlin consume++ scope++ xml:lang xml:space)]
                                [(datum) (and ?tagname ?attrs datum0 (felement ?tagname (assert scope++ index?) ?attrs empty? datum0))])
                    (cond [(not empty?) (xml-sax-subelement ?tagname /dev/xmlin consume++++ scope++++ saxcb datum lang space xml:filter)]
                          [else (values consume++++ scope++++ datum)])))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-sax-element-attributes : (-> Input-Port XML-Token-Consumer XML-Scope (Option String) Symbol
                                         (Values (Option (Listof (Pairof Symbol SAX-Attribute-Value))) Boolean XML-Token-Consumer XML-Scope (Option String) Symbol))
  (lambda [/dev/xmlin consume scope xml:lang xml:space]
    (let sax-element-attributes ([consume : XML-Token-Consumer consume]
                                 [scope : XML-Scope scope]
                                 [setubirtta : (Listof (Pairof Symbol SAX-Attribute-Value)) null]
                                 [lang : (Option String) xml:lang]
                                 [space : Symbol xml:space])
      (define-values (self consume++ scope++) (xml-consume-token /dev/xmlin consume scope))
      
      (cond [(eof-object? self) (values #false #false consume++ scope++ lang space)]
            [(eq? self />) (values (reverse setubirtta) #true consume++ scope++ lang space)]
            [(eq? self stag>) (values (reverse setubirtta) #false consume++ scope++ lang space)]
            [(not (symbol? self)) #| missing name |# (sax-element-attributes consume++ scope++ setubirtta lang space)]
            [else (let*-values ([(?eq consume++++ scope++++) (xml-consume-token /dev/xmlin consume++ scope++)]
                                [(?value consume** scope**) (xml-consume-token /dev/xmlin consume++++ scope++++)])
                    (cond [(eof-object? ?value) (sax-element-attributes consume** scope** setubirtta lang space)] ; Maybe Typed Racket is buggy here
                          [(not (and (eq? ?eq #\=) (xml-value-string? ?value))) (sax-element-attributes consume** scope** setubirtta lang space)]
                          [else (let ([setubirtta++ (cons (cons self ?value) setubirtta)])
                                  (case self
                                    [(xml:space) (sax-element-attributes consume** scope** setubirtta++ lang (xml:space-ref ?value space))]
                                    [(xml:lang) (sax-element-attributes consume** scope** setubirtta++ (xml:lang-ref '|| ?value) space)]
                                    [else (sax-element-attributes consume** scope** setubirtta++ lang space)]))]))]))))

(define xml-sax-subelement : (All (seed) (-> (Option Symbol) Input-Port XML-Token-Consumer XML-Scope (XML-Event-Handlerof seed) (Option seed)
                                             (Option String) Symbol (Option XML:Space-Filter) (Values XML-Token-Consumer XML-Scope (Option seed))))
  (lambda [tagname /dev/xmlin consume scope saxcb datum0 xml:lang xml:space xml:filter]
    (define felement : (XML-Element-Handler seed) (xml-event-handler-element saxcb))
    (define fcomment : (XML-Comment-Handler seed) (xml-event-handler-comment saxcb))
    (define fprocess : (XML-PI-Handler seed) (xml-event-handler-pi saxcb))
    (define fpcdata : (XML-PCData-Handler seed) (xml-event-handler-pcdata saxcb))
    (define fgeref : (XML-GEReference-Handler seed) (xml-event-handler-geref saxcb))
    
    (let sax-subelement ([consume : XML-Token-Consumer consume]
                         [scope : XML-Scope scope]
                         [datum : (Option seed) datum0]
                         [atadc : (Listof (U XML-White-Space String)) null])
      (define-values (self consume++ scope++) (xml-consume-token /dev/xmlin consume scope))
      
      (cond [(eof-object? rest) (values consume++ scope++ datum0)]
            [(xml-comment? self)
             (sax-subelement consume++ scope++
                             (and tagname datum
                                  (fcomment tagname (xml-white-space-raw self)
                                            (sax-cdata-process tagname atadc fpcdata datum)))
                             null #| comment itself is a kind of whitespace, thus, whitespaces before and after it should be consolidated altogether |#)]
            [(xml-white-space? self)
             (sax-subelement consume++ scope++ datum
                             (cond [(not (and datum tagname)) atadc]
                                   [(eq? xml:space 'preserve) (cons (xml:space=preserve tagname self xml:filter xml:lang) atadc)]
                                   [else (if (pair? atadc) (cons xml-whitespace/1 atadc) atadc)]))]
            [(eq? self #\<)
             (let*-values ([(pcdatum) (and tagname datum (sax-cdata-process tagname atadc fpcdata datum))]
                           [(consume++++ scope++++ datum++) (xml-sax-element /dev/xmlin consume++ scope++ saxcb pcdatum xml:lang xml:space xml:filter)])
               (sax-subelement consume++++ scope++++ (or datum++ pcdatum) xml-whitespaces/0))]
            [(string? self) (sax-subelement consume++ scope++ datum (if (and datum tagname) (cons self atadc) atadc))]
            [(eq? self </)
             (let*-values ([(pcdatum) (and tagname datum (sax-cdata-process tagname atadc fpcdata datum #true))]
                           [(?name consume++++ scope++++) (xml-consume-token /dev/xmlin consume++ scope++)]
                           [(?etag consume** scope**) (xml-consume-token /dev/xmlin consume++++ scope++++)])
               (values consume** scope**
                       (and pcdatum (eq? tagname ?name)
                            (symbol? ?name) (xml-etag? ?etag) 
                            (felement ?name (assert scope++ index?)
                                      #false #false pcdatum))))]
            [(eq? self <?)
             (let*-values ([(pcdatum) (and tagname datum (sax-cdata-process tagname atadc fpcdata datum))]
                           [(p consume++++ scope++++) (xml-sax-pi /dev/xmlin consume++ scope++)])
               (sax-subelement consume++++ scope++++
                               (if (mpair? p) (and tagname pcdatum (fprocess tagname (mcar p) (mcdr p) pcdatum)) pcdatum)
                               xml-whitespaces/0))]
            [(eq? self <!&CDATA&)
             (let*-values ([(pcdatum) (and tagname datum (sax-cdata-process tagname atadc fpcdata datum))]
                           [(?cdata consume++++ scope++++) (xml-consume-token /dev/xmlin consume++ scope++)]
                           [(?ecdata consume** scope**) (xml-consume-token /dev/xmlin consume++++ scope++++)])
               (cond [(not (string? ?cdata)) (sax-subelement consume** scope** pcdatum null)]
                     [else (sax-subelement consume** scope** (and tagname pcdatum (fpcdata tagname ?cdata #true pcdatum)) xml-whitespaces/0)]))]
            [(index? self)
             (sax-subelement consume++ scope++
                             (and tagname datum
                                  (fgeref self (integer->char self)
                                          (sax-cdata-process tagname atadc fpcdata datum)))
                             xml-whitespaces/0)]
            [(symbol? self)
             (sax-subelement consume++ scope++
                             (and tagname datum
                                  (fgeref self (xml-prentity-value-ref self)
                                          (sax-cdata-process tagname atadc fpcdata datum)))
                             xml-whitespaces/0)]
            [else #| deadcode |# (sax-subelement consume++ scope++ datum atadc)]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define sax-cdata-process : (All (seed) (->* (Symbol (Listof (U XML-White-Space String)) (XML-PCData-Handler seed) seed) (Boolean) seed))
  (lambda [tagname atadc fpcdata datum [tail? #false]]
    (cond [(null? atadc) datum]
          [else (let ([cdata/?1 (reverse (if (and tail? (eq? (car atadc) xml-whitespace/1)) (cdr atadc) atadc))])
                  (cond [(null? cdata/?1) datum]
                        [else (let ([cdata (if (eq? (car cdata/?1) xml-whitespace/0) (cdr cdata/?1) cdata/?1)])
                                (cond [(null? cdata) datum]
                                      [else (fpcdata tagname (apply string-append (map xml-cdata->datum cdata))
                                                     #false datum)]))]))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-whitespaces/0 : (Listof XML-White-Space) (list xml-whitespace/0))

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
