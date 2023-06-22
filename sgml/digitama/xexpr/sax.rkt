#lang typed/racket/base

(provide (all-defined-out))

(require racket/list)

(require "whitespace.rkt")
(require "grammar.rkt")
(require "prompt.rkt")

(require "../tokenizer/port.rkt")
(require "../tokenizer/delimiter.rkt")

(require "../misc.rkt")
(require "../stdin.rkt")
(require "../prentity.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type XML-Event-Handler (XML-Event-Handlerof Void))
(define-type SAX-Attribute-Value (U String (Boxof String)))
(define-type SAX-Attributes (Listof (Pairof Symbol SAX-Attribute-Value)))

;;; NOTE: K.I.S.S.
(define-type (XML-Prolog-Handler seed) (-> (U String Symbol) Nonnegative-Flonum (Option String) Boolean Boolean seed (Option seed)))
(define-type (XML-Doctype-Handler seed) (-> (Option Symbol) (Option String) (Option String) seed (Option seed)))
(define-type (XML-PI-Handler seed) (-> (Listof Symbol) Symbol (Option String) seed (Option seed)))
(define-type (XML-Element-Handler seed) (-> Symbol (Listof Symbol) (Option SAX-Attributes #| `False` implies ETag |#) Boolean Boolean seed (Option seed)))
(define-type (XML-PCData-Handler seed) (-> Symbol (Listof Symbol) String Boolean #| <= preserve |# Boolean #| <= `<![CDATA[]]>` |# seed (Option seed)))
(define-type (XML-GEReference-Handler seed) (-> (U Symbol Index) (Option Char) seed (Option seed)))
(define-type (XML-Comment-Handler seed) (-> (Listof Symbol) String Boolean seed (Option seed)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-sax-event-prefilter : (Parameterof (Option XML-Event-Handler)) (make-parameter #false))
(define default-sax-event-postfilter : (Parameterof (Option XML-Event-Handler)) (make-parameter #false))

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
  (xml-event-handler (or (if (void? document) (and src (xml-event-handler-prolog src)) document) sax-identity/6)
                     (or (if (void? doctype) (and src (xml-event-handler-doctype src)) doctype) sax-identity/4)
                     (or (if (void? pi) (and src (xml-event-handler-pi src)) pi) sax-identity/4)
                     (or (if (void? element) (and src (xml-event-handler-element src)) element) sax-identity/6)
                     (or (if (void? pcdata) (and src (xml-event-handler-pcdata src)) pcdata) sax-identity/6)
                     (or (if (void? geref) (and src (xml-event-handler-geref src)) geref) sax-identity/3)
                     (or (if (void? comment) (and src (xml-event-handler-comment src)) comment) sax-identity/4)))

(define #:forall (seed) make-xml-event-handler/fold : (-> (Listof (XML-Event-Handlerof seed)) (XML-Event-Handlerof seed))
  (lambda [handlers]
    (xml-event-handler
     (λ [[pname : (U String Symbol)] [version : Nonnegative-Flonum] [encoding : (Option String)] [standalone? : Boolean] [etype? : Boolean] [datum0 : seed]] : (Option seed)
       (let fold-chain ([datum : seed datum0]
                        [handlers : (Listof (XML-Event-Handlerof seed)) handlers])
         (cond [(null? handlers) datum]
               [else (let ([datum++ ((xml-event-handler-prolog (car handlers)) pname version encoding standalone? etype? datum)])
                       (fold-chain (or datum++ datum) (cdr handlers)))])))
     (λ [[?name : (Option Symbol)] [public : (Option String)] [system : (Option String)] [datum0 : seed]] : (Option seed)
       (let fold-chain ([datum : seed datum0]
                        [handlers : (Listof (XML-Event-Handlerof seed)) handlers])
         (cond [(null? handlers) datum]
               [else (let ([datum++ ((xml-event-handler-doctype (car handlers)) ?name public system datum)])
                       (fold-chain (or datum++ datum) (cdr handlers)))])))
     (λ [[xpath : (Listof Symbol)] [target : Symbol] [body : (Option String)] [datum0 : seed]] : (Option seed)
       (let fold-chain ([datum : seed datum0]
                        [handlers : (Listof (XML-Event-Handlerof seed)) handlers])
         (cond [(null? handlers) datum]
               [else (let ([datum++ ((xml-event-handler-pi (car handlers)) xpath target body datum)])
                       (fold-chain (or datum++ datum) (cdr handlers)))])))
     (λ [[name : Symbol] [xpath : (Listof Symbol)] [attrs : (Option SAX-Attributes)] [empty? : Boolean] [preserve? : Boolean] [datum0 : seed]] : (Option seed)
       (let fold-chain ([datum : seed datum0]
                        [handlers : (Listof (XML-Event-Handlerof seed)) handlers])
         (cond [(null? handlers) datum]
               [else (let ([datum++ ((xml-event-handler-element (car handlers)) name xpath attrs empty? preserve? datum)])
                       (fold-chain (or datum++ datum) (cdr handlers)))])))
     (λ [[element : Symbol] [xpath : (Listof Symbol)] [pcdata : String] [preserve? : Boolean] [cdata? : Boolean] [datum0 : seed]] : (Option seed)
       (let fold-chain ([datum : seed datum0]
                        [handlers : (Listof (XML-Event-Handlerof seed)) handlers])
         (cond [(null? handlers) datum]
               [else (let ([datum++ ((xml-event-handler-pcdata (car handlers)) element xpath pcdata preserve? cdata? datum)])
                       (fold-chain (or datum++ datum) (cdr handlers)))])))
     (λ [[entity : (U Symbol Index)] [?default-char : (Option Char)] [datum0 : seed]] : (Option seed)
       (let fold-chain ([datum : seed datum0]
                        [handlers : (Listof (XML-Event-Handlerof seed)) handlers])
         (cond [(null? handlers) datum]
               [else (let ([datum++ ((xml-event-handler-geref (car handlers)) entity ?default-char datum)])
                       (fold-chain (or datum++ datum) (cdr handlers)))])))
     (λ [[xpath : (Listof Symbol)] [comment : String] [preserve? : Boolean] [datum0 : seed]] : (Option seed)
       (let fold-chain ([datum : seed datum0]
                        [handlers : (Listof (XML-Event-Handlerof seed)) handlers])
         (cond [(null? handlers) datum]
               [else (let ([datum++ ((xml-event-handler-comment (car handlers)) xpath comment preserve? datum)])
                       (fold-chain (or datum++ datum) (cdr handlers)))]))))))

(define #:forall (seed) make-xml-event-handler/filter : (->* ((XML-Event-Handlerof seed))
                                                             (#:prefilter (Option XML-Event-Handler) #:postfilter (Option XML-Event-Handler))
                                                             (XML-Event-Handlerof seed))
  (lambda [handler #:prefilter [prefilter #false] #:postfilter [postfilter #false]]
    (if (or prefilter postfilter)
        (xml-event-handler
         (λ [[pname : (U String Symbol)] [version : Nonnegative-Flonum] [encoding : (Option String)] [standalone? : Boolean] [etype? : Boolean] [datum0 : seed]] : (Option seed)
           (when (and prefilter) ((xml-event-handler-prolog prefilter) pname version encoding standalone? etype? (void)))
           (define datum : (Option seed) ((xml-event-handler-prolog handler) pname version encoding standalone? etype? datum0))
           (when (and (not datum) postfilter) ((xml-event-handler-prolog postfilter) pname version encoding standalone? etype? (void)))
           datum)
         (λ [[?name : (Option Symbol)] [public : (Option String)] [system : (Option String)] [datum0 : seed]] : (Option seed)
           (when (and prefilter) ((xml-event-handler-doctype prefilter) ?name public system (void)))
           (define datum : (Option seed) ((xml-event-handler-doctype handler) ?name public system datum0))
           (when (and (not datum) postfilter) ((xml-event-handler-doctype postfilter) ?name public system (void)))
           datum)
         (λ [[xpath : (Listof Symbol)] [target : Symbol] [body : (Option String)] [datum0 : seed]] : (Option seed)
           (when (and prefilter) ((xml-event-handler-pi prefilter) xpath target body (void)))
           (define datum : (Option seed) ((xml-event-handler-pi handler) xpath target body datum0))
           (when (and (not datum) postfilter) ((xml-event-handler-pi postfilter) xpath target body (void)))
           datum)
         (λ [[name : Symbol] [xpath : (Listof Symbol)] [attrs : (Option SAX-Attributes)] [empty? : Boolean] [preserve? : Boolean] [datum0 : seed]] : (Option seed)
           (when (and prefilter) ((xml-event-handler-element prefilter) name xpath attrs empty? preserve? (void)))
           (define datum : (Option seed) ((xml-event-handler-element handler) name xpath attrs empty? preserve? datum0))
           (when (and (not datum) postfilter) ((xml-event-handler-element postfilter) name xpath attrs empty? preserve? (void)))
           datum)
         (λ [[element : Symbol] [xpath : (Listof Symbol)] [pcdata : String] [preserve? : Boolean] [cdata? : Boolean] [datum0 : seed]] : (Option seed)
           (when (and prefilter) ((xml-event-handler-pcdata prefilter) element xpath pcdata preserve? cdata? (void)))
           (define datum : (Option seed) ((xml-event-handler-pcdata handler) element xpath pcdata preserve? cdata? datum0))
           (when (and (not datum) postfilter) ((xml-event-handler-pcdata postfilter) element xpath pcdata preserve? cdata? (void)))
           datum)
         (λ [[entity : (U Symbol Index)] [?default-char : (Option Char)] [datum0 : seed]] : (Option seed)
           (when (and prefilter) ((xml-event-handler-geref prefilter) entity ?default-char (void)))
           (define datum : (Option seed) ((xml-event-handler-geref handler) entity ?default-char datum0))
           (when (and (not datum) postfilter) ((xml-event-handler-geref postfilter) entity ?default-char (void)))
           datum)
         (λ [[xpath : (Listof Symbol)] [comment : String] [preserve? : Boolean] [datum0 : seed]] : (Option seed)
           (when (and prefilter) ((xml-event-handler-comment prefilter) xpath comment preserve? (void)))
           (define datum : (Option seed) ((xml-event-handler-comment handler) xpath comment preserve? datum0))
           (when (and (not datum) postfilter) ((xml-event-handler-comment postfilter) xpath comment preserve? (void)))
           datum))
        handler)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (seed) load-xml-datum : (->* (SGML-Stdin (XML-Event-Handlerof (U Void seed)))
                                              (#:xml:lang String #:xml:space Symbol #:xml:space-filter (Option XML:Space-Filter))
                                              Void)
  (lambda [#:xml:lang [xml:lang ""] #:xml:space [xml:space 'default] #:xml:space-filter [xml:filter #false]
           /dev/rawin saxcb]
    (read-xml-datum* #:xml:lang xml:lang #:xml:space xml:space #:xml:space-filter xml:filter
                     /dev/rawin saxcb (void) void)))

(define #:forall (seed datum) read-xml-datum* : (->* (SGML-Stdin (XML-Event-Handlerof seed) seed (-> seed datum))
                                                     (#:xml:lang String #:xml:space Symbol #:xml:space-filter (Option XML:Space-Filter))
                                                     datum)
  (lambda [#:xml:lang [xml:lang ""] #:xml:space [xml:space 'default] #:xml:space-filter [xml:filter #false]
           /dev/rawin saxcb seed0 datum-unbox]
    (datum-unbox
     (read-xml-datum #:xml:lang xml:lang #:xml:space xml:space #:xml:space-filter xml:filter
                     /dev/rawin saxcb seed0))))

(define #:forall (seed) read-xml-datum : (->* (SGML-Stdin (XML-Event-Handlerof seed) seed)
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
                           (xml-sax /dev/xmlin pname version encoding standalone?
                                    (make-xml-event-handler/filter #:prefilter (default-sax-event-prefilter)
                                                                   #:postfilter (default-sax-event-postfilter)
                                                                   saxcb)
                                    datum0 (xml:lang-ref xml:lang) xml:space xml:filter)))))

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
              [datum : seed (or (fprolog pname version encoding standalone? #true datum0) datum0)])
      (let-values ([(self consume++ scope++) (xml-consume-token /dev/xmlin consume scope)])
        (cond [(eof-object? self) (or (fprolog pname version encoding standalone? #false datum) datum)]
              [(eq? self <!)
               (let-values ([(d consume++++ scope++++) (xml-sax-declaration /dev/xmlin consume++ scope++)])
                 (cond [(not d) (sax consume++++ scope++++ datum)]
                       [(eq? (vector-ref d 0) 'DOCTYPE)
                        (let-values ([(?name ?public ?system sPI) (xml-grammar-parse-doctype d)])
                          (sax consume++++ scope++++ (or (fdoctype ?name ?public ?system datum) datum)))]
                       [else (sax consume++++ scope++++ datum)]))]
              [(eq? self <?)
               (let-values ([(p consume++++ scope++++) (xml-sax-pi /dev/xmlin consume++ scope++)])
                 (sax consume++++ scope++++ (or (and (mpair? p) (fprocess null (mcar p) (mcdr p) datum)) datum)))]
              [(eq? self #\<)
               (let-values ([(consume++++ scope++++ datum++) (xml-sax-element /dev/xmlin consume++ scope++ null saxcb datum xml:lang xml:space xml:filter)])
                 (sax consume++++ scope++++ (or datum++ datum)))]
              [(xml-comment? self) (sax consume++ scope++ (or (fcomment null (xml-white-space-raw self) (eq? xml:space 'preserve) datum) datum))]
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

(define xml-sax-element : (All (seed) (-> Input-Port XML-Token-Consumer XML-Scope (Listof Symbol) (XML-Event-Handlerof seed) (Option seed)
                                          (Option String) Symbol (Option XML:Space-Filter)
                                          (Values XML-Token-Consumer XML-Scope (Option seed))))
  (lambda [/dev/xmlin consume scope xpath saxcb datum0 xml:lang xml:space xml:filter]
    (define-values (self consume++ scope++) (xml-consume-token /dev/xmlin consume scope))
    
    (cond [(eof-object? self) (values consume++ scope++ datum0)]
          [else (let* ([?tagname (and (symbol? self) self)]
                       [felement (xml-event-handler-element saxcb)])
                  ; broken start tag should not affect its parent and sibling elements
                  (let*-values ([(?attrs empty? consume++++ scope++++ lang space) (xml-sax-element-attributes /dev/xmlin consume++ scope++ xml:lang xml:space)]
                                [(datum) (and ?tagname ?attrs datum0 (or (felement ?tagname xpath ?attrs empty? (eq? space 'preserve) datum0) datum0))])
                    (cond [(not empty?) (xml-sax-subelement ?tagname /dev/xmlin consume++++ scope++++ xpath saxcb datum lang space xml:filter)]
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

(define xml-sax-subelement : (All (seed) (-> (Option Symbol) Input-Port XML-Token-Consumer XML-Scope (Listof Symbol) (XML-Event-Handlerof seed) (Option seed)
                                             (Option String) Symbol (Option XML:Space-Filter)
                                             (Values XML-Token-Consumer XML-Scope (Option seed))))
  (lambda [tagname /dev/xmlin consume scope xpath saxcb datum0 xml:lang xml:space xml:filter]
    (define felement : (XML-Element-Handler seed) (xml-event-handler-element saxcb))
    (define fcomment : (XML-Comment-Handler seed) (xml-event-handler-comment saxcb))
    (define fprocess : (XML-PI-Handler seed) (xml-event-handler-pi saxcb))
    (define fpcdata : (XML-PCData-Handler seed) (xml-event-handler-pcdata saxcb))
    (define fgeref : (XML-GEReference-Handler seed) (xml-event-handler-geref saxcb))
    (define subpath : (Listof Symbol) (if tagname (cons tagname xpath) xpath))
    (define preserve? : Boolean (eq? xml:space 'preserve))
    
    (let sax-subelement ([consume : XML-Token-Consumer consume]
                         [scope : XML-Scope scope]
                         [datum : (Option seed) datum0]
                         [atadc : (Listof (U XML-White-Space String)) null])
      (define-values (self consume++ scope++) (xml-consume-token /dev/xmlin consume scope))
      
      (cond [(eof-object? rest) (values consume++ scope++ datum)]
            [(xml-comment? self) ;; TODO: dealing with newlines
             (sax-subelement consume++ scope++
                             (and tagname datum
                                  (let ([datum++ (sax-cdata-process tagname atadc fpcdata xpath preserve? datum)])
                                    (or (fcomment subpath (xml-white-space-raw self) preserve? datum++) datum++)))
                             null #| comment itself is a kind of whitespace, thus whitespaces surrounded it should be consolidated altogether |#)]
            [(xml-white-space? self)
             (sax-subelement consume++ scope++ datum
                             (cond [(not (and datum tagname)) atadc]
                                   [(or preserve?) (cons (xml:space=preserve tagname self xml:filter xml:lang) atadc)]
                                   [else (if (pair? atadc) (cons xml-whitespace/1 atadc) atadc)]))]
            [(eq? self #\<)
             (let*-values ([(pcdatum) (and tagname datum (sax-cdata-process tagname atadc fpcdata xpath preserve? datum))]
                           [(consume++++ scope++++ datum++) (xml-sax-element /dev/xmlin consume++ scope++ subpath saxcb pcdatum xml:lang xml:space xml:filter)])
               (sax-subelement consume++++ scope++++ (or datum++ pcdatum) xml-whitespaces/0))]
            [(string? self) (sax-subelement consume++ scope++ datum (if (and datum tagname) (cons self atadc) atadc))]
            [(eq? self </)
             (let*-values ([(pcdatum) (and tagname datum (sax-cdata-process tagname atadc fpcdata xpath preserve? datum #true))]
                           [(?name consume++++ scope++++) (xml-consume-token /dev/xmlin consume++ scope++)]
                           [(?etag consume** scope**) (xml-consume-token /dev/xmlin consume++++ scope++++)])
               (values consume** scope**
                       (and pcdatum (eq? tagname ?name)
                            (symbol? ?name) (xml-etag? ?etag) 
                            (or (felement ?name xpath #false #false preserve? pcdatum) pcdatum))))]
            [(eq? self <?)
             (let*-values ([(pcdatum) (and tagname datum (sax-cdata-process tagname atadc fpcdata xpath preserve? datum))]
                           [(p consume++++ scope++++) (xml-sax-pi /dev/xmlin consume++ scope++)])
               (sax-subelement consume++++ scope++++
                               (cond [(mpair? p) (and pcdatum (or (fprocess subpath (mcar p) (mcdr p) pcdatum) pcdatum))]
                                     [else pcdatum])
                               xml-whitespaces/0))]
            [(eq? self <!&CDATA&)
             (let*-values ([(pcdatum) (and tagname datum (sax-cdata-process tagname atadc fpcdata xpath preserve? datum))]
                           [(?cdata consume++++ scope++++) (xml-consume-token /dev/xmlin consume++ scope++)]
                           [(?ecdata consume** scope**) (xml-consume-token /dev/xmlin consume++++ scope++++)])
               (cond [(not (string? ?cdata)) (sax-subelement consume** scope** pcdatum null)]
                     [else (sax-subelement consume** scope**
                                           (and pcdatum (or (fpcdata tagname xpath ?cdata preserve? #true pcdatum) pcdatum))
                                           xml-whitespaces/0)]))]
            [(index? self)
             (sax-subelement consume++ scope++
                             (and tagname datum
                                  (let ([datum++ (sax-cdata-process tagname atadc fpcdata xpath preserve? datum)])
                                    (or (fgeref self (integer->char self) datum++) datum++)))
                             xml-whitespaces/0)]
            [(symbol? self)
             (sax-subelement consume++ scope++
                             (and tagname datum
                                  (let ([datum++ (sax-cdata-process tagname atadc fpcdata xpath preserve? datum)])
                                    (or (fgeref self (xml-prentity-value-ref self) datum++) datum++)))
                             xml-whitespaces/0)]
            [else #| deadcode |# (sax-subelement consume++ scope++ datum atadc)]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define sax-cdata-process : (All (seed) (->* (Symbol (Listof (U XML-White-Space String)) (XML-PCData-Handler seed) (Listof Symbol) Boolean seed) (Boolean) seed))
  (lambda [tagname atadc fpcdata xpath preserve? datum [tail? #false]]
    (cond [(null? atadc) datum]
          [else (let ([cdata/?1 (reverse (if (and tail? (eq? (car atadc) xml-whitespace/1)) (cdr atadc) atadc))])
                  (cond [(null? cdata/?1) datum]
                        [else (let ([cdata (if (eq? (car cdata/?1) xml-whitespace/0) (cdr cdata/?1) cdata/?1)])
                                (or (and (pair? cdata)
                                         (fpcdata tagname xpath
                                                  (apply string-append (map xml-cdata->datum cdata))
                                                  preserve? #false datum))
                                    datum))]))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-whitespaces/0 : (Listof XML-White-Space) (list xml-whitespace/0))

(define sax-identity/3 : (All (seed) (-> Any Any seed seed))
  (lambda [a1 a2 datum]
    datum))

(define sax-identity/4 : (All (seed) (-> Any Any Any seed seed))
  (lambda [a1 a2 a3 datum]
    datum))

(define sax-identity/6 : (All (seed) (-> Any Any Any Any Any seed seed))
  (lambda [a1 a2 a3 a4 a5 datum]
    datum))

(define #:forall (seed) sax-element-terminator : (-> (XML-Element-Handler seed) (XML-Element-Handler seed))
  (lambda [handle]
    (λ [[name : Symbol] [xpath : (Listof Symbol)] [attrs : (Option SAX-Attributes)] [empty? : Boolean] [preserve? : Boolean] [datum0 : seed]]
      (or (handle name xpath attrs empty? preserve? datum0) datum0))))

(define #:forall (seed) sax-pcdata-terminator : (-> (XML-PCData-Handler seed) (XML-PCData-Handler seed))
  (lambda [handle]
    (λ [[element : Symbol] [xpath : (Listof Symbol)] [pcdata : String] [preserve? : Boolean] [cdata? : Boolean] [datum0 : seed]]
      (or (handle element xpath pcdata preserve? cdata? datum0) datum0))))
