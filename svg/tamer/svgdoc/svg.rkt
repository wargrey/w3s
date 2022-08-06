#lang typed/racket/base

(provide (all-defined-out))

(require sgml/xml)
(require sgml/sax)

(require racket/list)
(require racket/path)
(require racket/file)
(require racket/port)
(require racket/string)

(require typed/net/http-client)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define registered-groups : (Listof Keyword)
  '(#:presentation #:filter-primitive #:conditional-processing #:transfer-function-element
    #:animation-attribute-target #:animation-timing #:animation-value #:animation-addition
    #:graphical-event #:animation-event #:document-event #:presentation
    #:xlink #:style #:external))

(define hidden-groups : (Listof Keyword)
  '(#:core))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type SAX-Toclist-Statue (Pairof (Listof String) (Option Symbol)))
(define-type SVG-Attribute-Datum (U (Pairof Keyword (Listof Symbol)) Symbol))
(define-type SVG-Element-Datum (List Symbol (Listof Keyword) (Listof Keyword) (Listof Symbol)))

(struct svgdoc-database
  ([elements : (Listof SVG-Element-Datum)]
   [categories : (Immutable-HashTable Keyword (Listof Symbol))]
   [attlists : (Immutable-HashTable Symbol (Listof Keyword))]
   [attribs : (Immutable-HashTable Keyword (Listof Symbol))])
  #:type-name SvgDoc-Database)

(struct svgdoc-element
  ([name : Symbol]
   [categories : (Listof Keyword)]
   [attributes : (Listof SVG-Attribute-Datum)])
  #:type-name SvgDoc-Element
  #:transparent
  #:mutable)

(define svg-elements->database : (-> (Listof SvgDoc-Element) SvgDoc-Database)
  (lambda [elems]
    (define-values (elements attributes)
      (for/fold ([elements : (Listof SVG-Element-Datum) null]
                 [attribs : (Immutable-HashTable Keyword (Listof Symbol)) (hasheq)])
                ([e (in-list elems)])
        (define as : (Listof Symbol) (filter symbol? (svgdoc-element-attributes e)))
        (define cs : (Listof (Pairof Keyword (Listof Symbol)))
          (for/list ([a (in-list (svgdoc-element-attributes e))]
                     #:when (pair? a))
            a))
        
        (values (cons (list (svgdoc-element-name e)
                            (svgdoc-element-categories e)
                            (remove-duplicates (map (inst car Keyword (Listof Symbol)) cs))
                            as)
                      elements)

                (for/fold ([adb : (Immutable-HashTable Keyword (Listof Symbol)) attribs])
                          ([ac (in-list cs)])
                  (cond [(hash-has-key? adb (car ac)) adb]
                        [else (hash-set adb (car ac) (cdr ac))])))))

    (define categories : (Immutable-HashTable Keyword (Listof Symbol))
      (for/fold ([cs : (Immutable-HashTable Keyword (Listof Symbol)) (hasheq)])
                ([e (in-list elems)])
        (for/fold ([acs : (Immutable-HashTable Keyword (Listof Symbol)) cs])
                  ([c (in-list (svgdoc-element-categories e))])
          (hash-set acs c (cons (svgdoc-element-name e) (hash-ref acs c (inst list Symbol)))))))

    (define attlists : (Immutable-HashTable Symbol (Listof Keyword))
      (for/fold ([als : (Immutable-HashTable Symbol (Listof Keyword)) (hasheq)])
                ([(ac as) (in-hash attributes)])
        (for/fold ([als : (Immutable-HashTable Symbol (Listof Keyword)) als])
                  ([attr (in-list as)])
          (hash-set als attr (cons ac (hash-ref als attr (inst list Keyword)))))))

    (svgdoc-database (reverse elements) categories attlists attributes)))

(define svg-database-list-all-elements : (-> SvgDoc-Database (Listof Symbol))
  (lambda [svgdb]
    (for/list ([es (in-list (svgdoc-database-elements svgdb))])
      (car es))))

(define svg-database-list-all-categories : (-> SvgDoc-Database (Listof Keyword))
  (lambda [svgdb]
    (hash-keys (svgdoc-database-categories svgdb))))

(define svg-database-list-all-attgroups : (-> SvgDoc-Database (Listof Keyword))
  (lambda [svgdb]
    (hash-keys (svgdoc-database-attribs svgdb))))

(define (svg-database-list-categories [svgdb : SvgDoc-Database] [tag-name : Symbol]) : (Listof Keyword)
  (define e : (Option SVG-Element-Datum) (assq tag-name (svgdoc-database-elements svgdb)))

  (cond [(not e) null]
        [else (cadr e)]))

(define (svg-database-list-attgroups [svgdb : SvgDoc-Database] [tag-name : Symbol]) : (Listof Keyword)
  (define e : (Option SVG-Element-Datum) (assq tag-name (svgdoc-database-elements svgdb)))

  (cond [(not e) null]
        [else (caddr e)]))

(define (svg-database-list-attributes [svgdb : SvgDoc-Database] [tag-name : Symbol]) : (Listof Symbol)
  (define e : (Option SVG-Element-Datum) (assq tag-name (svgdoc-database-elements svgdb)))

  (cond [(not e) null]
        [else (for/fold ([attrs : (Listof Symbol) (cadddr e)])
                        ([c (in-list (caddr e))])
                (append attrs (hash-ref (svgdoc-database-attribs svgdb) c)))]))

(define (svg-database-list-attribute/groups [svgdb : SvgDoc-Database] [tag-name : Symbol]) : (Listof (U Keyword Symbol))
  (define e : (Option SVG-Element-Datum) (assq tag-name (svgdoc-database-elements svgdb)))

  (cond [(not e) null]
        [else (append (caddr e) (cadddr e))]))

(define (svg-database-list-elements-of-category [svgdb : SvgDoc-Database] [category : (Option Keyword)]) : (Listof Symbol)
  (if (not category)
      (for/fold ([es : (Listof Symbol) null])
                ([e (in-list (svgdoc-database-elements svgdb))])
        (cond [(null? (cadr e)) (cons (car e) es)]
              [else es]))
      (hash-ref (svgdoc-database-categories svgdb) category (inst list Symbol))))

(define (svg-database-list-elements-of-attribute [svgdb : SvgDoc-Database] [att-name : (U Keyword Symbol)]) : (Listof Symbol)
  (define attribs (svgdoc-database-attribs svgdb))

  (if (keyword? att-name)
      (for/fold ([es : (Listof Symbol) null])
                ([e (in-list (svgdoc-database-elements svgdb))])
        (cond [(memq att-name (caddr e)) (cons (car e) es)]
              [else es]))
      (for/fold ([es : (Listof Symbol) null])
                ([e (in-list (svgdoc-database-elements svgdb))])
        (cond [(memq att-name (cadddr e)) (cons (car e) es)]
              [(for/or : Any ([ac (in-list (caddr e))])
                 (memq att-name (hash-ref attribs ac)))
               (cons (car e) es)]
              [else es]))))

(define (svg-database-list-elements-of-attgroup [svgdb : SvgDoc-Database] [group : Keyword]) : (Listof Symbol)
  (for/fold ([es : (Listof Symbol) null])
            ([e (in-list (svgdoc-database-elements svgdb))])
    (cond [(memq group (caddr e)) (cons (car e) es)]
          [else es])))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-pcdata-element->text : (-> XML-Element-Children (U Symbol String False))
  (lambda [e]
    (cond [(string? e) (string->symbol e)] ; for category NONE
          [else (let ([pcdata (car (caddr (assert e list?)))])
                  (cond [(string? pcdata) pcdata]
                        [else #false]))])))

(define xml-pcdata-element->name : (-> XML-Element-Children Symbol)
  (lambda [e]
    (define pcdata : (U Symbol String False) (xml-pcdata-element->text e))
    (cond [(string? pcdata) (string->symbol (substring pcdata 1 (sub1 (string-length pcdata))))]
          [(symbol? pcdata) pcdata]
          [else (gensym 'svg)])))

(define xml-pcdata-element->attribute : (-> XML-Element-Children (Option SVG-Attribute-Datum))
  (lambda [e]
    (define children.li : (Listof XML-Element-Children) (caddr (assert e list?)))
    (case (length children.li)
      [(1) #| self attributes |#
       (let ([attr (xml-pcdata-element->name (car (caddr (assert (car children.li) list?))))])
         (case attr
           [(class) (cons '#:style (list attr 'style))]
           [(style) (cons '#:style (list attr 'class))]
           [(externalResourcesRequired) (cons '#:external (list attr))]
           [else attr]))]
      [(2) #| group attributes |#
       (let ([group (xml-pcdata-element->text (car children.li))])
         (and (string? group)
              (cons (string->keyword
                     (string-join
                      (drop-right (string-split group) 1)
                      "-"))
                    (remove-duplicates
                     (reverse (for/fold ([attrs : (Listof Symbol) null])
                                        ([child.span (in-list (caddr (assert (cadr children.li) list?)))])
                                (cond [(not (list? child.span)) attrs]
                                      [else (let ([attr (xml-pcdata-element->name (car (caddr child.span)))])
                                              (cond [(or attr) (cons attr attrs)]
                                                    [else attrs]))])))))))]
      [else #false])))

(define xml-pcdata-element->categories : (-> XML-Element-Children (Listof Keyword))
  (lambda [e]
    (let refine ([children : (Listof XML-Element-Children) (caddr (assert e list?))]
                 [seirogetac : (Listof Keyword) null])
      (cond [(null? children) (reverse seirogetac)]
            [else (let*-values ([(self rest) (values (car children) (cdr children))]
                                [(pcdata) (xml-pcdata-element->text self)])
                    (cond [(not (string? pcdata)) (refine rest seirogetac)]
                          [else (refine rest (cons (string->keyword
                                                    (string-replace (string-titlecase pcdata)
                                                                    #px"\\s+" "-"))
                                                   seirogetac))]))]))))

(define xml-pcdata-element->attributes : (-> XML-Element-Children (Listof SVG-Attribute-Datum))
  (lambda [e]
    (let refine ([children : (Listof XML-Element-Children) (caddr (assert e list?))]
                 [seirogetac : (Listof SVG-Attribute-Datum) null])
      (cond [(null? children) (reverse seirogetac)]
            [else (let*-values ([(self rest) (values (car children) (cdr children))]
                                [(attr) (xml-pcdata-element->attribute self)])
                    (cond [(or attr) (refine rest (cons attr seirogetac))]
                          [else (refine rest seirogetac)]))]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (read-w3c-url [uri : String]) : Input-Port
  (define-values (status headers /dev/w3cin) (http-sendrecv "www.w3.org" uri))
  (displayln (cons uri status))
  /dev/w3cin)

(define (svgdoc-save-uri [uri.html : Path-String] [rootdir : Path-String]) : Void
  (define /dev/w3cin (read-w3c-url (format "/TR/SVG11/~a" uri.html)))
  (define uri.file (build-path rootdir uri.html))

  (make-parent-directory* uri.file)
  (call-with-output-file* uri.file
    (λ [[/dev/flout : Output-Port]]
      (copy-port /dev/w3cin /dev/flout)))

  (printf "[wrote to ~a]~n" uri.file))

(define (svgdoc-toc-list) : (Listof String)
  (define /dev/w3cin (read-w3c-url "https://www.w3.org/TR/SVG11/Overview.html"))

  (define sax-seek-tocline : (XML-Element-Handler SAX-Toclist-Statue)
    (lambda [name depth attrs empty? preserve? datum]
      (define indent (make-string (* depth 4) #\space))
      
      (or (and attrs
               (case name
                 [(li) (let ([class (assq 'class attrs)])
                         (and class
                              (equal? (cdr class) "tocline1")
                              (cons (car datum) 'green)))]
                 [(a) (and (eq? (cdr datum) 'green)
                           (let ([href (assq 'href attrs)])
                             (and href
                                  (string? (cdr href))
                                  (cons (cons (cdr href) (car datum)) #false))))]
                 [else #false]))
          datum)))

  (define toc-handler : (XML-Event-Handlerof SAX-Toclist-Statue)
    ((inst make-xml-event-handler SAX-Toclist-Statue)
     #:element sax-seek-tocline))

  (define datum : SAX-Toclist-Statue (read-xml-datum /dev/w3cin toc-handler (cons null #false)))
  (reverse (car datum)))

(define (svgdoc-element-xexpr-list/http [uri.html : Path-String]) : (Listof XML-Element)
  (define /dev/w3cin (read-w3c-url (format "/TR/SVG11/~a" uri.html)))
  (define docs.xml (xml-doc-root (xml-document-normalize (read-xml-document /dev/w3cin))))

  (cond [(not docs.xml) null]
        [else (xml-children-seek docs.xml 'class "element-summary")]))

(define (svgdoc-element-xexpr-list/file [uri.html : Path-String]) : (Listof XML-Element)
  (cond [(not (file-exists? uri.html)) null]
        [(not (equal? (path-get-extension uri.html) #".html")) null]
        [else (let ([docs.xml (xml-doc-root (xml-document-normalize (read-xml-document uri.html)))])
                (cond [(not docs.xml) null]
                      [else (xml-children-seek docs.xml 'class "element-summary")]))]))

(define (svgdoc-element->datum [div : XML-Element]) : SvgDoc-Element
  (define children : (Listof XML-Element-Children) (caddr div))
  (define name.span : XML-Element-Children (car (caddr (assert (car children) list?))))
  (define details.dl : (Listof XML-Element-Children) (caddr (assert (cadr children) list?)))
  (define categories.dd : XML-Element-Children (list-ref details.dl 1))
  (define contents.dd : XML-Element-Children (list-ref details.dl 3))
  (define attributes.dd : XML-Element-Children (car (caddr (assert (list-ref details.dl 5) list?))))

  (svgdoc-element (xml-pcdata-element->name name.span)
               (xml-pcdata-element->categories categories.dd)
               (xml-pcdata-element->attributes attributes.dd)))

(define (svgdoc-load-database)
  (define svgdoc.dir : Path (collection-file-path "svgdoc" "svg" "tamer" "svgdoc" "compiled"))

  (unless (directory-exists? svgdoc.dir)
    (define chapters : (Listof Path-String) (svgdoc-toc-list))
    (for ([chapter (in-list chapters)])
      (svgdoc-save-uri chapter svgdoc.dir)))
  
  (define chapters : (Listof Path-String) (directory-list svgdoc.dir #:build? #true))

  (define element.divs : (Listof (Listof XML-Element))
    (map svgdoc-element-xexpr-list/file chapters))

  (define elements : (Listof SvgDoc-Element)
    (for/fold ([es : (Listof SvgDoc-Element) null])
              ([e.div (in-list element.divs)])
      (append es (map svgdoc-element->datum e.div))))

  (svg-elements->database elements))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (svgdoc-attr-group-displayln [svgdb : SvgDoc-Database] [attrs : (Listof (U Keyword Symbol))] [indent : String ""]) : Void
  (define groups (filter keyword? attrs))
  (define extra (remove-duplicates (filter symbol? attrs)))
  
  (for ([group (in-list (remove-duplicates groups))])
    (define attrs (sort (hash-ref (svgdoc-database-attribs svgdb) group (inst list Symbol)) symbol<?))

    (unless (memq group hidden-groups)
      (printf "~a~a[~a]:" indent group (length attrs))
      (cond [(not (memq group registered-groups))
             (newline)
             (for ([attr (in-list (sort attrs symbol<?))])
               (printf "~a    [~a : (Option String) #:=> xml-attribute-value->string #false]~n"
                       indent attr))]
            [else (printf " ~a~n" (sort attrs symbol<?))])))

  (when (pair? extra)
    (printf "~aREST[~a]:~n" indent (length extra))
    (for ([attr (in-list (sort extra symbol<?))])
      (printf "~a    [~a : (Option String) #:=> xml-attribute-value->string #false]~n" indent attr))))

(define svgdoc-category-displayln : (All (a) (->* (SvgDoc-Database (Listof a) (-> SvgDoc-Database a (Listof Symbol)) (-> SvgDoc-Database Symbol (Listof a))) (String) Void))
  (lambda [svgdb attrs svg-list list-for-element [indent ""]]
    (define element-lists : (Listof (Listof Symbol))
      (for/list ([attr (in-list attrs)])
        (svg-list svgdb attr)))
  
    (define common-elements : (Listof Symbol)
      (cond [(null? element-lists) null]
            [(null? (cdr element-lists)) (car element-lists)]
            [else (for/fold ([ces : (Listof Symbol) (sort (car element-lists) symbol<?)])
                            ([es (in-list (cdr element-lists))])
                    (take-common-prefix ces (sort es symbol<?)))]))
    
    (define categories : (Listof Keyword)
      (remove-duplicates
       (apply append
              (for/list : (Listof (Listof Keyword)) ([ce (in-list common-elements)])
                (svg-database-list-categories svgdb ce)))))
    
    (for ([c (in-list (append categories (list #false)))])
      (printf "~n~a~a:~n" indent (or c 'NONE))
      (let* ([es (svg-database-list-elements-of-category svgdb c)]
             [total (length es)])
        (for ([attr (in-list attrs)])
          (define matches : (Listof Symbol)
            (for/list ([e (in-list es)]
                       #:when (let ([e-as (list-for-element svgdb e)])
                                (memq attr e-as)))
              e))
          (printf "~a    ~a[~a/~a]: ~a~n" indent attr
                  (length matches) total
                  matches))))

    (when (pair? categories)
      (printf "~a~a~n" indent (length categories)))))

(define svgdoc-info-displayln : (All (a) (-> SvgDoc-Database (Listof a) Symbol (-> SvgDoc-Database a (Listof (U Keyword Symbol))) Void))
  (lambda [svgdb argv type svg-list]
    (define lists : (Listof (Pairof (Listof (U Keyword Symbol)) a))
      (for/list ([tag (in-list argv)])
        (cons (remove-duplicates (svg-list svgdb tag)) tag)))

    (printf "~n=================== ~a ===================~n" type)
    
    (for ([ls (in-list lists)])
      (when (pair? (car ls))
        (printf "~n~a[~a, ~a]: ~a~n"
                (or (cdr ls) 'NONE) type (length (car ls))
                (sort (car ls) attrib<?))))

    (let ([dict : (HashTable (U Keyword Symbol) Natural) (make-hasheq)]
          [n (length argv)])
      (for ([ls (in-list lists)])
        (for ([t (in-list (car ls))])
          (hash-set! dict t (+ (hash-ref dict t (λ [] 0)) 1))))

      (when (> n 1)       
        (define-values (e-share e-diff)
          (for/fold ([cs : (Listof (U Keyword Symbol)) null] [ds : (Listof (U Keyword Symbol)) null])
                    ([(t c) (in-hash dict)])
            (if (= c n)
                (values (cons t cs) ds)
                (values cs (cons t ds)))))
        (printf "~n+[~a]: ~a~n" (length e-share) (sort e-share attrib<?))
        (printf "-[~a]: ~a~n" (length e-diff) (sort e-diff attrib<?))))))

(define (svgdoc-element-attgroup-displayln [svgdb : SvgDoc-Database] [elements : (Listof Symbol)]) : Void
  (printf "~n=================== Attribute Groups ===================~n")
  (for ([e (in-list elements)])
    (let ([cs (svg-database-list-categories svgdb e)])
      (printf "~n~a: ~a~n" e (if (null? cs) 'NONE cs))
      (svgdoc-attr-group-displayln svgdb (svg-database-list-attribute/groups svgdb e) "    "))))

(define attrib<? : (-> (U Symbol Keyword) (U Symbol Keyword) Boolean)
  (lambda [a1 a2]
    (cond [(and (keyword? a1) (keyword? a2)) (keyword<? a1 a2)]
          [(and (symbol? a1) (symbol? a2)) (symbol<? a1 a2)]
          [(keyword? a1) #true]
          [else #false])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (svgdoc-load-database))
