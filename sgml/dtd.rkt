#lang typed/racket/base

(provide (all-defined-out) SGML-StdIn)
(provide (struct-out XML-DTD) read-xml-type-definition)
(provide XML-Schema xml-schema? struct:xml-schema)
(provide Open-Input-XML-XXE xml-dtd-expand)

(require racket/path)
(require racket/file)
(require racket/port)

(require typed/net/url)
(require typed/net/head)

(require "digitama/dtd.rkt")
(require "digitama/stdin.rkt")
(require "digitama/schema.rkt")
(require "digitama/normalize.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type XML-XXE-HTTP-URL-Filter (-> (Option String) (Option String) (Option String)))
(define-type DTD-Entities Schema-Entities)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-dtd-entity-expand : (->* (XML-DTD)
                                     (#:stop-if-xxe-unread? Boolean #:open-xxe-input-port (Option Open-Input-XML-XXE)
                                      #:ipe-topsize (Option Index) #:xxe-topsize (Option Index) #:xxe-timeout (Option Real)
                                      (Option DTD-Entities) Boolean)
                                     DTD-Entities)
  (lambda [#:stop-if-xxe-unread? [stop? #false] #:open-xxe-input-port [open-port #false]
           #:ipe-topsize [ipe-topsize (default-xml-ipe-topsize)] #:xxe-topsize [xxe-topsize (default-xml-xxe-topsize)] #:xxe-timeout [timeout (default-xml-xxe-timeout)]
           dtd [int-entities #false] [merge? #true]]
    (define-values (entities _ _?)
      (xml-dtd-entity-expand/partition dtd int-entities merge? stop? ipe-topsize
                                       (and open-port (vector-immutable open-port xxe-topsize timeout))))

    entities))

(define make-xml-load-http-entity : (->* () ((Option Path-String) (Option XML-XXE-HTTP-URL-Filter)) Open-Input-XML-XXE)
  (let ([simple-url-filter : XML-XXE-HTTP-URL-Filter (λ [public system] (or system public))]
        [topic : Symbol 'xml:public:entity])
    (define (open-input-url [source : String] [url : URL] [local-dir : Path] [topsize : (Option Index)]) : (Option (Pairof Input-Port Boolean))
      (define host : (Option String) (url-host url))
      
      (and (string? host)
           (let ([subpath (filter string? (map path/param-path (url-path url)))])
             (and (pair? subpath)
                  (let ([path.ent (apply build-path local-dir host subpath)])
                    
                    (unless (file-exists? path.ent)
                      (with-handlers ([exn? (λ [[e : exn]] (log-message (current-logger) 'warning topic (format "download failed: ~a" (exn-message e)) #false))])
                        (log-message (current-logger) 'debug topic (format "downloading ~a" source) #false)

                        (define temp.ent : Path (make-temporary-file "xml-~a.ent"))
                        (define-values (/dev/entin raw-headers) (get-pure-port/headers url))

                        (when (or (not topsize)
                                  (let* ([headers (extract-all-fields raw-headers)]
                                         [?field (assoc "content-length" headers)]
                                         [?value (and ?field (cdr ?field))]
                                         [?size (and ?value (string->number (if (bytes? ?value) (bytes->string/utf-8 ?value) ?value)))])
                                    (and (index? ?size) (<= ?size topsize))))

                          (make-parent-directory* path.ent)
                          (call-with-output-file* temp.ent #:exists 'truncate/replace
                            (λ [[/dev/entout : Output-Port]]
                              (copy-port /dev/entin /dev/entout)))
                          (rename-file-or-directory temp.ent path.ent #true)

                          (log-message (current-logger) 'debug topic
                                       (format "downloaded ~a to ~a" (file-name-from-path source) (path-only path.ent))
                                       #false))
                        
                        (close-input-port /dev/entin)))
                    
                    (and (file-exists? path.ent)
                         (cons (open-input-file path.ent) #true)))))))

    (lambda [[?localdir #false] [?url-filter #false]]
      (define url-filter : XML-XXE-HTTP-URL-Filter (or ?url-filter simple-url-filter))
      (define local-rootdir : Path
        (cond [(not ?localdir) (build-path (find-system-path 'temp-dir) "gydm.wargrey.xml:public:entity")]
              [else (simple-form-path ?localdir)]))
      
      (λ [rootdir public system topsize &alt-source]
        (define ?url : (Option String) (url-filter public system))
        (and (string? ?url)
             (if (path? rootdir)
                 (let* ([url (string->url ?url)]
                        [scheme (url-scheme url)])
                   (set-box! &alt-source (url->string url))
                   (and scheme
                        (or (string=? scheme "http") (string=? scheme "https"))
                        (open-input-url ?url url local-rootdir topsize)))
                 (let* ([url (string->url (string-append rootdir ?url))]
                        [source (url->string url)])
                   (set-box! &alt-source source)
                   (open-input-url source url local-rootdir topsize))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module reader racket/base
  (provide (except-out (all-from-out racket/base) read read-syntax))

  (provide (rename-out [dtd-read read]))
  (provide (rename-out [dtd-read-syntax read-syntax]))
  (provide (rename-out [dtd-info get-info]))
  
  (require sgml/village/sgmlang/reader))
