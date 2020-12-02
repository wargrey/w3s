#lang typed/racket/base

(provide (all-defined-out))

(require racket/format)
(require racket/port)

(require (for-syntax racket/base))
(require (for-syntax syntax/parse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (w3s-doc-process stx)
  (syntax-parse stx #:datum-literals []
    [(_ #:main+ lang.w3s:id MB cpu real gc #:body sexp ...)
     (syntax/loc stx
       (begin (provide lang.w3s)

              sexp ...

              (module+ main
                (newline)
                
                lang.w3s
                (w3s-display-times 'lang.w3s MB cpu real gc))))]
    [(_ process:id lang.w3s MB cpu real gc doc)
     (syntax/loc stx
       (w3s-doc-process #:main+ lang.w3s MB cpu real gc
                        #:body (define-values (lang.w3s MB cpu real gc) (w3s-read-doc doc process))))]
    [(_ ignored ...)
     (syntax/loc stx (void))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define w3s-memory-difference : (-> Natural Real)
  (lambda [memory0]
    (/ (- (current-memory-use) memory0)
       1024.0 1024.0)))

(define w3s-read-doc
  : (All (In Doc) (case-> [In (-> In Doc) -> (Values Doc Real Nonnegative-Integer Nonnegative-Integer Nonnegative-Integer)]
                          [Any Bytes Positive-Integer Nonnegative-Integer Positive-Integer (-> Input-Port Doc)
                               -> (Values Doc Real Nonnegative-Integer Nonnegative-Integer Nonnegative-Integer)]))
  (case-lambda
    [(/dev/docin read-doc)
     (let-values ([(memory0) (current-memory-use)]
                  [(&doc.xml cpu real gc) (time-apply read-doc (list /dev/docin))])
       (values (car &doc.xml) (w3s-memory-difference memory0) cpu real gc))]
    [(src bs line column position read-doc)
     (let ([/dev/docin (open-input-bytes bs src)]
           [mem0 (current-memory-use)])    
       (port-count-lines! /dev/docin)
       (set-port-next-location! /dev/docin line column position)
     
       (let-values ([(&lang.sgml cpu real gc) (time-apply read-doc (list /dev/docin))])
         (values (car &lang.sgml) (w3s-memory-difference mem0) cpu real gc)))]))

(define w3s-display-times : (-> Symbol Real Integer Integer Integer Void)
  (lambda [src MB cpu real gc]
    (define infomsg : String
      (if (= gc 0)
          (format "[~a]memory: ~aMB cpu time: ~a real time: ~a gc time: ~a"
            src (~r MB #:precision '(= 3)) cpu real gc)
          (format "[~a]memory: ~aMB cpu time: ~a(~a) real time: ~a(~a) gc time: ~a"
            src (~r MB #:precision '(= 3)) cpu (- cpu gc) real (- real gc) gc)))
    
    (displayln infomsg)
    (log-message (current-logger) 'info src infomsg
                 (vector MB cpu real gc) #false)))
