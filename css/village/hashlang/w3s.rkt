#lang typed/racket/base

(provide (all-defined-out))

(require racket/format)

(require (for-syntax racket/base))
(require (for-syntax syntax/parse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (w3s-doc-process stx)
  (syntax-parse stx #:datum-literals []
    [(_ process:id lang.w3s MB cpu real gc doc)
     #'(begin (provide lang.w3s)

              (define-values (lang.w3s MB cpu real gc)
                (let*-values ([(mem0) (current-memory-use)]
                              [(&lang.w3s cpu real gc) (time-apply process (list doc))])
                  (values (car &lang.w3s) (w3s-memory-difference mem0) cpu real gc)))

              (module+ main
                (newline)
                
                lang.w3s
                (w3s-display-times 'lang.w3s MB cpu real gc)))]
    [(_ ignored ...)
     #'(void)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define w3s-memory-difference : (-> Natural Real)
  (lambda [memory0]
    (/ (- (current-memory-use) memory0)
       1024.0 1024.0)))

(define w3s-read-doc : (All (Doc) (-> Any Bytes Positive-Integer Nonnegative-Integer Positive-Integer (-> Input-Port Doc)
                                      (Values Doc Real Nonnegative-Integer Nonnegative-Integer Nonnegative-Integer)))
  (lambda [src bs line column position read-doc]
    (define /dev/rawin : Input-Port (open-input-bytes bs src))
    (define mem0 (current-memory-use))
    
    (port-count-lines! /dev/rawin)
    (set-port-next-location! /dev/rawin line column position)
    
    (let-values ([(&lang.sgml cpu real gc) (time-apply read-doc (list /dev/rawin))])
      (values (car &lang.sgml) (w3s-memory-difference mem0) cpu real gc))))

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
