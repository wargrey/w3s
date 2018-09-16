#lang typed/racket/base

(provide (all-defined-out))

(define-type (Listof+ xml) (Pairof xml (Listof xml)))

(define xml-log-error : (->* ((U exn String)) (Any Log-Level Symbol) Void)
  (lambda [errobj [src #false] [level 'debug] [topic 'exn:xml:fail]]
    (define message : String (if (string? errobj) errobj (format "@~s: ~a: ~a" src (object-name errobj) (exn-message errobj))))
    (log-message (current-logger) level topic message errobj)))
  
(define xml-log-read-error : (->* ((U exn String)) (Any Log-Level) Void)
  (lambda [errobj [src #false] [level 'debug]]
    (xml-log-error errobj src level 'exn:xml:read)))
