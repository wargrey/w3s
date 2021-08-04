#lang typed/racket/base

;;; https://www.w3.org/TR/xml-names/#ns-qualnames

(provide (all-defined-out))

(require racket/string)
(require racket/symbol)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-qname-has-namespace? : (-> Symbol Boolean)
  (lambda [qname]
    (string-contains? (symbol->immutable-string qname) ":")))

(define xml-qname-split : (-> Symbol (Values Symbol Symbol))
  (lambda [qname]
    (define sqname : String (symbol->immutable-string qname))
    
    (cond [(not (string-contains? sqname ":")) (values qname qname)]
          [else (let ([parts : (Listof Symbol) (map string->symbol (string-split sqname ":"))])
                  (cond [(null? parts) #| input is just ': |# (values '|| '||)]
                        [(null? (cdr parts)) (if (string-suffix? sqname ":") (values (car parts) '||) (values '|| (car parts)))]
                        [(pair? (cddr parts)) (values qname qname)]
                        [else (values (car parts) (cadr parts))]))])))

(define xml-qname-prefix : (-> Symbol Symbol)
  (lambda [qname]
    (define-values (prefix _) (xml-qname-split qname))
    prefix))

(define xml-qname-local-part : (-> Symbol Symbol)
  (lambda [qname]
    (define-values (_ localpart) (xml-qname-split qname))
    localpart))

(define xml-qname-xmlns? : (-> Symbol Boolean)
  (lambda [qname]
    (or (eq? qname 'xmlns)
        (string-prefix? (symbol->immutable-string qname) "xmlns:"))))
