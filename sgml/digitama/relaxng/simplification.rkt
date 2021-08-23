#lang typed/racket/base

;;; https://relaxng.org/spec-20011203.html#simplification

(provide (all-defined-out))

(require digimon/filesystem)

(require sgml/xexpr)

(require "../relaxng.rkt")

(require "compact.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define rng-grammar-simplify : (-> RNG-Grammar RNG-Grammar)
  (lambda [rng]
    (define grammars : (U Pattern (Listof Grammar-Content)) (rng-grammar-body rng))

    (if (pattern? grammars)
        (rng-pattern-simplify rng grammars)
        (rng-grammars-simplify rng grammars))))

(define rng-pattern-simplify : (-> RNG-Grammar Pattern RNG-Grammar)
  (lambda [rng pattern]
    rng))

(define rng-grammars-simplify : (-> RNG-Grammar (Listof Grammar-Content) RNG-Grammar)
  (lambda [rng grammars]
    (define defines : (HashTable Symbol $define) (make-hash))
    
    (define simplified-body : (Listof Grammar-Content)
      (let simplify ([body : (Listof Grammar-Content) grammars]
                     [ydob : (Listof Grammar-Content) null])
        (cond [(null? body) (reverse ydob)]
              [else (let-values ([(self rest) (values (car body) (cdr body))])
                      (cond [(rng-include-element? self) (simplify rest (cons (rng-include->div rng self) ydob))]
                            [else (simplify rest ydob)]))])))
    
    (struct-copy rng-grammar rng
                 [body simplified-body])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define rng-include->div : (-> RNG-Grammar (U $include annotated-content) $div)
  (lambda [grammar include]
    (define-values (a:attrs a:children self)
      (cond [($include? include) (values null null include)]
            [else (let ([a (annotated-content-initial include)]
                        [inc (annotated-content-component include)])
                    (values (annotation-attributes a) (annotation-elements a)
                            (assert inc $include?)))]))
    
    (define href : Path (build-requiring-path (rng-grammar-location grammar) ($include-href self)))
    (define subrnc (rng-grammar-simplify (read-rnc-grammar href #:tagname 'div)))

    (define inherit ($include-inherit self))
    (define-values (remove-start? defines)
      (let collect : (Values Boolean (Listof Symbol))
        ([start? : Boolean #false]
         [defines : (Listof Symbol) null]
         [gc-rest : (Listof Grammar-Content) ($include-contents self)])
        (cond [(null? gc-rest) (values start? defines)]
              [else (let-values ([(subself subrest) (values (car gc-rest) (cdr gc-rest))])
                      (cond [($start? subself) (collect #true defines subrest)]
                            [($define? subself) (collect start? (cons ($define-name subself) defines) subrest)]
                            [else (collect start? defines subrest)]))])))

    (define gchidlren
      (let ([children (rng-grammar-body subrnc)])
        (if (list? children) children null)))

    ($div gchidlren)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define rng-include-element? : (-> Any Boolean : #:+ (U $include annotated-content))
  (lambda [rgc]
    (or ($include? rgc)
        (and (annotated-content? rgc)
             ($include? (annotated-content-component rgc))))))
