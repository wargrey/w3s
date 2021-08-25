#lang typed/racket/base

;;; https://relaxng.org/spec-20011203.html#simplification

(provide (all-defined-out))

(require digimon/filesystem)

(require sgml/xexpr)

(require "../relaxng.rkt")

(require "compact.rkt")
(require "schema.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define rng-current-inherit : (Parameterof (Option (Pairof Symbol String))) (make-parameter #false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define rnc-grammar-simplify : (-> RNC-Grammar RNG-Grammar)
  (lambda [rnc]
    (define grammars : (U Pattern (Listof Grammar-Content)) (rnc-grammar-body rnc))
    (define namespaces : RNC-Preamble-Namespaces (rnc-grammar-namespaces rnc))
    (define default-uri : (Option String) (rng-namespace-uri namespaces (rnc-grammar-default-namespace rnc)))
    (define xmlns:ns : (Listof (Pairof Symbol String))
      (for/fold ([xmlns : (Listof (Pairof Symbol String)) null])
                ([(ns uri) (in-hash namespaces)])
        (define xmlns:ns : Symbol (string->symbol (format "xmlns:~a" ns)))
        (define xmlns-uri : (Option String) (rng-namespace-uri namespaces uri))
        (cond [(not xmlns-uri) xmlns]
              [else (cons (cons xmlns:ns xmlns-uri) xmlns)])))

    (define rng : RNG-Grammar
      (if (pattern? grammars)
          (rng-pattern-simplify rnc grammars)
          (rnc-grammars-simplify rnc grammars)))

    (struct-copy rng-grammar rng
                 [attributes (cond [(not default-uri) (reverse xmlns:ns)]
                                   [else (cons (cons 'ns default-uri)
                                               (reverse xmlns:ns))])])))

(define rng-pattern-simplify : (-> RNC-Grammar Pattern RNG-Grammar)
  (lambda [rnc pattern]
    (rng-grammar (rnc-grammar-tagname rnc) (rnc-grammar-location rnc)
                 null null)))

(define rnc-grammars-simplify : (-> RNC-Grammar (Listof Grammar-Content) RNG-Grammar)
  (lambda [rnc grammars]
    (define defines : (HashTable Symbol $define) (make-hash))
    
    (define body : (Listof RNG-Grammar-Content)
      (let simplify ([body : (Listof Grammar-Content) grammars]
                     [ydob : (Listof RNG-Grammar-Content) null])
        (cond [(null? body) (reverse ydob)]
              [else (let-values ([(self rest) (values (car body) (cdr body))])
                      (cond [(rng-include-element? self) (simplify rest (cons (rng-include->div rnc self) ydob))]
                            [else (simplify rest ydob)]))])))
    
    (rng-grammar (rnc-grammar-tagname rnc) (rnc-grammar-location rnc)
                 null body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define rng-include->div : (-> RNC-Grammar (U $include annotated-content) RNG-Grammar)
  (lambda [grammar include]
    (define-values (a:attrs a:children self)
      (cond [($include? include) (values null null include)]
            [else (let ([a (annotated-content-initial include)]
                        [inc (annotated-content-component include)])
                    (values (annotation-attributes a) (annotation-elements a)
                            (assert inc $include?)))]))

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
    
    (define subrnc : RNG-Grammar
      (parameterize ([rnc-shadow-start? remove-start?]
                     [rnc-shadow-definitions defines])
        (rnc-grammar-simplify (read-rnc-grammar (build-requiring-path (rnc-grammar-location grammar) ($include-href self)) #:tagname 'div))))

    subrnc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define rng-include-element? : (-> Any Boolean : #:+ (U $include annotated-content))
  (lambda [rgc]
    (or ($include? rgc)
        (and (annotated-content? rgc)
             ($include? (annotated-content-component rgc))))))

(define rng-namespace-uri : (-> RNC-Preamble-Namespaces (U Symbol String False) (Option String))
  (lambda [ns uri]
    (cond [(string? uri) uri]
          [(symbol? uri) (rng-namespace-uri ns (hash-ref ns uri (λ [] #false)))]
          [else (let ([inherited-ns (rng-current-inherit)])
                  (and inherited-ns (cdr inherited-ns)))])))
