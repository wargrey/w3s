#lang typed/racket/base

;;; https://relaxng.org/spec-20011203.html#simplification

(provide (all-defined-out))

(require digimon/filesystem)

(require "../relaxng.rkt")

(require "compact.rkt")
(require "schema.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define rng-current-inherit : (Parameterof (Pairof Symbol (Option String))) (make-parameter (cons 'inherit #false)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define rnc-grammar-simplify : (->* (RNC-Grammar) ((Listof (Pairof Symbol String)) (Listof RNG-Grammar-Content)) RNG-Grammar)
  (lambda [rnc [annotated-attributes null] [siblings null]]
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

    (define attributes : (Listof (Pairof Symbol String))
      (append (cond [(not default-uri) (reverse xmlns:ns)]
                    [else (cons (cons 'ns default-uri) (reverse xmlns:ns))])
              annotated-attributes))

    (if (pattern? grammars)
        (rng-pattern-simplify rnc grammars)
        (rnc-grammars-simplify rnc grammars namespaces attributes siblings))))

(define rng-pattern-simplify : (-> RNC-Grammar Pattern RNG-Grammar)
  (lambda [rnc pattern]
    (rng-grammar (rnc-grammar-tagname rnc) (rnc-grammar-location rnc)
                 null null #false null rng-empty-definitions)))

(define rnc-grammars-simplify : (-> RNC-Grammar (Listof Grammar-Content) RNC-Preamble-Namespaces
                                    (Listof (Pairof Symbol String)) (Listof RNG-Grammar-Content)
                                    RNG-Grammar)
  (lambda [rnc grammars namespaces attributes siblings]
    (define-values (?start defines children) (rng-grammar-contents-simplify (rnc-grammar-location rnc) grammars namespaces))
    
    (rng-grammar (rnc-grammar-tagname rnc) (rnc-grammar-location rnc)
                 attributes children ?start siblings defines)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define rng-grammar-contents-simplify : (-> (U String Symbol) (Listof Grammar-Content) RNC-Preamble-Namespaces
                                            (Values (Option RNG-Element) RNG-Definitions (Listof RNG-Grammar-Content)))
  (lambda [source grammars namespaces]
    (let simplify ([body : (Listof Grammar-Content) grammars]
                   [ydob : (Listof RNG-Grammar-Content) null]
                   [a:initial : (Option Annotation) #false]
                   [?start : (Option RNG-Element) #false]
                   [defines : RNG-Definitions rng-empty-definitions])
      (cond [(null? body) (values ?start defines (reverse ydob))]
            [else (let-values ([(self rest) (values (car body) (cdr body))])
                    (cond [(grammar-annotation? self)
                           (simplify rest (cons (rng-annotation-element->foreign-element (grammar-annotation-element self)) ydob) #false ?start defines)]
                          [($include? self)
                           (simplify rest (cons (rng-include->nested-grammar-div source self a:initial namespaces) ydob) #false ?start defines)]
                          [(a:content? self)
                           (simplify (cons (a:content-component self) rest) ydob (a:content-initial self) ?start defines)]
                          [($div? self) ; yes, ignore the initial annotation
                           (simplify (append ($div-contents self) rest) ydob #false ?start defines)]
                          [else (simplify rest ydob #false ?start defines)]))]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define rng-include->nested-grammar-div : (-> (U Symbol String) $include (Option Annotation) RNC-Preamble-Namespaces RNG-Grammar)
  (lambda [pwd self a:initial namespaces]
    (define-values (a:attrs a:children)
      (cond [(not a:initial) (values null null)]
            [else (values (annotation-attributes a:initial)
                          (annotation-elements a:initial))]))

    (define inherit ($include-inherit self))
    (define-values (?start definitions siblings) (rng-grammar-contents-simplify pwd ($include-contents self) namespaces))
    
    (define subrnc : RNG-Grammar
      (parameterize ([rnc-shadow-start? (and ?start #true)]
                     [rnc-shadow-definitions (hash-keys definitions)]
                     [rng-current-inherit (if (not inherit) (rng-current-inherit) (cons inherit (hash-ref namespaces inherit (λ [] #false))))])
        (rnc-grammar-simplify (read-rnc-grammar (build-requiring-path pwd ($include-href self)) #:tagname 'div)
                              a:attrs (append (map rng-annotation-element->foreign-element a:children) siblings))))

    subrnc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define rng-empty-definitions : RNG-Definitions #hasheq())

(define rng-namespace-uri : (-> RNC-Preamble-Namespaces (U Symbol String False) (Option String))
  (lambda [ns uri]
    (cond [(string? uri) uri]
          [(symbol? uri) (rng-namespace-uri ns (hash-ref ns uri (λ [] #false)))]
          [else (cdr (rng-current-inherit))])))
