#lang typed/racket/base

(require css/syntax)

(require digimon/spec)

(require racket/string)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define tamer-parse : (-> CSS-StdIn (CSS-Parser (Listof Any)) (Values (U CSS-Syntax-Error False (Listof Any)) (Listof CSS-Token)))
  (lambda [com.css atom-parser]
    (define tokens : (Listof CSS-Token) (filter-not css:whitespace? (css-parse-component-values com.css)))
    (define-values (seulav rest) (atom-parser null tokens))

    (values (if (list? seulav) (reverse seulav) seulav) rest)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-behavior (it-check-modifier modifier lower upper)
  (let-values ([(least most) (css:multiplier-range modifier lower)])
    #:it ["should accept all integers in the interval [~a, ~a] if marked with '~a" least most modifier] #:do
    (expect-= least lower)
    (expect-= most upper)))

(define-behavior (it-check-<&>-filter filters expected-values)
  (let*-values ([(com.css) (string-join (map ~a (append expected-values (list '<&>))))]
                [(size) (length filters)]
                [(vs rest) (tamer-parse com.css (apply CSS:<&> filters))])
    #:it ["should return a ~a-length list, and whose values are '~a, fed with {~s}" size (take expected-values size) com.css] #:do
    (check-juxtaposing-values vs size expected-values)))

(define-behavior (it-check-<&>-parser filters expected-values)
  (let*-values ([(com.css) (string-join (map ~a (append expected-values (list '<&>))))]
                [(size) (length filters)]
                [(vs rest) (tamer-parse com.css (apply CSS<&> (map CSS:<^> filters)))])
    #:it ["should return a ~a-length list, and whose values are '~a, fed with {~s}" size (take expected-values size) com.css] #:do
    (check-juxtaposing-values vs size expected-values)))

(define-behavior (it-check-<&&>-filter filters expected-values)
  (let*-values ([(com.css) (string-join (map ~a (append expected-values (list '<&&>))))]
                [(size) (length filters)]
                [(vs rest) (tamer-parse com.css (apply CSS:<&&> filters))])
    #:it ["should return a ~a-length list, and whose values might be '~a, fed with {~s}" size (take expected-values size) com.css] #:do
    (check-no-ordered-values vs size expected-values)))

(define-behavior (it-check-<&&>-parser filters expected-values)
  (let*-values ([(com.css) (string-join (map ~a (append expected-values (list '<&&>))))]
                [(size) (length filters)]
                [(vs rest) (tamer-parse com.css (apply CSS<&&> (map CSS:<^> filters)))])
    #:it ["should return a ~a-length list, and whose values might be '~a, fed with {~s}" size (take expected-values size) com.css] #:do
    (check-no-ordered-values vs size expected-values)))

(define-behavior (it-check-<*>-filter filter multiplier count expected)
  (let*-values ([(com.css) (string-join (map ~a (append (build-list count add1) (list '<*>))))]
                [(vs rest) (tamer-parse com.css (CSS:<*> filter multiplier))]
                [(least most) (css:multiplier-range multiplier 0)])
    #:it ["should return a list of integers, whose size is ~a, fed with {~s} where number count is ~a" expected com.css count] #:do
    (check-range vs expected least most)))

(define-behavior (it-check-<*>-parser filter multiplier count expected)
  (let*-values ([(com.css) (string-join (map ~a (append (build-list count add1) (list '<*>))))]
                [(vs rest) (tamer-parse com.css (CSS<*> (CSS:<^> filter) multiplier))]
                [(least most) (css:multiplier-range multiplier 0)])
    #:it ["should return a list of integers, whose size is ~a, fed with {~s} where number count is ~a" expected com.css count] #:do
    (check-range vs expected least most)))

(define-behavior (it-check-<#>-filter filter multiplier count expected)
  (let*-values ([(com.css) (string-join (map ~a (append (build-list count add1) (list '<#>))) ", ")]
                [(vs rest) (tamer-parse com.css (CSS:<#> filter multiplier))]
                [(least most) (css:multiplier-range multiplier 0)])
    #:it ["should return a list of integers, whose size is ~a, fed with {~s} where number count is ~a" expected com.css count] #:do
    (check-range vs expected least most)))

(define-behavior (it-check-<#>-parser filter multiplier count expected)
  (let*-values ([(com.css) (string-join (map ~a (append (build-list count add1) (list '<#>))) ", ")]
                [(vs rest) (tamer-parse com.css (CSS<#> (CSS:<^> filter) multiplier))]
                [(least most) (css:multiplier-range multiplier 0)])
    #:it ["should return a list of integers, whose size is ~a, fed with {~s} where number count is ~a" expected com.css count] #:do
    (check-range vs expected least most)))

(define-behavior (it-check-<!>-filter filter multiplier count expected)
  (let*-values ([(com.css) (string-join (map ~a (append (build-list count add1) (list '<!>))))]
                [(vs rest) (tamer-parse com.css (CSS:<!> filter multiplier))]
                [(least most) (css:multiplier-range multiplier 0)])
    #:it ["should return a nested list of integers, whose size is ~a, fed with {~s} where number count is ~a" expected com.css count] #:do
    (cond [(list? vs) (expect-null (cdr vs)) (check-range (car vs) expected least most)]
          [else (check-range vs expected least most)])))

(define-behavior (it-check-<!>-parser filter multiplier count expected)
  (let*-values ([(com.css) (string-join (map ~a (append (build-list count add1) (list '<!>))))]
                [(vs rest) (tamer-parse com.css (CSS<!> (CSS:<^> filter) multiplier))]
                [(least most) (css:multiplier-range multiplier 0)])
    #:it ["should return a nested list of integers, whose size is ~a, fed with {~s} where number count is ~a" expected com.css count] #:do
    (cond [(list? vs) (expect-null (cdr vs)) (check-range (car vs) expected least most)]
          [else (check-range vs expected least most)])))

(define check-range : (-> Any (U False (-> Any Boolean) Index) Natural (U Natural +inf.0) Void)
  (lambda [vs expected least most]
    (cond [(not expected) (expect-false vs)]
          [(procedure? expected) (expect-satisfy expected vs)]
          [(list? vs)
           (let ([size (length vs)])
             (expect->= size least)
             (expect-<= size most))]
          [else (collapse "deadcode")])))

(define check-juxtaposing-values : (-> (U False CSS-Syntax-Error (Listof Any)) Index (Listof Any) Void)
  (lambda [vs size expected-values]
    (cond [(list? vs)
           (expect-= (length vs) size)
           (for ([given (in-list vs)]
                 [expected (in-list expected-values)])
             (expect-equal given expected))]
          [(exn:css? vs) (raise vs)]
          [else (collapse "deadcode")])))

(define check-no-ordered-values : (-> (U False CSS-Syntax-Error (Listof Any)) Index (Listof Any) Void)
  (lambda [vs size expected-values]
    (cond [(list? vs)
           (expect-= (length vs) size)
           (for ([given (in-list vs)])
             (expect-member given expected-values))]
          [(exn:css? vs) (raise vs)]
          [else (collapse "deadcode")])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-feature recognition #:do
  (context "combinators" #:do
           (let/spec ([filters (list (<css:ident>) (<css:integer>) (<css:delim>))]
                      [expt-values (list 'combine-all 128 #\&)])
                     (context "juxtaposing values" #:do
                              (context "filter" #:do
                                       (it-check-<&>-filter filters expt-values))
                              
                              (context "parser" #:do
                                       (it-check-<&>-parser filters expt-values)))
                     
                     (context "combin-all" #:do
                              (context "filter" #:do
                                       (it-check-<&&>-filter filters (shuffle expt-values)))
                              
                              (context "parser" #:do
                                       (it-check-<&&>-parser filters (shuffle expt-values))))))
           
  (context "multipliers" #:do
           (it-check-modifier '?         0  1)
           (it-check-modifier '*         0  +inf.0)
           (it-check-modifier '+         1  +inf.0)
           (it-check-modifier +16        16 16)
           (it-check-modifier '(8)       8  8)
           (it-check-modifier '(3 . 8)   3  8)
           (it-check-modifier '(4 . inf) 4  +inf.0)
           (it-check-modifier '(NaN . 4) 0  4)

           (let ([multiplier '(2 . 4)])
             (context ["given with the multiplier '~s'" multiplier] #:do
                      (context "filter" #:do
                               (it-check-<*>-filter (<css:integer>) multiplier 1 #false)
                               (it-check-<*>-filter (<css:integer>) multiplier 2 2)
                               (it-check-<*>-filter (<css:integer>) multiplier 5 4))
                      
                      (context "parser" #:do
                               (it-check-<*>-parser (<css:integer>) multiplier 1 #false)
                               (it-check-<*>-parser (<css:integer>) multiplier 2 2)
                               (it-check-<*>-parser (<css:integer>) multiplier 5 4))))

           (let ([multiplier '(1 . 3)])
             (context ["given with the multiplier '~s'" multiplier] #:do
                      (context "filter" #:do
                               (it-check-<#>-filter (<css:integer>) multiplier 0 #false)
                               (it-check-<#>-filter (<css:integer>) multiplier 3 3)
                               (it-check-<#>-filter (<css:integer>) multiplier 5 3))
                      
                      (context "parser" #:do
                               (it-check-<#>-parser (<css:integer>) multiplier 0 #false)
                               (it-check-<#>-parser (<css:integer>) multiplier 3 3)
                               (it-check-<#>-parser (<css:integer>) multiplier 5 3))))

           (let ([multiplier '+])
             (context ["given with the multiplier '~s'" multiplier] #:do
                      (context "filter" #:do
                               (it-check-<!>-filter (<css:integer>) multiplier 0 #false)
                               (it-check-<!>-filter (<css:integer>) multiplier 3 3)
                               (it-check-<!>-filter (<css:integer>) multiplier 5 5))
                      
                      (context "parser" #:do
                               (it-check-<!>-parser (<css:integer>) multiplier 0 #false)
                               (it-check-<!>-parser (<css:integer>) multiplier 3 3)
                               (it-check-<!>-parser (<css:integer>) multiplier 5 5))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (void (spec-prove recognition)))
