#lang typed/racket/base

(require css)
(require css/digitama/color)
(require css/digitama/image)

(require digimon/spec)

(require racket/string)

(require (for-syntax racket/base))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (tamer-context stx)
  (syntax-case stx []
    [(_ desc filters CSS:<> CSS<> #:do (it-check args ...) ...)
     (syntax/loc stx
       (context desc #:do
                (for/spec ([parser (in-list (list (apply CSS:<> filters) (apply CSS<> (map CSS:<^> filters))))]
                           [subc (in-list (list "filter" "parser"))])
                  (context subc #:do
                           (it-check filters parser args ...)
                           ...))))]
    [(_ desc filter multiplier CSS:<> CSS<> #:do (it-check args ...) ...)
     (syntax/loc stx
       (context [desc multiplier] #:do
                (for/spec ([parser (in-list (list (CSS:<> filter multiplier) (CSS<> (CSS:<^> filter) multiplier)))]
                           [subc (in-list (list "filter" "parser"))])
                  (context subc #:do
                           (it-check filter parser multiplier args ...)
                           ...))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define tamer-parse : (-> CSS-StdIn (CSS-Parser (Listof Any)) (Values (U CSS-Syntax-Error False (Listof Any)) (Listof CSS-Token)))
  (lambda [com.css atom-parser]
    (define tokens : (Listof CSS-Token) (filter-not css:whitespace? (css-parse-component-values com.css)))
    (define-values (seulav rest) (atom-parser null tokens))

    (values (if (list? seulav) (reverse seulav) seulav) rest)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-behavior (it-check-modifier modifier lower upper)
  (let-values ([(least most) (css:multiplier-range modifier lower)])
    #:it
    ["should accept all integers in the interval [~a, ~a] if marked with '~a" least most modifier] #:unless (eq? least most)
    ["should accept exact ~a integers if marked with '~a" least modifier]
    #:do
    (expect-= least lower)
    (expect-= most upper)))

(define-behavior (it-check-<&> filters parser expected-values okay?)
  (let*-values ([(com.css) (string-join (map ~a (append expected-values (list '<&>))))]
                [(size) (length filters)]
                [(vs rest) (tamer-parse com.css parser)])
    #:it
    ["should return a ~a-length list, and whose values are '~a, when fed with ~s" size (take expected-values size) com.css] #:when okay?
    ["should fail, when fed with ~s" com.css]
    #:do
    (check-juxtaposing-values vs size expected-values okay?)))

(define-behavior (it-check-<&&> filters parser expected-values maybe-count)
  (let*-values ([(com.css) (string-join (map ~a (append expected-values (list '<&&>))))]
                [(expt-values size) (maybe-expected-values->values filters expected-values maybe-count)]
                [(vs rest) (tamer-parse com.css parser)])
    #:it
    ["should return a ~a-length list, and whose values might be '~a, when fed with ~s" size expt-values com.css] #:when maybe-count
    ["should fail, when fed with ~s" com.css]
    #:do
    (check-no-ordered-values vs expt-values)))

(define-behavior (it-check-<++> filters parser expected-values maybe-count)
  (let*-values ([(com.css) (string-join (map ~a (append expected-values (list '<++>))))]
                [(expt-values size) (maybe-expected-values->values filters expected-values maybe-count)]
                [(vs rest) (tamer-parse com.css parser)])
    #:it
    ["should return a ~a-length list, and whose values might be '~a, when fed with ~s" size expt-values com.css] #:when maybe-count
    ["should fail, when fed with ~s" com.css]
    #:do
    (check-no-ordered-values vs expt-values)))

(define-behavior (it-check-<*> filter parser multiplier count expected)
  (let*-values ([(com.css) (string-join (map ~a (append (build-list count add1) (list '<*>))))]
                [(vs rest) (tamer-parse com.css parser)]
                [(least most) (css:multiplier-range multiplier 0)])
    #:it
    ["should return a list of integers, whose size is ~a, when fed with ~s where number count is ~a" expected com.css count] #:when expected
    ["should fail, when fed with ~s where number count is ~a" com.css count]
    #:do
    (check-range vs expected least most)))

(define-behavior (it-check-<#> filter parser multiplier count expected)
  (let*-values ([(com.css) (string-join (map ~a (append (build-list count add1) (list '<#>))) ", ")]
                [(vs rest) (tamer-parse com.css parser)]
                [(least most) (css:multiplier-range multiplier 0)])
    #:it
    ["should return a list of integers, whose size is ~a, when fed with ~s where number count is ~a" expected com.css count] #:when expected
    ["should fail, when fed with ~s where number count is ~a" com.css count]
    #:do
    (check-range vs expected least most)))

(define-behavior (it-check-<!> filter parser multiplier count expected)
  (let*-values ([(com.css) (string-join (map ~a (append (build-list count add1) (list '<!>))))]
                [(vs rest) (tamer-parse com.css parser)]
                [(least most) (css:multiplier-range multiplier 0)])
    #:it
    ["should return a nested list of integers, whose size is ~a, when fed with ~s where number count is ~a" expected com.css count] #:when expected
    ["should fail, when fed with ~s where number count is ~a" com.css count]
    #:do
    (cond [(list? vs) (expect-null (cdr vs)) (check-range (car vs) expected least most)]
          [else (check-range vs expected least most)])))

(define-behavior (it-check-function com.css filter expected)
  (let-values ([(vs rest) (tamer-parse com.css (CSS:<^> filter))])
    #:it
    ["should return an object in sense of ~a, when fed with ~s" (object-name expected) com.css] #:when (procedure? expected)
    ["should fail with exception `~a`, when fed with ~s" expected com.css] #:when (symbol? expected)
    ["should fail, when fed with ~s" com.css]
    #:do
    (cond [(not expected) (expect-false vs)]
          [(symbol? expected) (expect-satisfy exn:css? vs) (expect-eq (object-name vs) expected)]
          [(list? vs) (expect-= (length vs) 1) (expect-satisfy expected (car vs))]
          [else (tamer-deadcode vs)])))

(define check-range : (-> Any (U False (-> Any Boolean) Index) Natural (U Natural +inf.0) Void)
  (lambda [vs expected least most]
    (cond [(not expected) (expect-false vs)]
          [(procedure? expected) (expect-satisfy expected vs)]
          [(list? vs)
           (let ([size (length vs)])
             (expect->= size least)
             (expect-<= size most))]
          [else (tamer-deadcode vs)])))

(define check-juxtaposing-values : (-> (U False CSS-Syntax-Error (Listof Any)) Index (Listof Any) Boolean Void)
  (lambda [vs size expected-values okay?]
    (cond [(not okay?) (expect-false vs)]
          [(list? vs)
           (expect-= (length vs) size)
           (for ([given (in-list vs)]
                 [expected (in-list expected-values)])
             (expect-equal given expected))]
          [else (tamer-deadcode vs)])))

(define check-no-ordered-values : (-> (U False CSS-Syntax-Error (Listof Any)) (Option (Listof Any)) Void)
  (lambda [vs expected-values]
    (cond [(not expected-values) (expect-false vs)]
          [(list? vs)
           (expect-= (length vs) (length expected-values))
           (for ([given (in-list vs)])
             (expect-member given expected-values))]
          [else (tamer-deadcode vs)])))

(define tamer-deadcode : (-> Any Void)
  (lambda [vs]
    (cond [(exn:css? vs) (raise vs)]
          [else (collapse "deadcode")])))

(define maybe-expected-values->values : (-> (Listof Any) (Listof Any) (U (Listof Any) Boolean) (Values (Option (Listof Any)) Index))
  (lambda [filters expected-values maybe-count]
    (cond [(not maybe-count) (values #false 0)]
          [(list? maybe-count) (values maybe-count (length maybe-count))]
          [else (let ([size (length filters)]) (values (take expected-values size) size))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-feature recognizer #:do
  (let ([filters (list (procedure-rename (<css:ident>) '<css:ident>)
                       (procedure-rename (<css:integer>) '<css:integer>)
                       (procedure-rename (<css:delim>) '<css:delim>))])
    (context "combinators" #:do
             (tamer-context "juxtaposing values" filters CSS:<&> CSS<&> #:do
                           (it-check-<&> (list 'juxtaposing 128 #\&) #true)
                           (it-check-<&> (list 'juxtaposing 128.0 #\&) #false))
                        
             (tamer-context "combine-all" filters CSS:<&&> CSS<&&> #:do
                           (it-check-<&&> (list 128 'combine-all #\&) #true)
                           (it-check-<&&> (list 'combine-all #\&) #false))
                        
             (tamer-context "combine-any" filters CSS:<++> CSS<++> #:do
                           (it-check-<++> (list 128.0 'combine-any #\&) #false)
                           (it-check-<++> (list 'combine-any 128.0 #\&) '(combine-any))
                           (it-check-<++> (list #\& 'combine-any 128.0) '(combine-any #\&)))))

  (let ([filter (<css:integer>)])
    (context "multipliers" #:do
             (it-check-modifier '?         0  1)
             (it-check-modifier '*         0  +inf.0)
             (it-check-modifier '+         1  +inf.0)
             (it-check-modifier +16        16 16)
             (it-check-modifier '(8)       8  8)
             (it-check-modifier '(3 . 8)   3  8)
             (it-check-modifier '(4 . inf) 4  +inf.0)
             (it-check-modifier '(NaN . 4) 0  4)

             (tamer-context "given with the multiplier '~s'" filter '(2 . 4) CSS:<*> CSS<*> #:do
                            (it-check-<*> 1 #false)
                            (it-check-<*> 2 2)
                            (it-check-<*> 5 4))
             
             (tamer-context "given with the multiplier '~s'" filter '(1 . 3) CSS:<#> CSS<#> #:do
                            (it-check-<#> 0 #false)
                            (it-check-<#> 3 3)
                            (it-check-<#> 5 3))
             
             (tamer-context "given with the multiplier '~s'" filter '+ CSS:<!> CSS<!> #:do
                            (it-check-<!> 0 #false)
                            (it-check-<!> 3 3)
                            (it-check-<!> 5 5))))

  (context "functions" #:do
           (context "color functions" #:do
                    (it-check-function "hsl(120.0 100% 100% / 0.5)" (<css-color-notation>) flcolor?)
                    (it-check-function "hwb(120.0, 100%, 100%, 0.5)" (<css-color-notation>) flcolor?)
                    (it-check-function "rgb(0, 0.5, 0 / 0.618)" (<css-color-notation>) 'exn:css:missing-comma))
           
           (context "image functions" #:do
                    (it-check-function "url(tamer.png)" (<css-image>) string?)
                    (it-check-function "image(url(tamer.png))" (<css-image-notation>) css-image?)
                    (it-check-function "image(rtl \"tamer.png\", rgb(0 0.5 0 / 0.618))" (<css-image-notation>) css-image?)

                    (context "gradient functions" #:do
                             (it-check-function "url(tamer.png)" (<css-image>) string?)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (void (spec-prove recognizer)))
