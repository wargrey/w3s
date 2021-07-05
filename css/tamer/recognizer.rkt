#lang typed/racket/base

(require css)
(require css/digitama/color)
(require css/digitama/image)
(require css/digitama/syntax/unsafe/cascade)

(require digimon/spec)

(require racket/string)

(require (for-syntax racket/base))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (tamer-context stx)
  (syntax-case stx []
    [(_ desc parser #:do (it-check args ...) ...)
     (syntax/loc stx
       (context desc #:do
                (it-check parser args ...)
                ...))]
    [(_ desc filters CSS:<> CSS<> #:do (it-check args ...) ...)
     (syntax/loc stx
       (context desc #:do
                (for/spec ([parser (in-list (list (apply (inst CSS:<> Any) filters)
                                                  (CSS<> (for/list : (Listof (CSS-Parser (Listof Any))) ([f (in-list filters)])
                                                           ((inst CSS:<^> Any) f)))))]
                           [subc (in-list (list "filter" "parser"))])
                  (context subc #:do
                           (it-check filters parser args ...)
                           ...))))]
    [(_ desc filter multiplier CSS:<> CSS<> #:do (it-check args ...) ...)
     (syntax/loc stx
       (context [desc multiplier] #:do
                (for/spec ([parser (in-list (list ((inst CSS:<> Any) filter multiplier) (CSS<> ((inst CSS:<^> Any) filter) multiplier)))]
                           [subc (in-list (list "filter" "parser"))])
                  (context subc #:do
                           (it-check filter parser multiplier args ...)
                           ...))))]
    [(_ com.css filter CSS:<> CSS<> #:invalid-do (it-check args ...) ...)
     (syntax/loc stx
       (context ["when fed with ~s" com.css] #:do
                (for/spec ([parser (in-list (list ((inst CSS:<> Any) filter) (CSS<> ((inst CSS:<^> Any) filter))))]
                           [subc (in-list (list "filter" "parser"))])
                  (context subc #:do
                           (it-check com.css parser args ...)
                           ...))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define tamer-parse : (-> CSS-StdIn Procedure (Values (U CSS-Syntax-Error False (Listof Any)) (Listof CSS-Token)))
  (lambda [com.css atom-parser]
    (read-css-component-values* com.css atom-parser)))

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
    ["should be parsed into a ~a-length list, and whose values are '~a, when fed with ~s" size (take expected-values size) com.css] #:when okay?
    ["should fail, when fed with ~s" com.css]
    #:do
    (check-juxtaposing-values vs size expected-values okay?)))

(define-behavior (it-check-<&&> filters parser expected-values maybe-count)
  (let*-values ([(com.css) (string-join (map ~a (append expected-values (list '<&&>))))]
                [(expt-values size) (maybe-expected-values->values filters expected-values maybe-count)]
                [(vs rest) (tamer-parse com.css parser)])
    #:it
    ["should be parsed into a ~a-length list, and whose values might be '~a, when fed with ~s" size expt-values com.css] #:when maybe-count
    ["should fail, when fed with ~s" com.css]
    #:do
    (check-no-ordered-values vs expt-values)))

(define-behavior (it-check-tricky-<&&> parser expected-values maybe-count)
  (let*-values ([(com.css) (string-join (map ~a (append expected-values (list '<&&>))))]
                [(expt-values size) (maybe-expected-values->values null expected-values maybe-count)]
                [(vs rest) (tamer-parse com.css parser)])
    #:it
    ["should be parsed into a ~a-length list, and whose values might be '~a, when fed with ~s" size expt-values com.css]
    #:do
    (check-no-ordered-values vs expt-values)))

(define-behavior (it-check-<++> filters parser expected-values maybe-count)
  (let*-values ([(com.css) (string-join (map ~a (append expected-values (list '<++>))))]
                [(expt-values size) (maybe-expected-values->values filters expected-values maybe-count)]
                [(vs rest) (tamer-parse com.css parser)])
    #:it
    ["should be parsed into a ~a-length list, and whose values might be '~a, when fed with ~s" size expt-values com.css] #:when maybe-count
    ["should fail, when fed with ~s" com.css]
    #:do
    (check-no-ordered-values vs expt-values)))

(define-behavior (it-check-<*> filter parser multiplier count expected)
  (let*-values ([(com.css) (string-join (map ~a (append (build-list count add1) (list '<*>))))]
                [(vs rest) (tamer-parse com.css parser)]
                [(least most) (css:multiplier-range multiplier 0)])
    #:it
    ["should be parsed into a list of integers, whose size is ~a, when fed with ~s where number count is ~a" expected com.css count] #:when expected
    ["should fail, when fed with ~s where number count is ~a" com.css count]
    #:do
    (check-range vs expected least most)))

(define-behavior (it-check-<#> filter parser multiplier count expected)
  (let*-values ([(com.css) (string-join (map ~a (append (build-list count add1) (list '<#>))) ", ")]
                [(vs rest) (tamer-parse com.css parser)]
                [(least most) (css:multiplier-range multiplier 0)])
    #:it
    ["should be parsed into a list of integers, whose size is ~a, when fed with ~s where number count is ~a" expected com.css count] #:when expected
    ["should fail, when fed with ~s where number count is ~a" com.css count]
    #:do
    (check-range vs expected least most)))

(define-behavior (it-check-invalid-<#> com.css parser expected)
  (let*-values ([(vs rest) (tamer-parse com.css parser)]
                [(least most) (css:multiplier-range '+ 0)])
    #:it
    ["should fail, with exception `~a`" (object-name expected)] #:when (procedure? expected)
    ["should be parsed into a list of integers, whose size is ~a" expected] #:when (index? expected)
    ["should fail"]
    #:do (check-range vs expected least most)))

(define-behavior (it-check-<!> filter parser multiplier count expected)
  (let*-values ([(com.css) (string-join (map ~a (append (build-list count add1) (list '<!>))))]
                [(vs rest) (tamer-parse com.css parser)]
                [(least most) (css:multiplier-range multiplier 0)])
    #:it
    ["should be parsed into a nested list of integers, whose size is ~a, when fed with ~s where number count is ~a" expected com.css count] #:when expected
    ["should fail, when fed with ~s where number count is ~a" com.css count]
    #:do
    (cond [(list? vs) (expect-null (cdr vs)) (check-range (car vs) expected least most)]
          [else (check-range vs expected least most)])))

(define-behavior (it-check com.css filter expected)
  (let-values ([(vs rest) (tamer-parse com.css filter)])
    #:it
    ["should be parsed into an object in sense of ~a, when fed with ~s" (object-name expected) com.css] #:when (procedure? expected)
    ["should fail with exception `~a`, when fed with ~s" expected com.css] #:when (symbol? expected)
    ["should fail, when fed with ~s" com.css]
    #:do
    (cond [(not expected) (expect-false vs)]
          [(eq? expected 'exn:css:overconsumption) (expect-satisfy pair? rest)]
          [(symbol? expected) (expect-satisfy exn:css? vs) (expect-eq (object-name vs) expected)]
          [(list? vs) (expect-= (length vs) 1) (expect-null rest) (expect-satisfy expected (car vs))]
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
  (let ([filters (list (<css:ident>) (<css:integer>) (<css:delim>))]
        [parser (CSS<&&> ((inst CSS:<^> Any) (<css:ident>)) ((inst CSS:<*> Any) (<css:integer>) '?))])
    (context "combinators" #:do
             (tamer-context "juxtaposing values" filters CSS:<&> css-juxtapose #:do
                           (it-check-<&> (list 'juxtaposing 128 #\&) #true)
                           (it-check-<&> (list 'juxtaposing 128.0 #\&) #false))
                        
             (tamer-context "combine-all" filters CSS:<&&> CSS<&&> #:do
                           (it-check-<&&> (list 128 'combine-all #\&) #true)
                           (it-check-<&&> (list 'combine-all #\&) #false))
             
             (tamer-context "combine-all with optional component" parser #:do
                            (it-check-tricky-<&&> (list 128 'combine-all #\&) '(combine-all 128))
                            (it-check-tricky-<&&> (list 'combine-all #\&) '(combine-all))
                            (it-check-tricky-<&&> (list 128 #\&) #false))
                        
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

             (tamer-context "given with the multiplier '~s'" filter '+ CSS:<!> CSS<!> #:do
                            (it-check-<!> 0 #false)
                            (it-check-<!> 3 3)
                            (it-check-<!> 5 5))

             (tamer-context "given with the multiplier '~s'" filter '(2 . 4) CSS:<*> CSS<*> #:do
                            (it-check-<*> 1 #false)
                            (it-check-<*> 2 2)
                            (it-check-<*> 5 4))
             
             (tamer-context "given with the multiplier '~s'" filter '(1 . 3) CSS:<#> CSS<#> #:do
                            (it-check-<#> 0 #false)
                            (it-check-<#> 3 3)
                            (it-check-<#> 5 3))

             (context "invalid hash mark syntax" #:do
                      (tamer-context ", 1, 2, 3" filter CSS:<#> CSS<#> #:invalid-do
                                     (it-check-invalid-<#> #false))
                      (tamer-context "1, , 2, 3" filter CSS:<#> CSS<#> #:invalid-do
                                     (it-check-invalid-<#> 3))
                      (tamer-context "1, 2, 3," filter CSS:<#> CSS<#> #:invalid-do
                                     (it-check-invalid-<#> exn:css:missing-value?))
                      (tamer-context "1, 2, 3, end" filter CSS:<#> CSS<#> #:invalid-do
                                     (it-check-invalid-<#> 3))

                      (let ([parser (CSS<#> ((inst CSS:<*> Any) filter '?) '+)])
                        (context "with optional component and emitted commas before, after, and between values" #:do
                                 (it-check-invalid-<#> ", 1, 2, 3" parser exn:css:missing-value?)
                                 (it-check-invalid-<#> "1, , 2, 3" parser exn:css:missing-value?)
                                 (it-check-invalid-<#> "1, 2, 3," parser exn:css:missing-value?))))))

  (context "datatypes" #:do
           (context "<position>" #:do
                    (it-check "left" (<:css-position:>) css-position?)
                    (it-check "center bottom" (<:css-position:>) css-position?)
                    (it-check "left 3cm  top 10px" (<:css-position:>) css-position?)
                    (it-check "     10px     15px" (<:css-position:>) css-position?)
                    (it-check "left          15px" (<:css-position:>) css-position?)
                    (it-check "     10px top     " (<:css-position:>) css-position?)
                    (it-check "left      top 15px" (<:css-position:>) 'exn:css:overconsumption)
                    (it-check "left 10px top     " (<:css-position:>) 'exn:css:overconsumption))
           
           (context "<color>" #:do
                    (it-check "hsl(120.0 100% 100% / 0.5)" (<css-color-notation>) flcolor?)
                    (it-check "hwb(120.0, 100%, 100%, 0.5)" (<css-color-notation>) flcolor?)
                    (it-check "rgb(0, 0.5, 0 / 0.618)" (<css-color-notation>) 'exn:css:missing-comma))
           
           (context "<image>" #:do
                    (it-check "url(tamer.png)" (<css-image>) string?)
                    (it-check "image(url(tamer.png))" (<css-image-notation>) css-image?)
                    (it-check "image(rtl \"tamer.png\", rgb(0 0.5 0 / 0.618))" (<css-image-notation>) css-image?)

                    (context "gradient functions" #:do
                             (it-check "linear-gradient(left)" (<css-gradient-notation>) 'exn:css:type)
                             (it-check "linear-gradient(red)" (<css-gradient-notation>) 'exn:css:arity)

                             (context "linear gradients" #:do
                                      (it-check "linear-gradient(yellow, blue)" (<css-gradient-notation>) linear-gradient?)
                                      (it-check "linear-gradient(180deg, yellow, blue)" (<css-gradient-notation>) linear-gradient?)
                                      (it-check "linear-gradient(to top, blue, yellow)" (<css-gradient-notation>) linear-gradient?)
                                      (it-check "linear-gradient(to bottom, 0% yellow, blue 100%)" (<css-gradient-notation>) linear-gradient?)
                                      (it-check "linear-gradient(135deg, yellow, 50%, blue)" (<css-gradient-notation>) linear-gradient?)
                                      (it-check "linear-gradient(-45deg, blue, 50%, yellow)" (<css-gradient-notation>) linear-gradient?)
                                      (it-check "linear-gradient(yellow, blue 20%, #0f0)" (<css-gradient-notation>) linear-gradient?)
                                      (it-check "linear-gradient(to top right, red, white, blue)" (<css-gradient-notation>) linear-gradient?))

                             (context "radial gradients" #:do
                                      (it-check "radial-gradient(5px circle at top left, yellow, blue)" (<css-gradient-notation>) radial-gradient?)
                                      (it-check "radial-gradient(yellow, green)" (<css-gradient-notation>) radial-gradient?)
                                      (it-check "radial-gradient(ellipse at center, yellow 0%, green 100%)" (<css-gradient-notation>) radial-gradient?)
                                      (it-check "radial-gradient(farthest-corner at 50% 50%, yellow, green)" (<css-gradient-notation>) radial-gradient?)
                                      (it-check "radial-gradient(circle, yellow, green)" (<css-gradient-notation>) radial-gradient?)
                                      (it-check "radial-gradient(red, yellow, green)" (<css-gradient-notation>) radial-gradient?)
                                      (it-check "radial-gradient(farthest-side at left bottom, red, yellow 50px, green)" (<css-gradient-notation>) radial-gradient?)
                                      (it-check "radial-gradient(closest-side at 20px 30px, red, yellow, green)" (<css-gradient-notation>) radial-gradient?)
                                      (it-check "radial-gradient(20px 30px at 20px 30px, red, yellow, green)" (<css-gradient-notation>) radial-gradient?)
                                      (it-check "radial-gradient(closest-side circle at 20px 30px, red, yellow, green)" (<css-gradient-notation>) radial-gradient?)
                                      (it-check "radial-gradient(20px 20px at 20px 30px, red, yellow, green)" (<css-gradient-notation>) radial-gradient?)
                                      (it-check "radial-gradient(at 20px 30px, green 100%, yellow 150%, red 200%)" (<css-gradient-notation>) radial-gradient?))

                             (context "conic gradients" #:do
                                      (it-check "conic-gradient(at 25% 30%, white, black 60%)" (<css-gradient-notation>) conic-gradient?)
                                      (it-check "conic-gradient(#f06, gold)" (<css-gradient-notation>) conic-gradient?)
                                      (it-check "conic-gradient(at 50% 50%, #f06, gold)" (<css-gradient-notation>) conic-gradient?)
                                      (it-check "conic-gradient(from 0deg, #f06, gold)" (<css-gradient-notation>) conic-gradient?)
                                      (it-check "conic-gradient(from 0deg at center, #f06, gold)" (<css-gradient-notation>) conic-gradient?)
                                      (it-check "conic-gradient(#f06 0%, gold 100%)" (<css-gradient-notation>) conic-gradient?)
                                      (it-check "conic-gradient(#f06 0deg, gold 1turn)" (<css-gradient-notation>) conic-gradient?)
                                      (it-check "conic-gradient(white -50%, black 150%)" (<css-gradient-notation>) conic-gradient?)
                                      (it-check "conic-gradient(white -180deg, black 540deg)" (<css-gradient-notation>) conic-gradient?)
                                      (it-check "conic-gradient(hsl(0,0%,75%), hsl(0,0%,25%))" (<css-gradient-notation>) conic-gradient?)
                                      (it-check "conic-gradient(from 45deg, white, black, white)" (<css-gradient-notation>) conic-gradient?)
                                      (it-check "conic-gradient(hsl(0,0%,87.5%), white 45deg, black 225deg, hsl(0,0%,87.5%))"
                                                (<css-gradient-notation>) conic-gradient?)
                                      (it-check "conic-gradient(white 45deg, black 225deg, white 405deg)" (<css-gradient-notation>) conic-gradient?)
                                      (it-check "conic-gradient(red, yellow, lime, aqua, blue, magenta, red)" (<css-gradient-notation>) conic-gradient?)
                                      (it-check "conic-gradient(yellowgreen 40%, gold 0deg 75%, #f06 0deg)" (<css-gradient-notation>) conic-gradient?)
                                      (it-check "repeating-conic-gradient(hsla(0,0%,100%,.2) 0deg 15deg, hsla(0,0%,100%,0) 0deg 30deg)"
                                                (<css-gradient-notation>) conic-gradient?))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (void (spec-prove recognizer)))
