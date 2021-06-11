#lang typed/racket

(require "configure.rkt")
(require "../syntax.rkt")
(require "../digitama/syntax/selector.rkt")

(default-css-abc->specificity
  (λ [[a : Natural] [b : Natural] [c : Natural]]
    (+ (* a 100) (+ (* b 10) c))))

(time* (map (λ [[in : String]] : (Pairof String Integer)
              (let ([?complex-selectors (css-parse-selectors in)])
                (cond [(exn:css? ?complex-selectors) (cons in -1)]
                      [else (cons in (css-complex-selector-specificity (car ?complex-selectors)))])))
            (list "* + *"
                  "li"
                  "li::first-line"
                  "ul li"
                  "ul ol+li"
                  "h1 + *[rel=up]"
                  "ul ol li.red"
                  "li.red.level"
                  "module.main"
                  "module"
                  ":root"
                  "#1234"
                  ":not(FOO)#s12"
                  ".foo :matches(.bar, #baz)"
                  "body #darkside [sith] p")))
