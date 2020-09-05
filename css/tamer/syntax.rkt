#lang typed/racket

(require racket/logging)

(require "configure.rkt")
(require "../syntax.rkt")
(require "../image.rkt")
(require "../font.rkt")

(require racket/runtime-path)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-runtime-path tamer/tamer.css "tamer.css")

(css-configure-@media)
(collect-garbage)

(define tamer-sheet : CSS-Stylesheet (time-run (read-css-stylesheet tamer/tamer.css))) ; (require "tamer.css") is much faster
(define tamer-root : CSS-Subject (css-subject* #:type 'body #:id '#:header #::classes '(root)))
(define tamer-syntax : CSS-Subject (css-subject* #:type 'syntax #:id '#:syntax #:classes '(error maybe-error)))
(define tamer-reborn : CSS-Subject (css-subject* #:type 'reborn #:id '#:reborn))

(define css-all-parsers : CSS-Declaration-Parsers
  (lambda [suitcased-name !]
    (λ [[initial : (Listof Any)] [declared-values : (Listof CSS-Token)]]
      (values (map css-token->datum declared-values) null))))

(define css-maybe-error-parsers : CSS-Declaration-Parsers
  (lambda [suitcased-name deprecated!]
    (or (css-font-property-parsers suitcased-name)
        (css-image-property-parsers suitcased-name)
        (and (eq? suitcased-name 'at-exp)
             (CSS:<^> (list (<css:λracket>)
                            (<css:racket>)
                            (<css:block>)))))))

(define css-all-filter : (CSS-Cascaded-Value+Filter (Vectorof (Pairof Symbol Any)) Symbol)
  (lambda [declared-values inherited-values type]
    (for/vector : (Vectorof (Pairof Symbol Any)) ([desc-name (in-hash-keys declared-values)])
      (cons desc-name (box (css-ref declared-values inherited-values desc-name))))))

(match-define (list $root *root)
  (time-run (let-values ([(root:values for-children)
                          (css-cascade (list tamer-sheet) (list tamer-root)
                                       css-all-parsers css-all-filter
                                       #false ':root)])
              (list root:values for-children))))
(cons tamer-root $root)

(match-define (list $syntax *syntax)
  (time-run ((inst with-logging-to-port (List Any CSS-Values))
             (current-output-port)
             (thunk (let-values ([($syntax *syntax)
                                  (css-cascade (list tamer-sheet) (list tamer-syntax tamer-root)
                                               css-maybe-error-parsers css-all-filter
                                               *root 'syntax)])
                      (list $syntax *syntax)))
             'debug)))
(cons tamer-syntax $syntax)

(define $reborn
  (time-run ((inst with-logging-to-port Any)
             (current-output-port)
             (thunk (let-values ([($reborn *reborn)
                                  (css-cascade (list tamer-sheet) (list tamer-reborn tamer-syntax tamer-root)
                                               css-all-parsers css-all-filter
                                               *syntax 'reborn)])
                      $reborn))
             'debug)))
(cons tamer-reborn $reborn)
