#lang typed/racket/base

(provide (all-defined-out))
(provide (rename-out [symbol->immutable-string xml:attr-symbol->string]
                     [symbol-join xml:attr-list->string]

                     [string-normalize-spaces xml:attr-string->token]
                     [symbol->immutable-string xml:attr-symbol->token]
                     [symbol-join xml:attr-list->token]

                     [symbol->interned-symbol xml:attr-symbol->symbol]
                     [symbol->unreadable-symbol xml:attr-symbol->unreadable-symbol]

                     [symbol->keyword xml:attr-symbol->keyword]
                     [xml:attr-string->dimension xml:attr-symbol->dimension]
                     [xml:attr-string->percentage xml:attr-symbol->percentage]))

(require racket/format)

(require digimon/symbol)
(require digimon/string)
(require digimon/dimension)
(require digimon/number)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type XML-Boolean (U 'true 'false))
(define-type XML-Dimension FlDimension)
(define-type XML-Nonnegative-Dimension Nonnegative-FlDimension)
(define-type XML-Percentage FlPercentage)
(define-type XML-Nonnegative-Percentage Nonnegative-FlPercentage)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-boolean? : (-> Any Boolean : XML-Boolean)
  (lambda [v]
    (and (or (eq? v 'true)
             (eq? v 'false)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml:attr-datum->value : (-> Any String)
  (lambda [v]
    (cond [(string? v) v]
          [(symbol? v) (symbol->immutable-string v)]
          [(and (integer? v) (inexact? v)) (number->string (inexact->exact v))]
          [(number? v) (number->string v)]
          [(list? v) (string-join (map xml:attr-datum->value v))]
          [(#%dim? v) (dimension->string v)]
          [else (~a v)])))

(define xml:attr-hexdecimal->value : (-> Integer String)
  (lambda [v]
    (number->string v 16)))

(define xml:attr-listof-type->value : (All (T) (->* ((Listof T) (-> T String)) (String) String))
  (lambda [vs datum->value [sep ", "]]
    (string-join (map datum->value vs) sep)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml:attr-string->boolean : (-> String (Option XML-Boolean))
  (lambda [v]
    (cond [(member v '("true" "1")) 'true]
          [(member v '("false" "0")) 'false]
          [else #false])))

(define xml:attr-symbol->boolean : (-> Symbol (Option XML-Boolean))
  (lambda [v]
    (cond [(eq? v 'true) v]
          [(eq? v 'false) v]
          [else #false])))

(define #:forall (N) xml:attr-string->real : (case-> [String (-> String N) -> N]
                                                     [String (-> String Integer N) Integer -> N])
  (case-lambda
    [(v string->number) (string->number (string-trim v))]
    [(v string->number base) (string->number (string-trim v) base)]))

(define #:forall (N) xml:attr-symbol->real : (case-> [Symbol (-> String N) -> N]
                                                     [Symbol (-> String Integer N) Integer -> N])
  (case-lambda
    [(v string->number) (string->number (symbol->immutable-string v))]
    [(v string->number base) (string->number (symbol->immutable-string v) base)]))

(define #:forall (S) xml:attr-string->symbol : (-> String (-> String S) S)
  (lambda [v ->symbol]
    (->symbol (string-trim v))))

(define #:forall (S) xml:attr-list->symbol : (-> (Listof Symbol) (-> String S) S)
  (lambda [v ->symbol]
    (->symbol (symbol-join v))))

(define #:forall (Fl) xml:attr-string->dimension : (case-> [(U String Symbol) Symbol Boolean False -> (Option XML-Dimension)]
                                                           [(U String Symbol) Symbol Boolean (-> Flonum Boolean : #:+ Fl) -> (Option (#%Dim Fl))])
  (lambda [v canonical-unit ci? fln?]
    (define-values (n unit) (string->dimension v canonical-unit #:ci? ci?))
    (and n
         (cond [(not fln?) (#%dim n unit)]
               [(fln? n) ((inst #%dim Fl) n unit)]
               [else #false]))))

(define #:forall (Fl) xml:attr-string->percentage : (case-> [(U String Symbol) Symbol False -> (Option XML-Percentage)]
                                                            [(U String Symbol) Symbol (-> Flonum Boolean : #:+ Fl) -> (Option (#%Per Fl))])
  (lambda [v canonical-unit fln?]
    (define-values (n unit) (string->dimension v canonical-unit #:ci? #false))
    (and n
         (eq? unit '%)
         (cond [(not fln?) (make-percentage n)]
               [(fln? n) ((inst make-percentage Fl) n)]
               [else #false]))))

(define #:forall (T) xml:attr-string->list : (->* (String (-> String T)) ((U String Regexp (-> String (Listof String)))) (Listof T))
  (lambda [v string->datum [sep #px"\\s+"]]
    (map string->datum (if (procedure? sep) (sep v) (string-split v sep)))))

(define #:forall (T) xml:attr-symbol->list : (-> Symbol (-> String T) (Listof T))
  (lambda [v string->datum]
    (list (string->datum (symbol->immutable-string v)))))

(define #:forall (T) xml:attr-list->list : (-> (Listof Symbol) (-> String T) (Listof T))
  (lambda [vs string->datum]
    (for/list : (Listof T) ([v (in-list vs)])
      (string->datum (symbol->immutable-string v)))))

(define #:forall (T) xml:attr-string->filtered-list : (->* (String (-> String (Option T))) ((U String Regexp (-> String (Listof String)))) (Listof T))
  (lambda [v string->datum [sep #px"\\s+"]]
    (let parse ([vs : (Listof String) (if (procedure? sep) (sep v) (string-split v sep))]
                [ts : (Listof T) null])
      (cond [(null? vs) (reverse ts)]
            [else (let*-values ([(self rest) (values (car vs) (cdr vs))]
                                [(datum) (string->datum self)])
                    (cond [(not datum) (parse rest ts)]
                          [else (parse rest (cons datum ts))]))]))))

(define #:forall (T) xml:attr-symbol->filtered-list : (-> Symbol (-> String (Option T)) (Listof T))
  (lambda [v string->datum]
    (let ([datum (string->datum (symbol->immutable-string v))])
      (cond [(not datum) null]
            [else (list datum)]))))

(define #:forall (T) xml:attr-list->filtered-list : (-> (Listof Symbol) (-> String (Option T)) (Listof T))
  (lambda [v string->datum]
    (let parse ([vs : (Listof Symbol) v]
                [ts : (Listof T) null])
      (cond [(null? vs) (reverse ts)]
            [else (let*-values ([(self rest) (values (car vs) (cdr vs))]
                                [(self) (symbol->immutable-string self)]
                                [(datum) (string->datum self)])
                    (cond [(not datum) (parse rest ts)]
                          [else (parse rest (cons datum ts))]))]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml:attr-string-filter : (-> String (U String Bytes Regexp Byte-Regexp (-> String Boolean)) (Option String))
  (lambda [s pattern]
    (cond [(procedure? pattern) (and (pattern s) s)]
          [else (and (regexp-match? pattern s) s)])))

(define xml:attr-hexadecimal-filter : (-> String Positive-Byte (Option Index))
  (lambda [hex len]
    (and (= (string-length hex) len)
         (string->index hex 16))))

(define #:forall (N) xml:attr-real-filter
  : (case-> [(Option (∩ Real N)) (Option (∩ Real N)) -> (Option N)]
            [(Option (∩ Real N)) (Option (∩ Real N)) (Option (∩ Real N)) -> (Option N)]
            [(Option Real) (-> Any Boolean : #:+ (∩ Real N)) (Option (∩ Real N)) (Option (∩ Real N)) -> (Option N)])
  (case-lambda
    [(n min)
     (and n
          (cond [(and min) (and (<= min n) n)]
                [else n]))]
    [(n min max)
     (and n
          (cond [(and min max) (and (<= min n max) n)]
                [(and min) (and (<= min n) n)]
                [(and max) (and (<= n max) n)]
                [else n]))]
    [(n real? min max)
     (and n (real? n)
          (cond [(and min max) (and (<= min n max) n)]
                [(and min) (and (<= min n) n)]
                [(and max) (and (<= n max) n)]
                [else n]))]))
