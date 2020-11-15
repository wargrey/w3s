#lang typed/racket/base

(provide (all-defined-out))

(require typed/racket/unsafe)

(require (for-syntax racket/base))

(require "digicore.rkt")
(require "whitespace.rkt")

(unsafe-require/typed
 racket/unsafe/ops ; only works for Latin-1 Strings
 [unsafe-string-ref (-> String Index Char)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (define-xml:space=preserve stx)
  (syntax-case stx []
    [(_ xml:space=preserve #:=> Space #:remake-space remake-space #:space-datum space-datum #:space-newline? space-newline?)
     #'(define xml:space=preserve : (-> Symbol Space (Option XML:Space-Filter) (Option String) Space)
         (lambda [tag ws xml:?filter xml:lang]
           (define has-newline? : Boolean (space-newline? ws))

           (cond [(not (or xml:?filter has-newline?)) ws]
                 [else (let* ([xml:filter (or xml:?filter xml:space-values)]
                              [spaces : String (space-datum ws)]
                              [size : Index (string-length spaces)])
                         (let preserve-filter ([idx : Nonnegative-Fixnum 0]
                                               [xD? : Boolean #false]
                                               [secaps : (Option (Listof Char)) #false])
                           (if (>= idx size)
                               (let ([sp-chars (if (not xD?) secaps (assert (xml:space-cons spaces idx #\newline #\return xml:filter 0 secaps tag xml:lang)))])
                                 ; NOTE: XML-New-Line and XML:NewLine are designed to indicate if newlines have been normalized
                                 (if (not sp-chars) ws (remake-space ws xml:whitespace (list->string (reverse sp-chars)))))
                               (let ([ch (unsafe-string-ref spaces idx)]
                                     [idx+1 (+ idx 1)])
                                 (cond [(not xD?)
                                        (cond [(eq? ch #\return) (preserve-filter idx+1 #true secaps)]
                                              [else (preserve-filter idx+1 #false (xml:space-cons spaces idx ch ch xml:filter 0 secaps tag xml:lang))])]
                                       [(or (eq? ch #\newline) #;(eq? ch #\u0085))
                                        (preserve-filter idx+1 #false (xml:space-cons spaces idx #\newline #\return xml:filter 1 secaps tag xml:lang))]
                                       [else (preserve-filter idx #false (xml:space-cons spaces (- idx 1) #\newline #\return xml:filter 0 secaps tag xml:lang))])))))])))]))

(define-syntax (define-xml-child-cons stx)
  (syntax-case stx []
    [(_ xml-child-cons (#:-> Space XML-Children)
        #:remake-space remake-space #:space-datum space-datum
        #:spaces-fold spaces-fold #:spaces-consolidate spaces-consolidate
        #:space=preserve xml:space=preserve #:space-newline? space-newline?)
     #'(define xml-child-cons : (->* ((Listof Space) XML-Children (Option XML:Space-Filter) Symbol (Option String)) (Boolean) XML-Children)
         (lambda [secaps nerdlihc xml:filter tag xml:lang [tail? #false]]
           (cond [(null? secaps) nerdlihc]
                 [(not xml:filter) (if (or (null? nerdlihc) tail?) nerdlihc (cons (spaces-consolidate secaps) nerdlihc))]
                 [else (let* ([raw-spaces (spaces-fold secaps)]
                              [well-formed-raw-spaces (xml:space=preserve tag raw-spaces #false xml:lang)]
                              [has-newline? (space-newline? raw-spaces)]
                              [raw (space-datum well-formed-raw-spaces)]
                              [head? (null? nerdlihc)])
                         (define default-space : (Option String)
                           (cond [(and tail? head?) (xml:filter tag xml:lang raw #false 'span has-newline?)]
                                 [(and head?) (xml:filter tag xml:lang raw #false 'head has-newline?)]
                                 [(and tail?) (xml:filter tag xml:lang raw #false 'tail has-newline?)]
                                 [else (xml:filter tag xml:lang raw " " 'body has-newline?)]))
                         (cond [(not default-space) nerdlihc]
                               [(or (string=? default-space raw)) (cons well-formed-raw-spaces nerdlihc)]
                               [else (cons (remake-space well-formed-raw-spaces xml:whitespace default-space) nerdlihc)]))])))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml:space-cons : (-> String Fixnum Char Char (-> Symbol (Option String) Char (Option Char)) (U Zero One) (Option (Listof Char))
                             Symbol (Option String) (Option (Listof Char)))
  (lambda [spaces idx s os xml:filter nl-span secaps0 tagname xml:lang]
    (define ns : (Option Char) (xml:filter tagname xml:lang s))
    (cond [(eq? ns os) (if (list? secaps0) (cons s secaps0) secaps0)]
          [else (let ([secaps (if (list? secaps0) secaps0 (reverse (string->list (substring spaces 0 (- idx nl-span)))))])
                  (if (not ns) secaps (cons ns secaps)))])))

(define xml:space-values : (-> Symbol (Option String) Char (Option Char))
  (lambda [tag xml:lang char]
    char))
