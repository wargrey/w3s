#lang typed/racket/base

(require digimon/spec)

(require sgml/digitama/stdin)
(require sgml/digitama/digicore)
(require sgml/digitama/tokenizer/port)
(require sgml/digitama/relaxng/rnc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define open-tamer-input : (-> (U String (Listof Char)) Input-Port)
  (lambda [stream.rnc]
    (define /dev/rncin : Input-Port
      (open-input-string
       (cond [(string? stream.rnc) stream.rnc]
             [else (apply string stream.rnc)])))

    (port-count-lines! /dev/rncin)
    /dev/rncin))

(define tamer-char-IO : (-> (U String (Listof Char)) (Values (U (Listof Char) Symbol) (U (Listof Char) Symbol)))
  (lambda [stream.rnc]
    (define /dev/rncin : Input-Port (open-tamer-input stream.rnc))

    (define p:chars : (U (Listof Char) Symbol)
      (let peek-chars ([p:srahc : (Listof Char) null]
                       [offset : Nonnegative-Fixnum 0])
        (define-values (ch offset++) (peek-rnc-char /dev/rncin offset))
        
        (cond [(char? ch) (peek-chars (cons ch p:srahc) offset++)]
              [(eof-object? ch) (reverse p:srahc)]
              [else (cdr ch)])))

    (define r:chars : (U (Listof Char) Symbol)
      (let read-chars ([r:srahc : (Listof Char) null])
        (define ch : (U Char EOF XML-Error) (read-rnc-char /dev/rncin))
        
        (cond [(char? ch) (read-chars (cons ch r:srahc))]
              [(eof-object? ch) (reverse r:srahc)]
              [else (cdr ch)])))

    (values r:chars p:chars)))

(define tamer-tokens : (-> (U String (Listof Char)) (Listof XML-Token))
  (lambda [stream.rnc]
    (read-rnc-tokens* (open-tamer-input stream.rnc) '/dev/rncin)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-behavior (it-check-io stream.rnc expected-values)
  (let-values ([(r:chars p:chars) (tamer-char-IO stream.rnc)])
    #:it
    ["should be parsed into ~s, when fed with ~s" expected-values stream.rnc] #:when (list? expected-values)
    ["should fail due to ~a, when fed with ~v" expected-values stream.rnc]
    #:do
    (expect-equal r:chars expected-values)
    (expect-equal p:chars expected-values)))

(define-behavior (it-check-token stream.rnc expected-values)
  (let-values ([(tokens) (tamer-tokens stream.rnc)])
    #:it
    ["should be parsed into ~s, when fed with ~s" expected-values stream.rnc]
    #:do
    (expect-equal (length tokens) (length expected-values))
    (for ([t (in-list tokens)]
          [v (in-list expected-values)])
      (cond [(procedure? v) (expect-satisfy (cast v (-> Any Boolean)) t)]
            [else (expect-equal (xml-token->datum t) v)]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (spec-begin RelaxNG #:do
              (describe "Compact Syntax" #:do
                        (describe "Read/Peek Char" #:do
                                  (it-check-io "rnc" (list #\r #\n #\c))
                                  (it-check-io (list #\newline #\return #\. #\return #\newline #\.) (list #\newline #\newline #\. #\newline #\.))
                                  (it-check-io "\rnc" (list #\newline #\n #\c))
                                  (it-check-io (list #\\ #\r #\n #\c) (list #\\ #\r #\n #\c))
                                  (it-check-io "\\x{5C}x{5C}" (list #\\ #\x #\{ #\5 #\C #\}))
                                  (it-check-io "\\xxxx{A}\\x{D}" (list (integer->char #xA) (integer->char #xD)))
                                  (it-check-io "\\x{5G}" 'exn:xml:read:badchar)
                                  (it-check-io "\\x{5G]" 'exn:xml:read:eof))
                        (describe "Tokenization" #:do
                                  (it-check-token "rnc" (list 'rnc))
                                  (it-check-token "\\x{66}\\x{6f}\\x{6f}" (list 'foo))
                                  (it-check-token "bar\\x{5G]" (list 'bar xml:bad?))
                                  (it-check-token "\\x{A}D" (list xml:whitespace? 'D))
                                  (it-check-token "# \\x{A}D" (list xml:comment?))
                                  (it-check-token "'''\\x{66}\r\\x{6f}\r\\x{6f}'''" (list "f\no\no"))
                                  (it-check-token "'\\x{6g}' token" (list (string (default-rnc-error-literal)) xml:whitespace? '#:token))
                                  (it-check-token "'broken\r'literal" (list xml:bad? 'literal))))))
