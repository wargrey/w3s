#lang typed/racket/base

(require digimon/spec)

(require sgml/digitama/digicore)
(require sgml/digitama/tokenizer/port)

(require sgml/digitama/relaxng/rnc)
(require sgml/digitama/relaxng/grammar)

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

(define tamer-env : (All (a) (-> (U String (Listof Char)) (XML-Parser (Listof a)) (Values (XML-Option (Listof a)) (Listof XML-Token))))
  (lambda [stream.rnc <rng>]
    (define-values (grammar rest) (<rng> null (tamer-tokens stream.rnc)))
    (cond [(list? grammar) (values (reverse grammar) rest)]
          [else (values grammar rest)])))

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
    (for ([t (in-list tokens)]
          [v (in-list expected-values)])
      (cond [(procedure? v) (expect-satisfy (cast v (-> Any Boolean)) t)]
            [else (expect-equal (xml-token->datum t) v)]))
    (expect-equal (length tokens) (length expected-values))))

(define-behavior (it-check-parser stream.rnc logsrc <rng> expected-values)
  (let ([rng-object? (or (rng-env? expected-values)
                         (rng-pattern? expected-values)
                         (rng-name-class? expected-values)
                         (rng-grammar-content? expected-values))])
    #:it
    ["should be parsed into ~s, when fed with ~s" expected-values stream.rnc] #:when rng-object?
    ["should report error due to `~a`, when fed with ~s" (object-name expected-values) stream.rnc] #:when (procedure? expected-values)
    ["shouldn't report error, when fed with ~s" stream.rnc] #:when (and (vector? expected-values) (= (vector-length expected-values) 0))
    ["should report error as in ~a, when fed with ~s" expected-values stream.rnc]
    #:do
    (if (not rng-object?)

        (expect-log-message logsrc expected-values
                            (λ [] (tamer-env stream.rnc <rng>)))
        
        (let-values ([(es rest) (tamer-env stream.rnc <rng>)])
          (expect-satisfy list? es)
          (expect-equal (car (assert es list?)) expected-values)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (define logsrc : Log-Receiver (make-log-receiver (current-logger) 'warning 'exn:xml:syntax))
  
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
                                  (it-check-token "'broken\r'literal" (list xml:bad? 'literal))
                                  (it-check-token "'\"'\"'\"" (list (string #\") "'"))
                                  (it-check-token "\\ \\\\" (list #\\ xml:whitespace? #\\ #\\))
                                  (it-check-token "element\\element" (list '#:element 'element))
                                  (it-check-token "name|='value'" (list 'name #\| "value"))
                                  (it-check-token "start&=grammar" (list '#:start #\& '#:grammar))
                                  (it-check-token "ns:name" (list 'ns:name)))
                        (describe "Environment" #:do
                                  (it-check-parser "default namespace xml = 'uri'" logsrc (<:rnc-decl*:>) (rng-namespace 'xml "uri" #true))
                                  (it-check-parser "default namespace = inherit" logsrc (<:rnc-decl*:>) (rng-namespace '|| 'inherit #true))
                                  (it-check-parser "default namespace = inherit default namespace = inherit" logsrc (<:rnc-decl*:>) exn:xml:duplicate?)
                                  (it-check-parser "default namespace dup = 'dup' namespace dup = 'dup'" logsrc (<:rnc-decl*:>) exn:xml:duplicate?)
                                  (it-check-parser "namespace dup = 'is_okay' datatypes dup = 'yes'" logsrc (<:rnc-decl*:>) (vector))
                                  (it-check-parser "datatypes cat = 'https://' ~ 'gyoudmon.org'" logsrc (<:rnc-decl*:>) (rng-datatype 'cat "https://gyoudmon.org")))
                        (describe "Name Class" #:do
                                  (it-check-parser "ns:name" logsrc (<:rnc-name-class:>) (rng-name 'ns:name))
                                  (it-check-parser "*" logsrc (<:rnc-name-class:>) (rng-any-name #false #false))
                                  (it-check-parser "* - name" logsrc (<:rnc-name-class:>) (rng-any-name #false (rng-name 'name))))
                        (describe "Grammar Content" #:do
                                  (it-check-parser "start |= \\grammar" logsrc (<:rnc-grammar-content:>) (rng-start #\| (rng:ref 'grammar)))
                                  (it-check-parser "begin |= end" logsrc (<:rnc-grammar-content:>) (rng-define 'begin #\| (rng:ref 'end)))
                                  (it-check-parser "div { d = iv }" logsrc (<:rnc-grammar-content:>) (rng-div (list (rng-define 'd #\= (rng:ref 'iv)))))
                                  (it-check-parser "include 'target-only'" logsrc (<:rnc-grammar-content:>) (rng-include "target-only" #false null))
                                  (it-check-parser "include 'uri' { start = begin }" logsrc (<:rnc-grammar-content:>)
                                                   (rng-include "uri" #false (list (rng-start #\= (rng:ref 'begin)))))
                                  (it-check-parser "include 'uri' inherit = inherit { start = begin }" logsrc (<:rnc-grammar-content:>)
                                                   (rng-include "uri" 'inherit (list (rng-start #\= (rng:ref 'begin))))))
                        (describe "Pattern" #:do
                                  (it-check-parser "stupid-xml" logsrc (<:rnc-pattern:>) (rng:ref 'stupid-xml))
                                  (it-check-parser "notAllowed" logsrc (<:rnc-pattern:>) (rng:simple '#:notAllowed))
                                  (it-check-parser "parent mox" logsrc (<:rnc-pattern:>) (rng:parent 'mox))
                                  (it-check-parser "grammar { target = mox }" logsrc (<:rnc-pattern:>) (rng:grammar (list (rng-define 'target #\= (rng:ref 'mox)))))
                                  (it-check-parser "list { elem }" logsrc (<:rnc-pattern:>) (rng:element '#:list (list (rng:ref 'elem))))
                                  #;(it-check-parser "stupid-xml+" logsrc (<:rnc-pattern:>) (rng:element '#:+ (list (rng:ref 'stupid-xml))))))))
