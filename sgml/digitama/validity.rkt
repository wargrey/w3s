#lang typed/racket/base

(provide (all-defined-out))

(require typed/racket/unsafe)

(require "dtd.rkt")
(require "digicore.rkt")
(require "grammar.rkt")

(unsafe-require/typed
 racket/base
 [mpair? (-> Any Boolean : XML-Processing-Instruction*)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-validate : (-> XML-Type (Listof XML-Content*) Boolean Boolean)
  (lambda [dtype contents standalone?]
    (define ELEMENT : DTD-Elements (xml-type-elements dtype))
    (define ATTLIST : DTD-Attributes (xml-type-attributes dtype))
    
    (let verify ([rest : (Listof XML-Content*) contents]
                 [valid? : Boolean #false]
                 [nroot : Natural 0])
      (cond [(null? rest) (and valid? (eq? nroot 1))]
            [else (let-values ([(self rest++) (values (car rest) (cdr rest))])
                    (cond [(mpair? self) (verify rest++ valid? nroot)]
                          [else (when (> nroot 0) (make+exn:xml:multi-root (car self)))
                                (verify rest++
                                        (and (xml-validate-element self ELEMENT ATTLIST standalone?) valid?)
                                        (+ nroot 1))]))]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-validate-element : (-> XML-Element* DTD-Elements DTD-Attributes Boolean Boolean)
  (lambda [elem elements attributes standalone?]
    #false))
