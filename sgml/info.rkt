#lang info

(define collection 'use-pkg-name)
(define pkg-desc "SGML Dialect: An XML/DTD Processor Written in Typed Racket")

(define version "1.0")

(define scribblings '(["tamer/sgml.scrbl" (main-doc multi-page) (parsing-library)]))

(define module-suffixes '(#"txml" #"tdtd"))
