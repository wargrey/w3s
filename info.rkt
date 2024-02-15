#lang info

(define collection 'multi)
(define pkg-authors '(wargrey))

(define pkg-desc "W3 Standards Implemented in Typed Racket")
(define version "1.0")

(define deps '("base" "digimon" "graphics" "typed-racket-lib" "typed-racket-more"))
(define build-deps '("scribble-lib" "racket-doc" "typed-racket-doc" "digimon" "graphics"))
(define test-omit-paths 'all)
