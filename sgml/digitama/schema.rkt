#lang typed/racket/base

(provide (all-defined-out))

(require "digicore.rkt")
(require "grammar.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Schema-Attribute-Type (U XSch-Attribute-String-Type XSch-Attribute-Token-Type XSch-Attribute-Enum-Type))

(define-type Schema-Element-Sequence (Listof (Pairof (U Symbol Schema-Element-Children) Char)))
(define-type Schema-Element-Choice (Immutable-Vectorof (Pairof (U Symbol Schema-Element-Children) Char)))
(define-type Schema-Element-Children (U Schema-Element-Sequence Schema-Element-Choice))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct xsch-entity
  ([name : (U XML:Reference XML:PEReference)])
  #:transparent
  #:type-name XSch-Entity)

(struct xsch-internal-entity xsch-entity
  ([value : XML:String])
  #:transparent
  #:type-name XSch-Internal-Entity)

(struct xsch-token-entity xsch-internal-entity
  ([body : (Option (Listof XML-Token))])
  #:mutable
  #:type-name XSch-Token-Entity)

(struct xsch-external-entity xsch-entity
  ([public : (Option XML:String)]
   [system : (Option XML:String)])
  #:transparent
  #:type-name XSch-External-Entity)

(struct xsch-unparsed-entity xsch-external-entity
  ([ndata : XML:Name])
  #:transparent
  #:type-name XSch-Unparsed-Entity)

(struct xsch-notation
  ([name : XML:Name]
   [public : (Option XML:String)]
   [system : (Option XML:String)])
  #:transparent
  #:type-name XSch-Notation)

(struct xsch-attribute-string-type
  ()
  #:transparent
  #:type-name XSch-Attribute-String-Type)

(struct xsch-attribute-token-type
  ([name : XML:Name]
   [names? : Boolean])
  #:transparent
  #:type-name XSch-Attribute-Token-Type)

(struct xsch-attribute-enum-type
  ([options : (Pairof XML:Name (Listof XML:Name))]
   [?notation : (Option XML:Name)])
  #:transparent
  #:type-name XSch-Attribute-Enum-Type)

(struct xsch-attribute
  ([element : XML:Name]
   [name : XML:Name]
   [type : Schema-Attribute-Type])
  #:transparent
  #:type-name XSch-Attribute)

(struct xsch-attribute/required xsch-attribute
  ()
  #:transparent
  #:type-name XSch-Attribute/Required)

(struct xsch-attribute+default xsch-attribute
  ([value : XML:String]
   [fixed? : Boolean])
  #:transparent
  #:type-name XSch-Attribute+Default)

(struct xsch-element
  ([name : XML:Name])
  #:transparent
  #:type-name XSch-Element)

(struct xsch-empty-element xsch-element
  ()
  #:transparent
  #:type-name XSch-Empty-Element)

(struct xsch-mixed-element xsch-element
  ([children : (Listof Symbol)])
  #:transparent
  #:type-name XSch-Mixed-Element)

(struct xsch-element+children xsch-element
  ([content : (Pairof Schema-Element-Children Char)])
  #:transparent
  #:type-name XSch-Element+Children)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Schema-Entities (Immutable-HashTable (U Symbol Keyword) XSch-Entity))
(define-type Schema-Notations (Immutable-HashTable Symbol XSch-Notation))
(define-type Schema-Elements (Immutable-HashTable Symbol XSch-Element))
(define-type Schema-Attributes (Immutable-HashTable Symbol (Immutable-HashTable Symbol XSch-Attribute)))

(struct xml-schema
  ([entities : Schema-Entities]
   [notations : Schema-Notations]
   [elements : Schema-Elements]
   [attributes : Schema-Attributes])
  #:transparent
  #:type-name XML-Schema)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xsch:attribute:cdata : XSch-Attribute-String-Type (xsch-attribute-string-type))

(define xsch-empty-entities : Schema-Entities (make-immutable-hasheq))
(define xsch-empty-notations : Schema-Notations (make-immutable-hasheq))
(define xsch-empty-elements : Schema-Elements (make-immutable-hasheq))
(define xsch-empty-element-attributes : Schema-Attributes (make-immutable-hasheq))
(define xsch-empty-attributes : (Immutable-HashTable Symbol XSch-Attribute) (make-immutable-hasheq))
