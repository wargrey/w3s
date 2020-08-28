#lang scribble/manual

@(require digimon/tamer)

@(define the-name (racketmodname sgml/xml))

@handbook-title/pkg-desc[]

@margin-note{Using @tt{xml} as the library name will conflict with other libraries in the realm of
 Racket, and I neither want to add a prefix @tt{typed-} to it, hence the @|the-name|. In the future,
 there would be another @tt{sgml/html} as well.}

The @the-name is a typed XML parsing library which originally designed for my @racketmodname[svg]
engine.

Meanwhile, @the-name will:

@itemlist[
 @item{provide a sort of APIs to read and deal with the @tech{XML document}, with or without tokens
  information.}
  
 @item{provide a Racket language @litchar{#lang sgml/xml} for embedding @tech{XML document}s in compiled
  files, as well as hightlighting XML files.}
 ]

@handbook-smart-table[]

@include-section{document.scrbl}

@handbook-appendix[#:index-section? #true
 (url-bib-entry "XML11" "Extensible Markup Language (XML) 1.1 (Second Edition)" "https://www.w3.org/TR/xml11"
                #:author "W3C"
                #:date 2006)]
