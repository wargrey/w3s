#lang scribble/manual

@(require digimon/tamer)

@(define the-name (racketmodname sgml/xml))

@handbook-title/pkg-desc[]

@margin-note{Using @tt{xml} as the library name will conflict with other libraries in the realm of
 Racket, and I neither want to add a prefix @tt{typed-} to it, hence the @|the-name|. In the future,
 there would be another @tt{sgml/html} as well.}

The @the-name is a typed XML parsing library which originally designed for my @racketmodname[svg]
engine. That is, this library itself is not adequate for dealing with XML sources that used as data
models.

Meanwhile, @|the-name|:

@itemlist[
 @item{provides a sort of APIs to read and deal with the @tech{XML document}, with or without source
  location information.}
  
 @item{provides a Racket language @litchar{#lang sgml/xml} for embedding @tech{XML Document}s in compiled
  files, as well as hightlighting XML files.}

 @item{provides a Racket language @litchar{#lang sgml/dtd} for embedding @tech{XML Document Type Definition}s
  in compiled files, as well as hightlighting DTD files.}                                             
 ]

@handbook-smart-table[]

@include-section{document.scrbl}

@handbook-bonus-appendix[#:index-section? #true
 (url-bib-entry "XML10" "Extensible Markup Language (XML) 1.0 (Fifth Edition)" "https://www.w3.org/TR/xml"
                #:author "W3C"
                #:date 2008)
 (book-bib-entry "EXML" "Effective XML" (authors "Elliotte Rusty Harold") "Addison-Wesley Professional"
                 #:date 2003
                 #:url "http://www.ibiblio.org/xml/books/effectivexml/chapters/03.html")]
