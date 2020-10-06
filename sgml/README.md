# SGML Dialect: An XML/DTD Processor Written in Typed Racket

wargrey

> Using `xml` as the library name will conflict with other libraries in
> the realm of Racket, and I neither want to add a prefix `typed-` to it,
> hence the `sgml/xml`. In the future, there would be another `sgml/html`
> as well.

The `sgml/xml` is a typed XML parsing library which originally designed
for my `svg` engine. That is, this library itself is not adequate for
dealing with XML sources that used as data models.

Meanwhile, `sgml/xml`:

* provides a sort of APIs to read and deal with the XML document, with
  or without source location information.

* provides a Racket language `#lang sgml/xml` for embedding XML
  Documents in compiled files, as well as hightlighting XML files.

* provides a Racket language `#lang sgml/dtd` for embedding XML Document
  Type Definitions in compiled files, as well as hightlighting DTD
  files.


