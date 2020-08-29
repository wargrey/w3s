# SGML Dialect: An XML Processor Written in Typed Racket

wargrey

> Using `xml` as the library name will conflict with other libraries in
> the realm of Racket, and I neither want to add a prefix `typed-` to it,
> hence the `sgml/xml`. In the future, there would be another `sgml/html`
> as well.

The `sgml/xml` is a typed XML parsing library which originally designed
for my `svg` engine. That is, this library itself is not adequate for
dealing with XML sources that used as data models.

Meanwhile, `sgml/xml` will:

* provide a sort of APIs to read and deal with the XML document, with or
  without tokens information.

* provide a Racket language `#lang sgml/xml` for embedding XML documents
  in compiled files, as well as hightlighting XML files.


