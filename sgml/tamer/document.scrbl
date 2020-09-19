#lang scribble/manual

@(require digimon/tamer)

@handbook-typed-module-story[#:requires (sgml/tamer/whitespace) sgml/xml]{The XML Document}

The @deftech{XML Document} is composed of declarations, elements, comments, character references,
and processing instructions, all of which are indicated in the document by explicit markup.

@handbook-scenario{White Space Handling}

@tamer-action[
 (tamer-xml:space 'preserve
                  #\return #\newline
                  #\newline
                  #\return)

 (tamer-svg:space 'default
                  #\return #\newline
                  #\newline
                  #\return)

 (tamer-svg:space 'default
                  #\tab
                  #\return #\newline
                  #\space)

 (tamer-svg:space 'preserve
                  #\return #\newline
                  #\newline
                  #\return
                  #\tab)]

@handbook-reference[]
