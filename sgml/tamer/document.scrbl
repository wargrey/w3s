#lang scribble/manual

@(require digimon/tamer)

@handbook-typed-module-story[#:requires (sgml/tamer/whitespace) sgml/xml]{The XML Document}

The @deftech{XML Document} is composed of declarations, elements, comments, character references,
and processing instructions, all of which are indicated in the document by explicit markup.

@handbook-scenario{White Space Handling}

@tamer-action[
 (tamer-xml:space #\return #\newline
                  #\newline
                  #\return)]

@handbook-reference[]
