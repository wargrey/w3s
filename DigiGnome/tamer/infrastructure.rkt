#lang scribble/lp2

@(require "tamer.rkt")

@(require (for-syntax "tamer.rkt"))

@(tamer-story (tamer-story->libpath "infrastructure.rkt"))
@(define partner `(file ,(path->string (car (filter file-exists? (list (collection-file-path "makefile.rkt" (digimon-gnome))
                                                                       (build-path (digimon-world) "makefile.rkt")))))))
@(tamer-zone (make-tamer-zone))

@handbook-story{Hello, Hacker Hero!}

Every hacker needs a robust developing infrastructure, more clearly meanwhile I mean @hyperlink[@(cadr partner)]{@italic{makefile.rkt}}
to make life simple. However testing the building routines on the entire project always makes nonsense but costs high,
thus I will focus on @seclink[@(digimon-gnome)]{the meta-project @(digimon-gnome)} and @seclink["rules"]{project organization rules}.

@chunk[|<infrastructure story>|
       {module story racket
         |<infrastructure taming start>|
         
         |<ready? help!>|
         |<hello rules!>|}
       
       |<tamer battle>|]

where @chunk[|<infrastructure taming start>|
             (require "tamer.rkt")
             (require setup/getinfo)
             
             (tamer-story (tamer-story->libpath "infrastructure.rkt"))
             (define partner `(file ,(format "~a/makefile.rkt" (digimon-world))))]

@tamer-smart-summary[]

@handbook-scenario[#:tag @(digimon-gnome)]{Ready? Let@literal{'}s have a try!}

@chunk[|<ready? help!>|
       (define-values {make out err $?}
         (values (dynamic-require partner 'main {λ _ #false})
                 (open-output-bytes 'stdout)
                 (open-output-bytes 'stderr)
                 (make-parameter +NaN.0)))

       |<setup and teardown timidly>|

       (define-tamer-suite spec-examples "Ready? It works!"
         (list (test-suite "make: simple options"
                           |<testsuite: simple options>|)
               (test-suite "make: complex options"
                           |<testcase: complex options>|)))]

You may have already familiar with the @hyperlink["http://en.wikipedia.org/wiki/Make_(software)"]{GNU Make},
nonetheless you are still free to check the options first. Normal @bold{Racket} program always knows
@exec{@|-~-|h} or @exec{@|-~-|@|-~-|help} option:

@tamer-action[((dynamic-require/expose (tamer-story) 'make) "--help")
              (code:comment @#,t{See, @racketcommentfont{@italic{makefile}} complains that @racketcommentfont{@bold{Scribble}} is killed by accident.})]

Be careful here, the buggy implementation may keep invoking another test routine endlessly in which case
this @italic{handbook} itself may be depended by some other files.
Kill those unexpected routines crudely is unreasonable since they will run in their own namespace and may have side effects.

@chunk[|<setup and teardown timidly>|
       (define ENV (current-environment-variables))
       (when (environment-variables-ref ENV #"taming")
         (error 'make "[fatal] Unexpected subroutine stops here!"))

       (define {{setup . argv}}
         (dynamic-wind {λ _ (environment-variables-set! ENV #"taming" #"true")}
                       {λ _ (parameterize ([current-directory (digimon-world)]
                                           [current-output-port out]
                                           [current-error-port err]
                                           [exit-handler $?])
                              (apply make argv))}
                       {λ _ (environment-variables-set! ENV #"taming" #false)}))

       (define {teardown}
         (get-output-bytes out #true)
         (get-output-bytes err #true)
         ($? +NaN.0))]

Now let@literal{'}s try to make thing done:

@tamer-note['spec-examples]
@chunk[|<testsuite: simple options>|
       (test-suite "make --silent --help"
                   #:before (setup "--silent" "--help") #:after teardown
                   (test-pred "should exit normally" zero? ($?))
                   (test-pred "should keep quiet" zero? (file-position out)))
       (test-suite "make --silent love"
                   #:before (setup "--silent" "love") #:after teardown
                   (test-true "should abort" ((negate zero?) ($?)))
                   (test-pred "should report errors" positive? (file-position err)))]

It seems that it works very well, and so it does.
But the @italic{before} and @italic{after} routines are out of testcase
in which case the following call for the same testsuite may waste too much time
since only the results of testcases are cached during the process of rendering the @italic{handbook}.

So, a testcase with the additional work sealed inside would be better: 

@chunk[|<testcase: complex options>|
       (let* ([goal-md (build-path (digimon-world) "README.md")]
              [make-md (list "--always-make" "--touch" "++only" (digimon-gnome)
                             (path->string (file-name-from-path "README.md")))])
         (test-spec (string-join (cons "make" make-md))
                    #:do (apply setup make-md) #:~do teardown
                    (let* ([stdout (get-output-string out)]
                           [stderr (get-output-string err)]
                           [times (compose1 length (curryr regexp-match* stdout))]
                           [msecs file-or-directory-modify-seconds])
                      (check-pred zero? ($?) stderr)
                      (check-pred zero? (file-position err) stderr)
                      (check-eq? (times #px"Leave Digimon Zone") 1
                                 "worked in wrong digimon zone!")
                      (check-eq? (times #px"make: made") 1
                                 "too many files are made!")
                      (check <= (msecs (digimon-zone)) (msecs goal-md)
                             "target file has not updated!")
                      (check-pred zero? (times #px"Output to")
                                  "touching is okay, not remaking!"))))]

@subsection[#:tag "rules"]{Scenario: The rules serve you!}

Since the term @italic{Architecture} is all about designing rules, and this story is all about building system.
So apart from @italic{@hyperlink["https://github.com/digital-world/DigiGnome"]{conventions}},
we need a sort of rules that the @italic{makefile.rkt} (and systems it builds) should satisfy.

@chunk[|<hello rules!>|
       (define digimons (parameterize ([current-directory (digimon-world)])
                          (for/list ([dirname (in-list (directory-list))]
                                     #:when (directory-exists? dirname)
                                     #:when (regexp-match? #px"^[^.]" dirname))
                            dirname)))
       
       |<rules: info.rkt>|
       |<rules: readme.md>|]

@subsubsection{Rules on project organization}

@chunk[|<rules: info.rkt>|
       (define-tamer-suite rules:info.rkt "Rules: info.rkt settings"
         (cons (let ([info-ref (get-info/full (digimon-world))])
                 (test-suite "/info.rkt" (test-case |<facts: rule 1>|)))
               (for/list ([digimon (in-list digimons)])
                 (define info-ref (get-info/full (build-path (digimon-world) digimon)))
                 (test-suite (format "/~a/info.rkt" digimon)
                             (test-case |<facts: rule 2>|)
                             (test-case |<facts: rule 3>|)
                             (test-case |<facts: rule 4>|)))))]

@tamer-note['rules:info.rkt]
@handbook-rule[1]{The entire project is a multi-collection package, non-hidden directories within it are considered as the subprojects.}
@chunk[|<facts: rule 1>|
       "Rule 1: multi"
       (check-equal? (info-ref 'collection) 'multi
                     "'collection should be 'multi!")
       (check-pred positive? (length digimons)
                   "No real project found!")]

@handbook-rule[2]{Each subproject should have an explicit name, even if the name is the same as its directory.}
@chunk[|<facts: rule 2>|
       "Rule 2: collection"
       (check-pred string? (info-ref 'collection)
                   "'collection should be string!")]

@handbook-rule[3]{@racket[compile-collection-zos] and friends should never touch special paths.}
@chunk[|<facts: rule 3>|
       "Rule 3: compile-omit-paths"
       (for ([omit (in-list (list* "makefile.rkt" "submake.rkt" "info.rkt"
                                   (map (compose1 path->string file-name-from-path)
                                        (list (digimon-stone) (digimon-tamer)))))])
         (check-not-false (let ([maybe-omits (info-ref 'compile-omit-paths)])
                            (or (equal? maybe-omits 'all) (member omit maybe-omits)))
                          (format "'compile-omit-paths should contain ~a!" omit)))]

@handbook-rule[4]{@exec{raco test} should do nothing since we would do testing in a more controllable way.}
@chunk[|<facts: rule 4>|
       "Rule 4: test-omit-paths"
       (check-equal? (info-ref 'test-omit-paths) 'all
                     "'test-omit-paths should be 'all!")]

@subsubsection{Rules on project documentation}

@chunk[|<rules: readme.md>|
       (match-define {list top.scrbl sub.scrbl}
         (map (compose1 (curry find-relative-path (digimon-zone)) build-path)
              (list (digimon-stone) (digimon-tamer))
              (list "readme.scrbl" "handbook.scrbl")))
       
       (define-tamer-suite rules:readme.md "Rules: README.md dependencies"
         (cons (test-suite "/README.md" (test-case |<facts: rule 5>|))
               (for/list ([digimon (in-list digimons)])
                 (test-suite (format "/~a/readme.md" digimon)
                             (test-case |<facts: rule 6>|)))))]

@tamer-note['rules:readme.md]
@handbook-rule[5]{The project@literal{'}s toplevel @italic{README.md} is designated as the @italic{main-toc} of @bold{Scribble}.}
@chunk[|<facts: rule 5>|
       (format "Rule 5: ~a/~a" (digimon-gnome) top.scrbl)
       (check-pred file-exists? (build-path (digimon-zone) top.scrbl))]

@handbook-rule[6]{Each subproject@literal{'}s @italic{README.md} follows its @italic{handbook}@literal{'}s index page.}
@chunk[|<facts: rule 6>|
       (format "Rule 6: ~a/~a" digimon sub.scrbl)
       (check-pred file-exists? (build-path (digimon-world) digimon sub.scrbl))]

@handbook-scenario{What if the @italic{handbook} is unavaliable?}

Furthermore, the @italic{handbook} itself is the standard test report, but it@literal{'}s still reasonable
to check the system in some more convenient ways. Hence we have @chunk[|<tamer battle>|
                                                                       {module main racket
                                                                         |<infrastructure taming start>|
                                                                         
                                                                         (exit-with-code (tamer-spec))}]

Run @exec{racket «@smaller{tamer files}»} we will get @hyperlink["http://hspec.github.io"]{@italic{hspec-like}} report.

Technically speaking, @exec{raco test --submodule main} is always there,
although that way is not recommended, and is omitted by @filepath{info.rkt}.