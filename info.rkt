#lang setup/infotab
(define collection 'multi)
(define deps
  (list "base"
        "compatibility-lib"
        "macro-debugger"
        "macro-debugger-text-lib"
        "pconvert-lib"
        "sandbox-lib"
        "scribble-lib"
        "srfi-lib"
        "srfi-lite-lib"))
(define build-deps
  (list "compatibility-doc"
        "data-doc"
        "racket-doc"
        "scribble-doc"
        "racket-index"
        "rackunit-gui"
        "rackunit-lib"))
