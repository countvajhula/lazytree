#lang info
(define collection 'multi)
(define deps '("base"
               "collections-lib"
               "relation"
               "social-contract"))
(define build-deps '("scribble-lib"
                     "scribble-abbrevs"
                     "racket-doc"
                     "collections-doc"
                     "functional-doc"
                     "rackunit-lib"
                     "pict-lib"
                     "cover"
                     "cover-coveralls"
                     "sandbox-lib"))
;; at the moment, this flag needs to be at the package level in order
;; for it to take effect, possibly because the tests are run against
;; the package rather than the collection
(define test-omit-paths '("dev" "coverage"))
(define clean '("data/compiled")) ; via --fast-clean --pkgs lazytree
(define pkg-desc "Lightweight, lazy trees.")
(define version "1.1")
(define pkg-authors '(countvajhula))
