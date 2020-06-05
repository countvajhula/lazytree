#lang info
(define collection 'multi)
(define deps '("base"
               "collections-lib"
               "relation"))
(define build-deps '("scribble-lib"
                     "scribble-abbrevs"
                     "racket-doc"
                     "collections-doc"
                     "functional-doc"
                     "rackunit-lib"
                     "pict-lib"
                     "sandbox-lib"))
(define compile-omit-paths '("dev"))
(define clean '("compiled" "doc"))
(define pkg-desc "Lightweight, lazy trees.")
(define version "1.0")
(define pkg-authors '(countvajhula))
