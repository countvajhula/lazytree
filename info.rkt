#lang info
(define collection 'multi)
(define deps '("base"
               "collections-lib"
               "Relation"))
(define build-deps '("scribble-lib"
                     "scribble-abbrevs"
                     "racket-doc"
                     "collections-doc"
                     "functional-doc"
                     "rackunit-lib"
                     "sandbox-lib"))
(define scribblings '(("scribblings/lazytree.scrbl" (multi-page))))
(define clean '("compiled" "doc"))
(define pkg-desc "Lightweight, lazy trees.")
(define version "0.0")
(define pkg-authors '(countvajhula))
