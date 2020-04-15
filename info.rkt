#lang info
(define collection "lazytree")
(define deps '("base"
               "collections-lib"
               "relation"))
(define build-deps '("scribble-lib"
                     "scribble-abbrevs"
                     "racket-doc"
                     "rackunit-lib"
                     "sandbox-lib"))
(define scribblings '(("scribblings/lazytree.scrbl" (multi-page))))
(define clean '("compiled" "doc"))
(define pkg-desc "Lightweight, lazy trees.")
(define version "0.0")
(define pkg-authors '(countvajhula))
