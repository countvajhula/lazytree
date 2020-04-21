#lang scribble/doc
@require[scribble/manual
         scribble-abbrevs/manual
         scribble/example
         racket/sandbox
         pict/private/layout
         @for-label[data/lazytree
                    (except-in racket
                               map
                               filter
                               sequence?)
                    (only-in data/collection
                             map
                             filter
                             sequence?)
                    (only-in relation
                             fold
                             ->list
                             /=)]]

@(define eval-for-docs
  (parameterize ([sandbox-output 'string]
                 [sandbox-error-output 'string]
                 [sandbox-memory-limit #f])
                 (make-evaluator 'racket/base
                                 '(require (except-in data/collection
                                                      append
                                                      index-of
                                                      foldl
                                                      foldl/steps))
                                 '(require relation)
                                 '(require data/lazytree)
                                 '(require racket/stream))))

@(define tree-layout-eval (make-base-eval))
@(tree-layout-eval '(require pict/tree-layout pict))
@(tree-layout-eval '(define node-pict (circle 15 #:border-width 2)))

@title{Lightweight, Lazy Trees}
@author{Siddhartha Kasivajhula}

@defmodule[data/lazytree]

Lightweight, general-purpose utilities for working with tree-structured data.

This module provides utilities to leverage the natural hierarchical structure of nested lists (and streams) to represent and perform computations on tree-structured data. The tree representation, in list form, is simply @codeblock{(data child ...)} where each child has the same structure. A single-element stream represents a leaf node, containing only data and no children. Any sequence with this structure is treatable as a tree for the purposes of the utilities provided here. For example, the list @codeblock{'(1 (2 (3) (4)) (5 (6)))} is a well-formed tree, with structure that could be visualized as:

@examples[
    #:eval tree-layout-eval
	#:result-only
	(naive-layered #:x-spacing 36 (tree-layout #:pict (text "1" null 18)
                                    (tree-layout #:pict (text "2" null 18)
                                      (tree-layout #:pict (text "3" null 18))
                                      (tree-layout #:pict (text "4" null 18)))
                                    (tree-layout #:pict (text "5" null 18)
                                      (tree-layout #:pict (text "6" null 18)))))
  ]

@defproc[(make-tree [f (-> any/c sequence?)]
                    [node any/c])
         sequence?]{

  Construct a tree from a node (which could be any value) and a function @racket[f] that yields the next level of the hierarchy (i.e. "children" or "parents") given an input node. The function @racket[f] is recursively -- and lazily -- applied starting from @racket[node] to yield a stream exhibiting the canonical tree structure.

@examples[
    #:eval eval-for-docs
    (struct taxon (name children))
    (define dog (taxon "Dog" '()))
    (define cat (taxon "Cat" '()))
    (define mammal (taxon "Mammal" (list dog cat)))
    (->list (tree-traverse (make-tree taxon-children mammal)))
  ]
}

@defproc[(tree-traverse [t sequence?]
                        [#:order order (one-of/c 'pre 'post 'in 'level) 'pre]
                        [#:converse? converse? boolean? #f])
         sequence?]{

  Traverse a tree using one of the @hyperlink["https://en.wikipedia.org/wiki/Tree_traversal"]{standard traversal orders}, i.e. preorder, postorder, in-order or level-order traversal. If @racket[converse?] is true, then traverses right-to-left instead of left-to-right. Although these traversals are canonically defined for binary trees, trees with an arity greater than two are still supported, using trivial generalizations of the binary tree versions. For instance, an in-order traversal would visit a single child prior to visiting the parent, and then visit all of the remaining children of that parent. See @hyperlink["http://ceadserv1.nku.edu/longa/classes/mat385_resources/docs/traversal.htm"]{here} for some helpful animations of tree traversals.

@examples[
    #:eval eval-for-docs
    (define t '(1 (2 (3) (4)) (5 (6))))
    (->list (tree-traverse #:order 'pre t))
    (->list (tree-traverse #:converse? #t #:order 'pre t))
    (->list (tree-traverse #:order 'post t))
    (->list (tree-traverse #:order 'in t))
    (->list (tree-traverse #:order 'level t))
    (struct taxon (name children))
    (define dog (taxon "Dog" '()))
    (define cat (taxon "Cat" '()))
    (define mammal (taxon "Mammal" (list dog cat)))
    (define t (make-tree taxon-children mammal))
    (->list (map taxon-name (tree-traverse #:order 'pre t)))
    (->list (map taxon-name (tree-traverse #:order 'post t)))
    (->list (map taxon-name (tree-traverse #:order 'in t)))
    (->list (map taxon-name (tree-traverse #:order 'level t)))
    (->list (map taxon-name (tree-traverse #:converse? #t #:order 'pre t)))
  ]
}

@defproc[(tree-map [f (-> any/c any/c)]
                   [t sequence?])
         sequence?]{

  Analogously to @racket[map], lazily maps each element in the tree under the function @racket[f].

@examples[
    #:eval eval-for-docs
    (struct taxon (name children))
    (define dog (taxon "Dog" '()))
    (define cat (taxon "Cat" '()))
    (define mammal (taxon "Mammal" (list dog cat)))
    (define t (make-tree taxon-children mammal))
    (first (tree-map taxon-name t))
  ]
}

@defproc[(tree-filter [f (-> any/c boolean?)]
                      [t sequence?])
         sequence?]{

  Analogously to @racket[filter], lazily filters each element in the tree under the function @racket[f]. If a node is filtered out, none of its descendants are present in the resulting tree.

@examples[
    #:eval eval-for-docs
    (struct taxon (name children))
    (define dog (taxon "Dog" '()))
    (define cat (taxon "Cat" '()))
    (define mammal (taxon "Mammal" (list dog cat)))
    (define t (make-tree taxon-children mammal))
    (->list (tree-traverse (tree-map taxon-name (tree-filter (lambda (v) (/= (taxon-name v) "Dog")) t))))
  ]
}

@defproc[(tree-fold [f (-> any/c any/c any/c)]
                    [t sequence?]
                    [base any/c]
                    [#:order order (one-of/c 'pre 'post 'in 'level) 'pre]
                    [#:converse? converse? boolean? #f]
                    [#:argument-order argument-order (one-of/c 'abb 'bab) 'abb]
                    [#:with-steps? with-steps? boolean? #f])
         any/c]{

  Analogously to @racket[fold], combines elements of the tree using the function @racket[f]. While normal folds have a left or right direction, the direction of a tree fold is determined by the traversal order, which is specified via @racket[order].

@examples[
    #:eval eval-for-docs
    (struct taxon (name children))
    (define dog (taxon "Dog" '()))
    (define cat (taxon "Cat" '()))
    (define mammal (taxon "Mammal" (list dog cat)))
    (define t (make-tree taxon-children mammal))
    (tree-fold (lambda (x xs) (.. (taxon-name x) xs)) t "")
    (tree-fold #:order 'post (lambda (x xs) (.. (taxon-name x) xs)) t "")
  ]
}
