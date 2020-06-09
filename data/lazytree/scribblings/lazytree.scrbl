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
                             false.
                             ->list
                             /=
                             ..)]]

@(define eval-for-docs
  (parameterize ([sandbox-output 'string]
                 [sandbox-error-output 'string]
                 [sandbox-memory-limit #f])
                 (make-evaluator 'racket/base
                                 '(require racket/math
                                           (except-in data/collection
                                                      append
                                                      index-of
                                                      foldl
                                                      foldl/steps)
                                           relation
                                           data/lazytree
                                           racket/stream))))

@(define tree-layout-eval (make-base-eval))
@(tree-layout-eval '(require pict/tree-layout pict))
@(tree-layout-eval '(define node-pict (circle 15 #:border-width 2)))

@title{Lightweight, Lazy Trees}
@author{Siddhartha Kasivajhula}

@defmodule[data/lazytree]

Lightweight, general-purpose utilities for working with tree-structured data.

This module provides a means to leverage the natural hierarchical structure of nested lists (and streams) to represent and perform computations on arbitrary tree-structured data. By using it, applications can keep tree-related operations abstracted, without needing to re-implement standard tree operations for every tree-structured data type or even explicitly represent the tree at all. Additionally, this module provides utilities to conveniently translate between different tree representations, so that these utilities can be used regardless of source format, and the results of computations can be translated into any desired output format.

@section{Schema}

In order to use the utilities in this module, the tree-structured data must first be translated to a canonical format or schema by using the @racket[make-tree] interface. This schema is simply @codeblock{(data child ...)}, a stream, where each child has the same structure. A single-element stream represents a leaf node, containing only data and no children. An empty stream represents an empty tree. Any sequence with this structure is treatable as a tree for the purposes of the utilities provided here.

As an example, the list @codeblock{'(1 (2 (3) (4)) (5 (6)))} is a well-formed tree, with structure that could be visualized as:

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

Trees of this schema may be translated to any format (such as the original source format) by using the @racket[export-tree] interface. As @racket[make-tree] likewise enables converting any input tree representation to this schema, these two interfaces could be used in tandem to translate between two different data representations involving the tree-structured data, independently of any of the other utilities provided by this module.

@section{API}

@defproc[(make-tree [f (-> any/c sequence?)]
                    [node any/c]
					[#:with-data dataf identity]
					[#:empty-pred empty-pred false.])
         sequence?]{

  Construct a tree from a node (which could be any value) and a function @racket[f] that yields the next level of the hierarchy (i.e. "children" or "parents") given an input node. The function @racket[f] is recursively -- and lazily -- applied starting from @racket[node] to yield a stream exhibiting the @seclink["Schema"]{canonical tree structure}. By default, the "data" contained in the tree are @emph{the provided node objects themselves} (whatever they may be). In many cases it may be more natural to use the relevant "contents" of the original tree in the data position, rather than the node itself. This may be specified providing an appropriate data accessor via @racket[#:with-data]. If the input format includes sentinel values to indicate an empty tree, then a predicate to recognize these empty trees should be provided via @racket[#:empty-pred].

@examples[
    #:eval eval-for-docs
    (struct taxon (name children))
    (define dog (taxon "Dog" '()))
    (define cat (taxon "Cat" '()))
    (define mammal (taxon "Mammal" (list dog cat)))
    (export-tree list (make-tree taxon-children mammal))
    (export-tree list (make-tree taxon-children
                                 mammal
                                 #:with-data taxon-name))
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
    (define t (make-tree taxon-children
                         mammal
                         #:with-data taxon-name))
    (->list (tree-traverse #:order 'pre t))
    (->list (tree-traverse #:order 'post t))
    (->list (tree-traverse #:order 'in t))
    (->list (tree-traverse #:order 'level t))
    (->list (tree-traverse #:converse? #t #:order 'pre t))
  ]
}

@defproc[(tree-map [f (-> any/c any/c)]
                   [t sequence?])
         sequence?]{

  Analogously to @racket[map], lazily maps each element in the tree under the function @racket[f].

@examples[
    #:eval eval-for-docs
    (define t '(1 (2 (3) (4)) (5 (6))))
    (export-tree list (tree-map sqr t))
    (struct taxon (name children))
    (define dog (taxon "Dog" '()))
    (define cat (taxon "Cat" '()))
    (define mammal (taxon "Mammal" (list dog cat)))
    (define t (make-tree taxon-children mammal))
    (export-tree list (tree-map taxon-name t))
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
    (define t (make-tree taxon-children
                         mammal
                         #:with-data taxon-name))
    (export-tree list
                 (tree-filter (lambda (v)
                                (/= v "Dog"))
                              t))
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
    (define t (make-tree taxon-children
                         mammal
                         #:with-data taxon-name))
    (tree-fold .. t)
    (tree-fold #:order 'post .. t)
  ]
}

@defproc[(export-tree [f procedure?]
                      [tree sequence?]
					  [#:empty-cons empty-cons #f])
         sequence?]{

  Export a tree to an arbitrary output format, as specified by the output type constructor @racket[f]. In constructing the output tree, @racket[f] will be invoked for each node in @racket[tree] with the arguments @racket[data child ...] representing that node (see @secref{Schema}), in order to construct the corresponding node in the output tree. Note that these arguments are provided to @racket[f] directly rather than as a list. For example, if the output format is a struct type with named subtrees such as "left" and "right," @racket[f] in this case would typically just be the struct type constructor. In cases where named subtrees are present and could be empty, each corresponding empty tree in the output type is constructed using @racket[empty-cons], which is expected to be a function that takes no arguments and returns an empty tree instance. If @racket[empty-cons] is @racket[#f], the empty subtrees are excluded from the result.

  As @racket[make-tree] supports any input tree representation and @racket[export-tree] supports any output representation, these two interfaces could be used in tandem to translate between two different data representations involving the tree-structured data, independently of any of the other utilities provided by this module.

@examples[
    #:eval eval-for-docs
    (export-tree list
                 (make-tree rest
                            '(1 (2 (3) (4)) (5 (6)))
                            #:with-data first))
    (struct taxon (name children) #:transparent)
    (define dog (taxon "Dog" '()))
    (define cat (taxon "Cat" '()))
    (define mammal (taxon "Mammal" (list dog cat)))
    (export-tree (λ tree
                   (taxon (first tree)
                          (rest tree)))
                 (make-tree taxon-children
                            mammal
                            #:with-data taxon-name))
    (export-tree list
                 (make-tree taxon-children
                            mammal
                            #:with-data taxon-name))
    (struct node (data left right)
      #:transparent)
    (struct empty-tree ()
      #:transparent)
    (define (node-children t)
      (list (node-left t)
            (node-right t)))
    (define tree (node 1
                       (node 2
                             (empty-tree)
                             (node 3
                                   (empty-tree)
                                   (empty-tree)))
                       (empty-tree)))
    (export-tree node
                 (make-tree node-children
                            tree
                            #:with-data node-data
                            #:empty-pred empty-tree?)
                 #:empty-cons empty-tree)
    (struct node-too (data children)
      #:transparent)
    (export-tree (λ tree
                   (node-too (first tree)
                             (rest tree)))
                 (make-tree node-children
                            tree
                            #:with-data node-data
                            #:empty-pred empty-tree?))
  ]
}
