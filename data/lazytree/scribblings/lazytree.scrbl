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
                               first
                               rest
                               sequence?)
                    (only-in data/collection
                             map
                             filter
                             first
                             rest
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

  Construct a tree from a node (which could be any value) and a function @racket[f] that yields the next level of the hierarchy (i.e. "children" or "parents") given an input node. The function @racket[f] is recursively -- and lazily -- applied starting from @racket[node] to yield a stream exhibiting the @seclink["Schema"]{canonical tree structure}. By default, the "data" contained in the tree are @emph{the provided node objects themselves} (whatever they may be). In many cases it may be more natural to use the relevant "contents" of the original tree in the data position, rather than the node itself. This may be specified providing an appropriate data accessor via @racket[#:with-data]. If the input format includes sentinel values to indicate an empty tree, then a predicate to recognize these empty trees should be provided via @racket[#:empty-pred]. Supplementary to the examples below, see @racket[export-tree] for more thorough examples leveraging these parameters.

@margin-note{@bold{What if my data has cycles in it?} Then the data is technically a graph rather than a tree, but with some cunning you can still use these interfaces to perform tree-structured operations on it. First, you'll need to ensure that the function @racket[f] provided to @racket[make-tree] excludes "visited" nodes so that such cycles are not present in the resulting tree (@racket[f] would probably need to be a @hyperlink["https://www.gnu.org/software/guile/manual/html_node/Closure.html"]{closure} to keep track of visited nodes). Additionally, bear in mind that this derived tree wouldn't contain all of the edges from the original data, so exporting it via @racket[export-tree] would not reconstruct the original data but the derived tree representation.}

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

@deftogether[(
@defproc[(data [t sequence?])
         any/c]
@defproc[(children [t sequence?])
         sequence?]
)]{
  Aliases for @racket[first] and @racket[rest], respectively, these access the data contained in a node and the children of the node, respectively. Note that "node" and "tree" are different ways of talking about the same thing since nodes in the tree representation are always trees themselves. The result of @racket[data] could be any value, while @racket[children] evaluates to a sequence of nodes or trees that are the immediate children of @racket[t].

@examples[
    #:eval eval-for-docs
	(data '(1 (2 (3) (4)) (5 (6))))
	(children '(1 (2 (3) (4)) (5 (6))))
	(->list (map data (children '(1 (2 (3) (4)) (5 (6))))))
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

  See @racket[tree-accumulate] for another way to fold over trees.

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

@defproc[(tree-accumulate [f (-> any/c ... any/c)]
                          [t sequence?])
         any/c]{

  Similarly to @racket[tree-fold], this combines elements of the tree using the function @racket[f], but it does so respecting the hierarchy of the tree rather than in terms of a linear ordering on the tree. The first argument to @racket[f] will be the data value for a particular node, and the remaining arguments will be the accumulated results from each subtree at that level. Since the number of children of each node may vary in general (and e.g. leaf nodes have none), @racket[f] must be variadic, i.e. it must accept an arbitrary number of arguments.

  To learn more about how this interface works, see @hyperlink["https://www.cs.cornell.edu/courses/cs3110/2019sp/textbook/hop/fold_trees.html"]{Fold with Trees}.

@examples[
    #:eval eval-for-docs
    (define t '(1 (2 (3) (4)) (5 (6))))
    (tree-accumulate + t)
    (define (size t)
      (tree-accumulate (λ (v . vs)
                          (apply + 1 vs))
                       t))
    (size t)
    (define (depth t)
      (tree-accumulate (λ (v . vs)
                          (if (empty? vs)
                              1
                              (add1 (apply max vs))))
                       t))
    (depth t)
    (define (preorder t)
      (tree-accumulate (λ (v . vs)
                          (if (empty? vs)
                              (list v)
                              (join (append (list (list v)) vs))))
                       t))
    (preorder t)
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

@section{Working With Symbolic Expressions}

A common type of data that you might work with in Lisp-land is a @deftech{symex}, also known as a symbolic expression, S-expression, or, as it is sometimes called in Racket, a @tech/reference{datum}. A symex is just the usual syntax in the Lisp family of languages, and is defined recursively as either being a list of symexes or an atom, the latter class of which forms the syntactic base case and includes e.g. literals and identifiers. We have already seen that nested lists are naturally tree-structured, and this means that we can use the interfaces in this module to manipulate them, although the structure with symexes is a little different from the canonical nested list representation we have been using.

Whatever the format of the tree-structured data, in order to use these interfaces on it, we first need to extract a canonical tree representation using @racket[make-tree]. To do this, we must ask, "What are the nodes in this representation? And how do we get the children of each node?" Let's look at an example: @racket[(+ 1 (* 2 3))]. There are a couple of different choices we could make here. The first is to treat the operator as the data in the node and the operands as the children. Another choice we could make is to consider sub-expressions of this expression to correspond to nodes in the tree, with the whole expression itself being the data content of the root node. Each of these options could be useful in different cases; let's look at them in turn.

In the first option, since the operator is always first in a symex, with the remaining elements being operands, we'd like to use @racket[first] as the data function, and @racket[rest] as @racket[f] to indicate the children in the @racket[make-tree] interface. But symexes could also be atoms and not just lists, so we'd need to handle this case as well. Specifically, if the symex is an atom, we'd want to use the expression itself as the data, and @racket[null] as the list of children.

As an example, we could use such a representation to write a simple "tree accumulation" style Lisp interpreter.

@examples[
    #:label #f
    #:eval eval-for-docs
    (define expression '(+ (* 2 3) (- 10 (* 3 12))))
    (eval expression)
    (define t
      (make-tree #:with-data (λ (v)
                               (if (list? v)
                                   (first v)
                                   v))
                 (λ (v)
                   (if (list? v)
                       (rest v)
                       null))
                 expression))
    (define (tree-eval v . args)
      (if (empty? args)
          (eval v)
          (apply (eval v) args)))
    (define (my-eval expr)
      (tree-accumulate tree-eval expr))
    (my-eval t)
  ]

For the second option, we seek to treat the expressions themselves as the data contents of each node. Since the default behavior with @racket[make-tree] is already to treat the input itself as the data in the node, it only remains to pass in an appropriate function @racket[f] which will produce a list of the children of a given node. For the example here, it would seem that this function should be @racket[identity] (or equivalently, @racket[values]) since the expression itself is a list consisting of the children we've identified. But what about the sub-expression @racket[+] which is an atomic node? The function @racket[f] must return a @racket[list] of children, but calling @racket[values] on @racket[+] does not produce a list. In this case, what we want is for the function to produce the empty list, since the node @racket[+] has no sub-expressions, i.e. no children. So, what we are looking for is a function that returns the input if the input is a list, and the empty list if it is an atom, that is to say, something like @racket[(λ (v) (if (list? v) v null))].

In the previous example we saw that the first representation allowed us to implement a simple Lisp interpreter. We got the answer we were looking for, but what if we wanted to see the steps involved in the evaluation? We could emulate these steps in the second representation by traversing the tree using an appropriate traversal and evaluating the results, since each node in this representation represents an entire subexpression used in computing the overall result.

@examples[
    #:label #f
    #:eval eval-for-docs
    (define expression '(+ (* 2 3) (- 10 (* 3 12))))
    (eval expression)
    (->list
     (map eval
          (tree-traverse #:order 'post
                         #:converse? #t
                         (make-tree (λ (v)
                                      (if (list? v)
                                          v
                                          null))
                                    expression))))
  ]

The interfaces in this module may also be useful in writing parsers and macros, where you may need to transform syntax (trees) in a structured way.
