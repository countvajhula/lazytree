#lang racket/base

(require (except-in racket/contract/base
                    predicate/c)
         racket/stream
         racket/undefined
         (only-in racket/function
                  identity)
         (except-in data/collection
                    foldl
                    foldl/steps
                    append
                    index-of)
         contract/social
         relation)

(require "lazytree/private/util.rkt")

(provide (contract-out
          [make-tree (->* ((encoder/c sequence?)
                           any/c)
                          (#:with-data procedure?
                           #:empty-pred predicate/c)
                          sequence?)]
          [export-tree (->* (procedure?
                             sequence?)
                            (#:empty-cons (maybe/c thunk/c))
                            any/c)]
          [tree-traverse (->* (sequence?)
                              (#:order (one-of/c 'pre
                                                 'post
                                                 'in
                                                 'level)
                               #:converse? boolean?)
                              sequence?)]
          [tree-map mapper/c]
          [tree-filter filter/c]
          [tree-fold (->* ((binary-composition/c any/c) sequence?)
                          (any/c
                           #:order (one-of/c 'pre
                                             'post
                                             'in
                                             'level)
                           #:converse? boolean?
                           #:argument-order (one-of/c 'abb
                                                      'bab)
                           #:with-steps? boolean?)
                          any/c)]))

(module+ test
  (require rackunit
           rackunit/text-ui
           racket/stream
           (except-in data/collection
                      foldl
                      foldl/steps
                      append
                      index-of)
           relation))

(define (make-tree f
                   node
                   #:with-data [dataf identity]
                   #:empty-pred [empty-pred false.])
  ;; lazily derive a tree in the canonical format
  ;; T = (data child ...) from the source format
  (if (empty-pred node)
      empty-stream
      (stream-cons (dataf node)
                   (map (curry make-tree
                               f
                               #:with-data dataf
                               #:empty-pred empty-pred)
                        (f node)))))

(define (export-tree f
                     tree
                     #:empty-cons [empty-cons #f])
  ;; export (data child ...) to the source format
  (if (empty? tree)
      (if empty-cons (empty-cons) '())
      (apply f
             (first tree)
             (filter (if empty-cons
                         true.
                         (!! null?))
                     (map (curry export-tree
                                 f
                                 #:empty-cons empty-cons)
                          (rest tree))))))

(define (tree-map f tree)
  (if (empty? tree)
      empty-stream
      (stream-cons (f (first tree))
                   (map (curry tree-map f)
                        (rest tree)))))

(define (tree-filter f tree)
  (if (empty? tree)
      empty-stream
      (if (f (first tree))
          (stream-cons (first tree)
                       (remove-when empty?
                                    (map (curry tree-filter f)
                                         (rest tree))))
          empty-stream)))

(define (tree-fold f
                   tree
                   [base undefined]
                   #:order [order 'pre]
                   #:converse? [converse? #f]
                   #:argument-order [argument-order 'abb]
                   #:with-steps? [with-steps? #f])
  (foldl f
         (tree-traverse tree
                        #:order order
                        #:converse? converse?)
         #:into base
         #:order argument-order
         #:with-steps? with-steps?))

(define (tree-traverse-preorder tree
                                #:converse? [converse? #f])
  (if (empty? tree)
      empty-stream
      (stream-cons (first tree)
                   (->stream  ; handle `ID`
                    (join (map (curry tree-traverse-preorder
                                      #:converse? converse?)
                               (if converse?
                                   (reverse (rest tree))
                                   (rest tree))))))))

(define (tree-traverse-postorder tree
                                 #:converse? [converse? #f])
  (if (empty? tree)
      empty-stream
      (.. (join (map (curry tree-traverse-postorder
                            #:converse? converse?)
                     (if converse?
                         (reverse (rest tree))
                         (rest tree))))
          (stream (first tree)))))

(define (tree-traverse-inorder tree
                               #:converse? [converse? #f])
  (if (empty? tree)
      empty-stream
      (if (empty? (rest tree))
          (stream (first tree))
          (let ([children (if converse?
                              (reverse (rest tree))
                              (rest tree))])
            (apply ..
                   (tree-traverse-inorder (first children)
                                          #:converse? converse?)
                   (stream (first tree))
                   (map (curry tree-traverse-inorder
                               #:converse? converse?)
                        (rest children)))))))

(define (tree-traverse-levelorder tree
                                  #:converse? [converse? #f])
  (if (empty? tree)
      empty-stream
      (let loop ([queue (list tree)])
        (if (empty? queue)
            empty-stream
            (let ([current (first queue)])
              (if (empty? current)
                  (loop (rest queue))
                  (stream-cons (first current)
                               (loop (.. (rest queue)
                                         (if converse?
                                             (reverse (rest current))
                                             (rest current)))))))))))

(define (tree-traverse tree
                       #:order [order 'pre]
                       #:converse? [converse? #f])
  (cond [(= order 'pre)
         (tree-traverse-preorder tree
                                 #:converse? converse?)]
        [(= order 'post)
         (tree-traverse-postorder tree
                                  #:converse? converse?)]
        [(= order 'in)
         (tree-traverse-inorder tree
                                #:converse? converse?)]
        [(= order 'level)
         (tree-traverse-levelorder tree
                                   #:converse? converse?)]
        [else
         (error "Invalid traversal order!")]))

(module+ test

  (define tests
    (test-suite
     "Lazytree tests"
     (test-case
         "Empty list-formatted tree"
       (define t (list))
       (check-equal? (->list (tree-traverse t #:order 'pre))
                     (list))
       (check-equal? (->list (tree-traverse t #:order 'pre #:converse? #t))
                     (list))
       (check-equal? (->list (tree-traverse t #:order 'post))
                     (list))
       (check-equal? (->list (tree-traverse t #:order 'post #:converse? #t))
                     (list))
       (check-equal? (->list (tree-traverse t #:order 'in))
                     (list))
       (check-equal? (->list (tree-traverse t #:order 'in #:converse? #t))
                     (list))
       (check-equal? (->list (tree-traverse t #:order 'level))
                     (list))
       (check-equal? (->list (tree-traverse t #:order 'level #:converse? #t))
                     (list))
       (check-equal? (->list (tree-traverse (tree-map add1 t)))
                     (list))
       (check-equal? (->list (tree-traverse (tree-filter (curryr < 7) t)))
                     (list))
       (check-equal? (tree-fold + t) ID)
       (check-equal? (->list
                      (tree-traverse
                       (make-tree rest
                                  t
                                  #:empty-pred empty?
                                  #:with-data first)))
                     (->list (tree-traverse t))
                     "idempotence for list-formatted tree")
       (check-equal? (export-tree list
                                  (make-tree rest
                                             t
                                             #:empty-pred empty?
                                             #:with-data first))
                     t
                     "isomorphic representation (sanity)"))

     (test-case
         "Leaf list-formatted tree"
       (define t (list 1))
       (check-equal? (->list (tree-traverse t #:order 'pre))
                     (list 1))
       (check-equal? (->list (tree-traverse t #:order 'pre #:converse? #t))
                     (list 1))
       (check-equal? (->list (tree-traverse t #:order 'post))
                     (list 1))
       (check-equal? (->list (tree-traverse t #:order 'post #:converse? #t))
                     (list 1))
       (check-equal? (->list (tree-traverse t #:order 'in))
                     (list 1))
       (check-equal? (->list (tree-traverse t #:order 'in #:converse? #t))
                     (list 1))
       (check-equal? (->list (tree-traverse t #:order 'level))
                     (list 1))
       (check-equal? (->list (tree-traverse t #:order 'level #:converse? #t))
                     (list 1))
       (check-equal? (->list (tree-traverse (tree-map add1 t)))
                     (list 2))
       (check-equal? (->list (tree-traverse (tree-filter (curryr < 7) t)))
                     (list 1))
       (check-equal? (tree-fold + t) 1)
       (check-equal? (->list
                      (tree-traverse
                       (make-tree rest
                                  t
                                  #:empty-pred empty?
                                  #:with-data first)))
                     (->list (tree-traverse t))
                     "idempotence for list-formatted tree")
       (check-equal? (export-tree list
                                  (make-tree rest
                                             t
                                             #:with-data first))
                     t
                     "isomorphic representation (sanity)"))

     (test-case
         "List-formatted numeric tree"
       (define t (list 1
                       (list 2
                             (list 3)
                             (list 4))
                       (list 5
                             (list 6)
                             (list 7))
                       (list 8
                             (list 9
                                   (list 10)
                                   (list 11))
                             (list 12
                                   (list 13)
                                   (list 14)))))
       (check-equal? (->list (tree-traverse t #:order 'pre))
                     (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14))
       (check-equal? (->list (tree-traverse t #:order 'pre #:converse? #t))
                     (list 1 8 12 14 13 9 11 10 5 7 6 2 4 3))
       (check-equal? (->list (tree-traverse t #:order 'post))
                     (list 3 4 2 6 7 5 10 11 9 13 14 12 8 1))
       (check-equal? (->list (tree-traverse t #:order 'post #:converse? #t))
                     (list 14 13 12 11 10 9 8 7 6 5 4 3 2 1))
       (check-equal? (->list (tree-traverse t #:order 'in))
                     (list 3 2 4 1 6 5 7 10 9 11 8 13 12 14))
       (check-equal? (->list (tree-traverse t #:order 'in #:converse? #t))
                     (list 14 12 13 8 11 9 10 1 7 5 6 4 2 3))
       (check-equal? (->list (tree-traverse t #:order 'level))
                     (list 1 2 5 8 3 4 6 7 9 12 10 11 13 14))
       (check-equal? (->list (tree-traverse t #:order 'level #:converse? #t))
                     (list 1 8 5 2 12 9 7 6 4 3 14 13 11 10))
       (check-equal? (->list (tree-traverse (make-tree (unthunk (->generator (take 3 (repeat (list 1 2))) '())) 5)))
                     (list 5 1 1 1 2 2 2))
       (check-equal? (->list (tree-traverse (tree-map add1 t)))
                     (list 2 3 4 5 6 7 8 9 10 11 12 13 14 15))
       (check-equal? (->list (tree-traverse (tree-filter odd? t)))
                     (list 1 5 7))
       (check-equal? (tree-fold + t) 105)
       (check-equal? (tree-fold + t 1) 106)
       (check-equal? (->list (tree-fold + t #:order 'pre #:with-steps? #t))
                     (list 0 1 3 6 10 15 21 28 36 45 55 66 78 91 105))
       (check-equal? (->list (tree-fold + t #:order 'pre #:converse? #t #:with-steps? #t))
                     (list 0 1 9 21 35 48 57 68 78 83 90 96 98 102 105))
       (check-equal? (->list (tree-fold + t #:order 'post #:with-steps? #t))
                     (list 0 3 7 9 15 22 27 37 48 57 70 84 96 104 105))
       (check-equal? (->list (tree-fold + t #:order 'in #:with-steps? #t))
                     (list 0 3 5 9 10 16 21 28 38 47 58 66 79 91 105))
       (check-equal? (->list (tree-fold + t #:order 'level #:with-steps? #t))
                     (list 0 1 3 8 16 19 23 29 36 45 57 67 78 91 105))
       (check-equal? (->list
                      (tree-traverse
                       (make-tree rest
                                  t
                                  #:empty-pred empty?
                                  #:with-data first)))
                     (->list (tree-traverse t))
                     "idempotence for list-formatted tree")
       (check-equal? (export-tree list
                                  (make-tree rest
                                             t
                                             #:with-data first))
                     t
                     "isomorphic representation (sanity)"))

     (test-case
         "Empty non-list-formatted tree instance"
       (struct node (data left right)
         #:transparent)
       (struct empty-tree ()
         #:transparent)
       (define (node-children t)
         (list (node-left t)
               (node-right t)))
       (define tree (empty-tree))
       (let ([t (make-tree node-children
                           tree
                           #:empty-pred empty-tree?)])
         (check-equal? (tree-fold + (tree-map node-data t)) ID))
       (let ([t (make-tree node-children
                           tree
                           #:empty-pred empty-tree?
                           #:with-data node-data)])
         (check-equal? (tree-fold + t) ID)
         (check-equal? (->list (tree-traverse t #:order 'pre)) (list))
         (check-equal? (->list (tree-traverse t #:order 'pre #:converse? #t)) (list))
         (check-equal? (->list (tree-traverse t #:order 'post)) (list))
         (check-equal? (->list (tree-traverse t #:order 'post #:converse? #t)) (list))
         (check-equal? (->list (tree-traverse t #:order 'in)) (list))
         (check-equal? (->list (tree-traverse t #:order 'in #:converse? #t)) (list))
         (check-equal? (->list (tree-traverse t #:order 'level)) (list))
         (check-equal? (->list (tree-traverse t #:order 'level #:converse? #t)) (list))
         (check-equal? (->list (tree-traverse (tree-map add1 t)))
                       (list))
         (check-equal? (->list (tree-traverse (tree-filter (curryr < 7) t)))
                       (list)))
       (check-equal? (export-tree node
                                  (make-tree node-children
                                             tree
                                             #:empty-pred empty-tree?
                                             #:with-data node-data)
                                  #:empty-cons empty-tree)
                     tree
                     "isomorphic representation (sanity)"))

     (test-case
         "Leaf non-list-formatted tree instance"
       (struct node (data left right)
         #:transparent)
       (struct empty-tree ()
         #:transparent)
       (define (node-children t)
         (list (node-left t)
               (node-right t)))
       (define tree (node 1
                          (empty-tree)
                          (empty-tree)))
       (let ([t (make-tree node-children
                           tree
                           #:empty-pred empty-tree?)])
         (check-equal? (tree-fold + (tree-map node-data t)) 1))
       (let ([t (make-tree node-children
                           tree
                           #:empty-pred empty-tree?
                           #:with-data node-data)])
         (check-equal? (tree-fold + t) 1)
         (check-equal? (->list (tree-traverse t #:order 'pre)) (list 1))
         (check-equal? (->list (tree-traverse t #:order 'pre #:converse? #t)) (list 1))
         (check-equal? (->list (tree-traverse t #:order 'post)) (list 1))
         (check-equal? (->list (tree-traverse t #:order 'post #:converse? #t)) (list 1))
         (check-equal? (->list (tree-traverse t #:order 'in)) (list 1))
         (check-equal? (->list (tree-traverse t #:order 'in #:converse? #t)) (list 1))
         (check-equal? (->list (tree-traverse t #:order 'level)) (list 1))
         (check-equal? (->list (tree-traverse t #:order 'level #:converse? #t)) (list 1))
         (check-equal? (->list (tree-traverse (tree-map add1 t)))
                       (list 2))
         (check-equal? (->list (tree-traverse (tree-filter (curryr < 7) t)))
                       (list 1)))
       (check-equal? (export-tree node
                                  (make-tree node-children
                                             tree
                                             #:empty-pred empty-tree?
                                             #:with-data node-data)
                                  #:empty-cons empty-tree)
                     tree
                     "isomorphic representation (sanity)"))

     (test-case
         "Tree with sentinel empty nodes"
       (struct node (data left right)
         #:transparent)
       (struct empty-tree ()
         #:transparent)
       (define (node-children t)
         (list (node-left t)
               (node-right t)))
       (struct node-too (data children)
         #:transparent)
       (define tree (node 1
                          (node 2
                                (node 3
                                      (node 4
                                            (empty-tree)
                                            (empty-tree))
                                      (empty-tree))
                                (node 5
                                      (empty-tree)
                                      (node 6
                                            (empty-tree)
                                            (empty-tree))))
                          (node 7
                                (node 8
                                      (empty-tree)
                                      (empty-tree))
                                (empty-tree))))
       (let ([t (make-tree node-children
                           tree
                           #:empty-pred empty-tree?)])
         (check-equal? (tree-fold + (tree-map node-data t)) 36))
       (let ([t (make-tree node-children
                           tree
                           #:empty-pred empty-tree?
                           #:with-data node-data)])
         (check-equal? (tree-fold + t) 36)
         (check-equal? (->list (tree-traverse t #:order 'pre)) (list 1 2 3 4 5 6 7 8))
         (check-equal? (->list (tree-traverse t #:order 'pre #:converse? #t)) (list 1 7 8 2 5 6 3 4))
         (check-equal? (->list (tree-traverse t #:order 'post)) (list 4 3 6 5 2 8 7 1))
         (check-equal? (->list (tree-traverse t #:order 'post #:converse? #t)) (list 8 7 6 5 4 3 2 1))
         (check-equal? (->list (tree-traverse t #:order 'in)) (list 4 3 2 5 6 1 8 7))
         (check-equal? (->list (tree-traverse t #:order 'in #:converse? #t)) (list 7 8 1 6 5 2 3 4))
         (check-equal? (->list (tree-traverse t #:order 'level)) (list 1 2 7 3 5 8 4 6))
         (check-equal? (->list (tree-traverse t #:order 'level #:converse? #t)) (list 1 7 2 8 5 3 6 4))
         (check-equal? (->list (tree-traverse (tree-map add1 t)))
                       (list 2 3 4 5 6 7 8 9))
         (check-equal? (->list (tree-traverse (tree-filter odd? t)))
                       (list 1 7))
         (check-equal? (export-tree (λ tree
                                      (node-too (first tree)
                                                (rest tree)))
                                    t)
                       (node-too 1
                                 (list
                                  (node-too 2
                                            (list
                                             (node-too 3
                                                       (list
                                                        (node-too 4
                                                                  (list))))
                                             (node-too 5
                                                       (list
                                                        (node-too 6
                                                                  (list))))))
                                  (node-too 7
                                            (list
                                             (node-too 8
                                                       (list))))))
                       "empty subtrees are eliminated"))
       (check-equal? (export-tree node
                                  (make-tree node-children
                                             tree
                                             #:empty-pred empty-tree?
                                             #:with-data node-data)
                                  #:empty-cons empty-tree)
                     tree
                     "isomorphic representation (sanity)"))
     (test-case
         "Infinite tree"
       (let ([t (make-tree (λ (v) (list (add1 v)
                                        ((power add1 2) v)
                                        ((power add1 3) v)))
                           1)])
         (check-equal? (->list (take 10 (tree-traverse t #:order 'pre))) (list 1 2 3 4 5 6 7 8 9 10))
         (check-equal? (->list (take 10 (tree-traverse t #:order 'pre #:converse? #t))) (list 1 4 7 10 13 16 19 22 25 28))
         ;; can't do post- or in-order traversals since they start at the ends
         (check-equal? (->list (take 10 (tree-traverse t #:order 'level))) (list 1 2 3 4 3 4 5 4 5 6))
         (check-equal? (->list (take 10 (tree-traverse t #:order 'level #:converse? #t))) (list 1 4 3 2 7 6 5 6 5 4))
         (check-equal? (->list (take 10 (tree-traverse (tree-map add1 t))))
                       (list 2 3 4 5 6 7 8 9 10 11))
         (check-equal? (->list (take 10 (tree-traverse (tree-filter odd? t))))
                       (list 1 3 5 7 9 11 13 15 17 19))

         (check-equal? (->list (take 10 (tree-fold + t #:order 'pre #:with-steps? #t)))
                       (list 0 1 3 6 10 15 21 28 36 45))
         (check-equal? (->list (take 10 (tree-fold + t #:order 'pre #:converse? #t #:with-steps? #t)))
                       (list 0 1 5 12 22 35 51 70 92 117))
         (check-equal? (->list (take 10 (tree-fold + t #:order 'level #:with-steps? #t)))
                       (list 0 1 3 6 10 13 17 22 26 31))
         (check-equal? (->list (take 10 (tree-fold + t #:order 'level #:converse? #t #:with-steps? #t)))
                       (list 0 1 5 8 10 17 23 28 34 39)))))))

(module+ test
  (run-tests tests))
