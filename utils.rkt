#lang racket/base

(require racket/contract/base
         racket/stream
         racket/generic
         racket/undefined
         racket/set
         (except-in data/collection
                    foldl
                    foldl/steps
                    append
                    index-of)
         (only-in data/collection
                  (append d:append))
         relation)

(provide (contract-out
          [make-tree (-> (-> any/c sequence?)
                         any/c
                         sequence?)]
          [tree-traverse (->* (sequence?)
                              (#:order (one-of/c 'pre
                                                 'post
                                                 'in
                                                 'level)
                               #:converse? boolean?)
                              sequence?)]
          [tree-map (-> (-> any/c any/c)
                        sequence?
                        sequence?)]
          [tree-filter (-> (-> any/c boolean?)
                           sequence?
                           sequence?)]
          [tree-fold (->* ((-> any/c any/c any/c) sequence?)
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

(define (~remove-when #:how-many [how-many #f]
                      pred
                      seq)
  (if ((|| set? gset?) seq)
      (raise-argument-error 'remove-when
                            "sequence? that is not a pure set"
                            seq)
      (if (empty? seq)
          seq
          (if how-many
              (if (> how-many 0)
                  (let ([v (first seq)]
                        [vs (rest seq)])
                    (if (pred v)
                        (~remove-when #:how-many (sub1 how-many)
                                      pred
                                      (rest seq))
                        (stream-cons v
                                     (~remove-when #:how-many how-many
                                                   pred
                                                   (rest seq)))))
                  seq)
              (filter (!! pred) seq)))))

(define (remove-when #:how-many [how-many #f]
                     pred
                     seq)
  (let ([result (~remove-when #:how-many how-many
                              pred
                              seq)])
    (if (string? seq)
        (->string result)
        result)))

(define (make-tree f node)
  (stream-cons node
               (map (curry make-tree f)
                    (f node))))

(define (tree-map f tree)
  (if (empty? tree)
      (stream)
      (stream-cons (f (first tree))
                   (map (curry tree-map f)
                        (rest tree)))))

(define (tree-filter f tree)
  (if (empty? tree)
      (stream)
      (if (f (first tree))
          (stream-cons (first tree)
                       (remove-when empty?
                                    (map (curry tree-filter f)
                                         (rest tree))))
          (stream))))

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
      (stream)
      (stream-cons (first tree)
                   (apply d:append
                          (map (curry tree-traverse-preorder
                                      #:converse? converse?)
                               (if converse?
                                   (reverse (rest tree))
                                   (rest tree)))))))

(define (tree-traverse-postorder tree
                                 #:converse? [converse? #f])
  (if (empty? tree)
      (stream)
      (d:append (apply d:append
                       (map (curry tree-traverse-postorder
                                   #:converse? converse?)
                            (if converse?
                                (reverse (rest tree))
                                (rest tree))))
                (stream (first tree)))))

(define (tree-traverse-inorder tree
                               #:converse? [converse? #f])
  (if (empty? tree)
      (stream)
      (if (empty? (rest tree))
          (stream (first tree))
          (let ([children (if converse?
                              (reverse (rest tree))
                              (rest tree))])
            (apply d:append
                   (tree-traverse-inorder (first children)
                                          #:converse? converse?)
                   (stream (first tree))
                   (map (curry tree-traverse-inorder
                               #:converse? converse?)
                        (rest children)))))))

(define (tree-traverse-levelorder tree
                                  #:converse? [converse? #f])
  (if (empty? tree)
      (stream)
      (let loop ([queue (list tree)])
        (if (empty? queue)
            (stream)
            (let ([current (first queue)])
              (stream-cons (first current)
                           (loop (d:append (rest queue)
                                           (if converse?
                                               (reverse (rest current))
                                               (rest current))))))))))

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
