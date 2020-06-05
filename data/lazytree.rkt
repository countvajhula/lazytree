#lang racket/base

(require racket/contract/base
         racket/stream
         racket/generic
         racket/undefined
         (except-in data/collection
                    foldl
                    foldl/steps
                    append
                    index-of)
         (only-in data/collection
                  (append d:append))
         relation)

(require "private/util.rkt")

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

(module+ test
  (require rackunit
           racket/stream
           (only-in racket/function
                    thunk)
           (except-in data/collection
                      foldl
                      foldl/steps
                      append
                      index-of)
           relation))

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

(module+ test

  (let ([t (list 1
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
                             (list 14))))]
        [empty-tree (list)]
        [leaf-tree (list 1)])
    (check-equal? (->list (tree-traverse t #:order 'pre))
                  (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14))
    (check-equal? (->list (tree-traverse empty-tree #:order 'pre))
                  (list))
    (check-equal? (->list (tree-traverse leaf-tree #:order 'pre))
                  (list 1))
    (check-equal? (->list (tree-traverse t #:order 'pre #:converse? #t))
                  (list 1 8 12 14 13 9 11 10 5 7 6 2 4 3))
    (check-equal? (->list (tree-traverse empty-tree #:order 'pre #:converse? #t))
                  (list))
    (check-equal? (->list (tree-traverse leaf-tree #:order 'pre #:converse? #t))
                  (list 1))
    (check-equal? (->list (tree-traverse t #:order 'post))
                  (list 3 4 2 6 7 5 10 11 9 13 14 12 8 1))
    (check-equal? (->list (tree-traverse empty-tree #:order 'post))
                  (list))
    (check-equal? (->list (tree-traverse leaf-tree #:order 'post))
                  (list 1))
    (check-equal? (->list (tree-traverse t #:order 'post #:converse? #t))
                  (list 14 13 12 11 10 9 8 7 6 5 4 3 2 1))
    (check-equal? (->list (tree-traverse empty-tree #:order 'post #:converse? #t))
                  (list))
    (check-equal? (->list (tree-traverse leaf-tree #:order 'post #:converse? #t))
                  (list 1))
    (check-equal? (->list (tree-traverse t #:order 'in))
                  (list 3 2 4 1 6 5 7 10 9 11 8 13 12 14))
    (check-equal? (->list (tree-traverse empty-tree #:order 'in))
                  (list))
    (check-equal? (->list (tree-traverse leaf-tree #:order 'in))
                  (list 1))
    (check-equal? (->list (tree-traverse t #:order 'in #:converse? #t))
                  (list 14 12 13 8 11 9 10 1 7 5 6 4 2 3))
    (check-equal? (->list (tree-traverse empty-tree #:order 'in #:converse? #t))
                  (list))
    (check-equal? (->list (tree-traverse leaf-tree #:order 'in #:converse? #t))
                  (list 1))
    (check-equal? (->list (tree-traverse t #:order 'level))
                  (list 1 2 5 8 3 4 6 7 9 12 10 11 13 14))
    (check-equal? (->list (tree-traverse empty-tree #:order 'level))
                  (list))
    (check-equal? (->list (tree-traverse leaf-tree #:order 'level))
                  (list 1))
    (check-equal? (->list (tree-traverse t #:order 'level #:converse? #t))
                  (list 1 8 5 2 12 9 7 6 4 3 14 13 11 10))
    (check-equal? (->list (tree-traverse empty-tree #:order 'level #:converse? #t))
                  (list))
    (check-equal? (->list (tree-traverse leaf-tree #:order 'level #:converse? #t))
                  (list 1))
    (check-equal? (->list (tree-traverse (make-tree (unthunk (->generator (take 3 (repeat (list 1 2))) '())) 5)))
                  (list 5 1 1 1 2 2 2))
    (check-equal? (->list (tree-traverse (tree-map add1 t)))
                  (list 2 3 4 5 6 7 8 9 10 11 12 13 14 15))
    (check-equal? (->list (tree-traverse (tree-map add1 empty-tree)))
                  (list))
    (check-equal? (->list (tree-traverse (tree-map add1 leaf-tree)))
                  (list 2))
    (check-equal? (->list (tree-traverse (tree-filter (curryr < 7) t)))
                  (list 1 2 3 4 5 6))
    (check-equal? (->list (tree-traverse (tree-filter (curryr < 7) empty-tree)))
                  (list))
    (check-equal? (->list (tree-traverse (tree-filter (curryr < 7) leaf-tree)))
                  (list 1))
    (check-equal? (tree-fold + t) 105)
    (check-equal? (tree-fold + empty-tree) ID)
    (check-equal? (tree-fold + leaf-tree) 1)
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
                  (list 0 1 3 8 16 19 23 29 36 45 57 67 78 91 105))))
