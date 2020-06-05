#lang racket/base

(require racket/stream
         racket/set
         (except-in data/collection
                    foldl
                    foldl/steps
                    append
                    index-of)
         relation)

(provide remove-when)

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
