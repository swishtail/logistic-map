;;; Find fixed points of the logistic map

(define (logistic-fixed-points r-lower r-upper r-step iterations tail tolerance)
  (define (reverse-logistic-list x r n)
    (let logistic-iter ((counter n)
                        (result (list x)))
      (if (zero? counter)
          result
          (logistic-iter (- counter 1)
                         (cons (* r (car result) (- 1 (car result))) result)))))

  (define (quantise x tolerance)
    (* tolerance (round (/ x tolerance))))

  (define (fixed-points r iterations tail tolerance)
    (let fixed-points-iter ((sample-set '())
                            (tail-samples
                             (list-head
                              (reverse-logistic-list 0.5 r iterations) tail)))
      (if (null? tail-samples)
          sample-set
          (fixed-points-iter (adjoin-set (quantise (car tail-samples) tolerance)
                                         sample-set)
                             (cdr tail-samples)))))

  (define (make-r-entry r sample-set)
    (cons r (list (tree->list sample-set))))

  (define (r-list->points r-list)
    (apply append-many
           (map (lambda (r-entry)
                  (map (lambda (point)
                         (list (car r-entry) point))
                       (cadr r-entry)))
                r-list)))

  (let ((r-range (range r-lower r-upper r-step)))
    (r-list->points
     (map (lambda (r)
            (make-r-entry r
                          (fixed-points r iterations tail tolerance)))
          r-range))))

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (tree->list tree)
  (if (null? tree)
      '()
      (append (tree->list (left-branch tree))
              (cons (entry tree)
                    (tree->list (right-branch tree))))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))

(define (range lower upper step)
  (if (> lower upper)
      '()
      (cons lower
            (range (+ lower step) upper step))))

(define (list-head x k)
  (if (zero? k)
      '()
      (cons (car x)
            (list-head (cdr x) (- k 1)))))

(define (fold-right op initial x)
  (if (null? x)
      initial
      (op (car x)
          (fold-right op initial (cdr x)))))

(define (append-many . lists)
  (fold-right append '() lists))

(define the-points
  (logistic-fixed-points 3.56 4 0.0001 2000 1500 0.0001))

(display (length the-points))
(newline)