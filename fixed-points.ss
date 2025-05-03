;;; Find fixed points of the logistic map

(define (logistic-fixed-points r-lower r-upper r-step iterations tail tolerance)
  (fold-right append
              '()
              (map (lambda (r) (fixed-points r iterations tail tolerance))
                   (range r-lower r-upper r-step))))

(define (fixed-points r iterations tail tolerance)
  (map (lambda (point) (list (exact->inexact r) point))
       (tree->list
        (fold-right adjoin-set
                    '()
                    (map (lambda (sample) (quantise sample tolerance))
                         (list-head (reverse-orbit r iterations) tail))))))

(define (reverse-orbit r n)
  (let orbit-iter ((counter n) (result (list 0.5)))
    (if (zero? counter)
        result
        (orbit-iter (- counter 1)
                    (cons (* r (car result) (- 1 (car result))) result)))))

(define (quantise x tolerance)
  (* tolerance (round (/ x tolerance))))

(define (range lower upper step)
  (if (> lower upper)
      '()
      (cons lower (range (+ lower step) upper step))))

(define (fold-right op initial x)
  (if (null? x)
      initial
      (op (car x)
          (fold-right op initial (cdr x)))))

(define (list-head x k)
  (if (zero? k)
      '()
      (cons (car x)
            (list-head (cdr x) (- k 1)))))

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

(display
 (length
  (logistic-fixed-points
   356/100 4 1/10000 2000 1500 1/10000)))
