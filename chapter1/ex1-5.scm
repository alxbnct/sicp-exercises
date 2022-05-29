(define (p) (p))

(define (test x y)
  (if (= 0 x) 0 y))

;; This will cause a infinite loop!
(test 0 (p))