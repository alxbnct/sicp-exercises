(define (A x y)
  (cond ((= x 0) (* 2 y))
        ((= y 0) 0)
        ((= y 1) 2)
        (else (A (-1+ x) (A x (-1+ y))))))

#|
(A 1 10)
=> ;Value: 1024

(A 2 4)
=> ;Value: 65536

(A 3 3)
=> ;Value: 65536
|#

(define (f n)
  (A 0 n))

#|
f(n) -> 2*n
|#

(define (g n)
  (A 1 n))

#|
g(n) -> 2^n
|#

(define (h n)
  (A 2 n))

#|          n - 1 times
          /------^------\
h(n) -> 2^(2^(2^...(2^2)))
|#