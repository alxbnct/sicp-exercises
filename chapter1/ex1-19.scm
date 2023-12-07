#|
The problem here is to find a way of computing p' and q' in terms of
such that applying T_(pq) twice is the same as applying T_(p'q') once.

let lna and lnb be the number of a and b on the left side of the pair
(*,*), rna and rnb be the number of a and b on the right side.

                    (a, b)
 p = 0, q = 1    => (b+a, a)                                            lna = 1, lnb = 1, rna = 1, rnb = 0
 p = 1, q = 1    => (a + b+a, b+a)                                      lna = 2, lnb = 1, rna = 1, rnb = 1
 p = 1, q = 2    => (a+b+a + b+a, a + b+a)                              lna = 3, lnb = 2, rna = 2, rnb = 1
 p = 2, q = 3    => (a+b+a+b+a + a+b+a, a+b+a + b+a)                    lna = 5, lnb = 3, rna = 3, rnb = 2
 p = 3, q = 5    => (a+b+a+b+a+a+b+a +  a+b+a+b+a, a+b+a+b+a + a+b+a)   lna = 8, lnb = 5, rna = 5, rnb = 3

We can find the pattern here: rna = lnb = q, rnb = p, lna = p+q
So the following transformation T_(pq) makes sense
 T     : (a, b)  => (a + b, a)
 T_(pq): (a, b)  => (lnb * b + lna * a, rna * a + rnb * b)
                    = (q * b + (q+p) * a, q * a + p * b)
                    = (bq + aq + ap, bp + aq)

And when applying T_(p'q') once is equivalent as applying T_(pq)^2, we
can calculate p' and q' in terms of p and q as follows:
 T_(p'q') := T_(pq)^2
 p' = p + q, q' = p + 2*q
|#

#| These are wrong @
(define (fib-log n)
  (define (fib-iter a b p q count)
    (cond ((= count 0) b)
          ((even? count)
           (fib-iter a b (+ p q) (+ p (* 2 q)) (/ count 2)))
          (else (fib-iter (+ (* b q) (* a q) (* a p))
                          (+ (* b p) (* a q))
                          p q (- count 1)))))
  (fib-iter 1 0 0 1 n))

(define (fib-log n)
  (let lp ((a 1) (b 0) (p 0) (q 1) (count n))
    (display `((a b): (,a ,b) (p q): (,p ,q) count: ,count))
    (newline)
    (cond ((= count 0) b)
          ((even? count)
           (lp a b (+ p q) (+ p (* 2 q)) (/ count 2)))
          (else (lp (+ (* b q) (* a q) (* a p))
                    (+ (* b p) (* a q))
                    p q (- count 1))))))
|#

(define (fib-log n)
  (let lp ((a 1) (b 0) (p 0) (q 1) (n n))
    (display `(a: ,a b: ,b p: ,p q: ,q n: ,n))
    (newline)
    (cond ((= n 0) b)
          ((even? n)
           (lp a b
               (+ (* p p) (* q q))
               (+ (* 2 p q) (* q q))
               (/ n 2)))
          (else (lp (+ (* b q) (* a q) (* a p))
                    (+ (* b p) (* a q))
                    p q (- n 1))))))
