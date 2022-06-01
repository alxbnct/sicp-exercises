(define (sine angle)
  (define (cube x) (* x x x))
  (define (p x)    (- (* 3 x) (* 4 (cube x))))
  (define (in:sine angle)
    (if (not (> (abs angle) 0.001))
        angle
        (p (in:sine (/ angle 3)))))
  (in:sine (exact->inexact angle)))     ; use `exact->inexact' to
                                        ; convert integer to floating point

#|
(sine 3)
;Value: .1411199045684386
|#

#|
1) 4 times;
2) Time: Theta(n) = log_3(n))
   Space: Theta(n) = log_3(n))

1) 5 times. Because the 4th call still called `in:sine' and the `angle'.
2) let n be the size of the problem (which is the `angle'), and x be
   the times we apply `p', hence
   n * (1/3)^x < 0.001
=> (1/3)^x < 0.001 / n
=> x > log_ (1/3) 1/n
=> x < log_3 1/n = - log_3 n
=> x > log_3 n = log n / log 3

   This means we need to apply `p' at least (log n / log 3) times, so
   the time complexity is (log n).

   `in:sine' is not tail recursive, it's linear recursive, it need to
   push the funcall info into the stack for each funcall, since it
   calls `p' for at least (log n / log 3) times, it's space complexity
   is also (log n).
|#