#|
(define (cube-root x)
  (define (improve guess)
    (/ (+ (* 2 guess) (/ x (square guess))) 3))
  (define (good-enough? guess previous-guess)
    (< (abs (- guess previous-guess)) 0.0001))
  (define (cube-root-iter guess previous-guess iter)
    (display "iter: ")
    (display iter)
    (display " ")
    (display "guess: ")
    (display guess)
    (newline)
    (if (good-enough? guess previous-guess)
        guess
        (cube-root-iter (improve guess) guess (1+ iter))))
  (cube-root-iter 1.0 .0 1))

(cube-root 27)
=>
iter: 1 guess: 1.
iter: 2 guess: 9.666666666666666
iter: 3 guess: 6.540758356453956
iter: 4 guess: 4.570876778578707
iter: 5 guess: 3.4780192333867963
iter: 6 guess: 3.0626891086275365
iter: 7 guess: 3.001274406506175
iter: 8 guess: 3.0000005410641766
iter: 9 guess: 3.0000000000000977
;Value: 3.0000000000000977

(cube-root 485285092345820943285034958)
=>
...
iter: 104 guess: 785836717.234608
iter: 105 guess: 785836717.1271406
iter: 106 guess: 785836717.1271406
;Value: 785836717.1271406

(cube-root 0.0002334345345345)
=>
iter: 1 guess: 1.
iter: 2 guess: .6667444781781781
iter: 3 guess: .4446713538247455
iter: 4 guess: .2968410880719121
iter: 5 guess: .1987771301197436
iter: 6 guess: .1344873828404053
iter: 7 guess: .09396035786407697
iter: 8 guess: .07145386274396665
iter: 9 guess: .06287617034600057
iter: 10 guess: 6.1599558404404585e-2
iter: 11 guess: .06157273589214351
;Value: .06157273589214351
|#


