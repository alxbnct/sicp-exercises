#|

(define (my-sqrt-newton x)
  (define (good-enough? guess x)
    (< (abs (- (square guess) x)) 0.0001))
  (define (improve guess x)
    (- guess (/ (- (square guess) x) (* 2 guess))))
  (define (sqrt-iter guess x iter)
    (display "iter: ")
    (display iter)
    (display " ")
    (display "guess: ")
    (display guess)
    (newline)
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x) x (1+ iter))))
  (sqrt-iter 1.0 x 1))

(define (my-sqrt x)
  (define (average x y)
    (/ (+ x y) 2))
  (define (good-enough? guess x)
    (< (abs (- (square guess) x)) 0.0001))
  (define (improve guess x)
    (average guess (/ x guess)))
  (define (sqrt-iter guess x iter)
    (display "iter: ")
    (display iter)
    (newline)
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x) x (1+ iter))))
  (sqrt-iter 1.0 x 1))

(my-sqrt-newton 0.345235)
(my-sqrt 0.345235)

iter: 1
iter: 2
iter: 3
iter: 4
;Value: .5875913993227003

iter: 1
iter: 2
iter: 3
iter: 4
;Value: .5875913993227003

Interestingly, it doesn't show any improvements of using newton's
method (this is not related to the original question though), why?

|#


(define (my-sqrt-newton-cauchy x)
  (define (good-enough? guess previous-guess)
    (< (abs (- guess previous-guess)) 0.0001))
  (define (improve guess x)
    (- guess (/ (- (square guess) x) (* 2 guess))))
  (define (sqrt-iter guess previous-guess x iter)
    (display "iter: ")
    (display iter)
    (display " ")
    (display "guess: ")
    (display guess)
    (newline)
    (if (good-enough? guess previous-guess)
        guess
        (sqrt-iter (improve guess x) guess x (1+ iter))))
  (sqrt-iter 1.0 -1 x 1))


(define (my-sqrt-cauchy x)
  (define (good-enough? guess previous-guess)
    (< (abs (- guess previous-guess)) 0.0001))
  (define (average x y)
    (/ (+ x y) 2))
  (define (improve guess x)
    (average guess (/ x guess)))
  (define (sqrt-iter guess previous-guess x iter)
    (display "iter: ")
    (display iter)
    (newline)
    (if (good-enough? guess previous-guess)
        guess
        (sqrt-iter (improve guess x) guess x (1+ iter))))
  (sqrt-iter 1.0 -1 x 1))

#|
(my-sqrt-cauchy 9)

(my-sqrt-cauchy 0.324235)

(my-sqrt-newton-cauchy 0.324235)
iter: 1
iter: 2
iter: 3
iter: 4
iter: 5
;Value: .5694163690452583


(my-sqrt-newton 0.324235)
iter: 1 guess: 1.
iter: 2 guess: .6621175
iter: 3 guess: .5759057748860663
iter: 4 guess: .5694529297582406
;Value: .5694529297582406

It seemd that the cauchy criterion one doesn't perform well, at least
in the case.
|#



#| For very large numbers
(my-sqrt-cauchy 8945729837598)
...
iter: 23
iter: 24
iter: 25
iter: 26
iter: 27
iter: 28
;Value: 2990941.2962473873

(my-sqrt-newton-cauchy 8945729837598)
...
iter: 23
iter: 24
iter: 25
iter: 26
iter: 27
iter: 28
;Value: 2990941.296247387

(my-sqrt-newton 8945729837598) ;; this doesn't converge
...
iter: 104160 guess: 2990941.296247387
iter: 104161 guess: 2990941.2962473873
iter: 104162 guess: 2990941.296247387
iter: 104163 guess: 2990941.2962473873
iter: 104164 guess: 2990941.296247387
iter: 104165 guess: 2990941.2962473873
;Quit!

Here the code shows the benefit of using cauchy criterion in
good-enough? The original
(good-enough? 2990941.296247387 894572983759)
=> (< (abs (- (square 2990941.296247387) 8945729837598)) 0.0001)
=> (< (abs (- 8945729837597.998 8945729837598)) 0.001)
=> (< 0.002 0.001)
=> #f
(good-enough? 2990941.2962473873 894572983759)
=> (< (abs (- (square 2990941.2962473873) 8945729837598)) 0.0001)
=> (< (abs (- 8945729837598.002 8945729837598)) 0.001)
=> (< 0.002 0.001)
=> #f
which can't converge because of the limited precision of computers.
|#


#| For extreamly small numbers
(my-sqrt-newton-cauchy 0.000000345346341)
(my-sqrt-newton 0.000000345346341)
(my-sqrt-cauchy 0.000000345346341)

iter: 1
iter: 2
iter: 3
iter: 4
iter: 5
iter: 6
iter: 7
iter: 8
iter: 9
iter: 10
iter: 11
iter: 12
iter: 13
iter: 14
;Value: 5.877391481519098e-4

iter: 1 guess: 1.
iter: 2 guess: .5000001726731704
iter: 3 guess: .25000043168280695
iter: 4 guess: .12500090653289284
iter: 5 guess: .06250183464179232
iter: 6 guess: .03125368001052742
iter: 7 guess: 1.5632364896106705e-2
iter: 8 guess: 7.827228324454497e-3
;Value: 7.827228324454497e-3

iter: 1
iter: 2
iter: 3
iter: 4
iter: 5
iter: 6
iter: 7
iter: 8
iter: 9
iter: 10
iter: 11
iter: 12
iter: 13
iter: 14
;Value: 5.877391481519098e-4

As you can see, the non cauchy one is actually producing an incorrect
answer! 
|#

#| For extreamly large numbers
(my-sqrt-newton-cauchy 3249823900985042382309423423537527520572835798347528943589473589274398572934579234)
...
iter: 137 guess: 6.263859200323567e40
iter: 138 guess: 5.726036361706783e40
iter: 139 guess: 5.700778627781775e40
iter: 140 guess: 5.700722674624787e40
iter: 141 guess: 5.700722674350194e40
iter: 142 guess: 5.700722674350194e40
;Value: 5.700722674350194e40

Ok, this seems good.

(my-sqrt-cauchy 3249823900985042382309423423537527520572835798347528943589473589274398572934579234)
...
iter: 137
iter: 138
iter: 139
iter: 140
iter: 141
iter: 142
;Value: 5.7007226743501945e40

This one seems more accurate, hmm...
|#

#|
Why both of the improve method work? It's because of the fixed point theorem.
Theoream. Let phi(x) \in C[a,b] satisfying the following conditions:
1) a <= phi(x) <= b FOR-ANY x \in [a,b]
2) THERE-EXSIST a constant 0 < L < 1, SUCH-THAT FOR-ANY x,y \in [a,b],
   || phi(x) - phi{y) || <= L || x - y||
then phi(x) has only one fixed point x* \in [a,b].

I am not going to go through this theorem, but you can proof that both
improve method are satisfying the fixed point theorem (sort of, but
I'm not sure for the x->0+ part, i'll check this in the future), hence
they can converge.

For the first `improve': phi(x) = (x + a / x) / 2, (suppose we're finding sqrt(a))
=> phi'(x) = 1/2 - a/(2 * x^2) < L < 1 as x -> infty, but as x -> 0+,
   phi(x) -> -infty, there must be something wrong here.

For the second `improve': phi(x) = ( 2 x + a / (x^2)) / 3,
=> phi'(x) = 2/3  - 2 a / (3 x^3) < L < 1 as x -> infty, but as x ->0+,
   phi(x) -> -infty, there must be something wrong here as well.
|#


#| sicp on page 35
(define (square1 x) (* x x))
(define (square2 x) (exp (double (log x))))
(define (double x) (+ x x))
(square2 9) => ;Value: 81.00000000000003
(square1 9) => ;Value: 81

The results of these two procedure are not indistinguishable, one
produces a floating point number, the other produces an integer. I
think a better example for demonstrating such idea (procedural
abstraction) is
(define (square3 x)
  (let loop ((i x)
             (sum 0))
    (if (< i 1)
        sum
        (loop (-1+ i) (+ sum x)))))

(square3 9) => ;Value: 81

Of course, this is a dumb way to squre a number, but it's better for
this specific example i suppose.
|#