#|
Last modified: Sat 21 May 2022 11:22:20 PM HKT
Author: Yosef Weissmann

if I use new-if in my-sqrt, and execute (my-sqrt 9) the REP loop will report:

;Aborting!: maximum recursion depth exceeded

Why?

I add a side effect to both functions and here is the output:
;Value: my-sqrt

1.
5.
3.4
3.023529411764706
3.00009155413138
3.000000001396984
;Value: 3.000000001396984

;Value: my-sqrt-new-if

1.
5.
3.4
3.023529411764706
3.00009155413138
3.000000001396984
3.
3.
3.
3.
3.
3.
3.
3.
3.
3.
3.
3.
3.
3.
3.
3.
3.
... (skip many 3.s)
;Aborting!: maximum recursion depth exceeded

This means that the predicate in new-if, (good-enough? guess x), is
not evaluated to true even when guess = 3. (which is good enough). So
from this example we can see that new-if i...

;Value: my-sqrt-new-if
#f
#f
#f
#f
#f
#t
#t
#t
#t
#t
#t
#t
#t
#t
#t
#t
... (skip many #ts)
;Aborting!: maximum recursion depth exceeded

No, as i modified the side effect, it shows that (good-enough? guess
x) is evaluated to #t after a few steps of iterations. But why new-if
can't evaluate the other branch?

I guess it's because of the early evaluation of (good-enough? guess x) 

|#

(define (new-if pred then else)
  (cond (pred then)
        (else else)))

(define-syntax new-if-syntax
  (syntax-rules ()
      ((_ pred then else)
       (cond (pred then)
             (else else)))))


(let ((a 9))
  (new-if (> a 1) a 4))

(let ((a 9))
  (new-if (< a 1) (1+ a) (+ a 4)))

(define (my-sqrt-new-if x)
  (define (average x y)
    (/ (+ x y) 2))
  (define (good-enough? guess x)
    (< (abs (- (square guess) x)) 0.0001))
  (define (improve guess x)
    (average guess (/ x guess)))
  (define (sqrt-iter guess x)
;    (display guess)
;    (newline)
    (display (good-enough? guess x))
    (newline)
    (new-if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x) x)))
  (sqrt-iter 1.0 x))

(define (my-sqrt-new-if-syntax x)
  (define (average x y)
    (/ (+ x y) 2))
  (define (good-enough? guess x)
    (< (abs (- (square guess) x)) 0.0001))
  (define (improve guess x)
    (average guess (/ x guess)))
  (define (sqrt-iter guess x)
    (display guess)
    (newline)
    (display (good-enough? guess x))
    (newline)
    (new-if-syntax (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x) x)))
  (sqrt-iter 1.0 x))

(define (my-sqrt x)
  (define (average x y)
    (/ (+ x y) 2))
  (define (good-enough? guess x)
    (< (abs (- (square guess) x)) 0.0001))
  (define (improve guess x)
    (average guess (/ x guess)))
  (define (sqrt-iter guess x)
    (display guess)
    (newline)
    (display (good-enough? guess x))
    (newline)
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x) x)))
  (sqrt-iter 1.0 x))

(my-sqrt 9)
(my-sqrt-new-if 9)
(my-sqrt-new-if-syntax 9)

#|
The original my-sqrt with side effects has the following output:
;Value: my-sqrt

1.
#f
5.
#f
3.4
#f
3.023529411764706
#f
3.00009155413138
#f
3.000000001396984
#t
;Value: 3.000000001396984


But very strange, if I use the new syntax new-if-syntax, the program
produces the following outputs:

;Value: my-sqrt-new-if-syntax

1.
#f
5.
#f
3.4
#f
3.023529411764706
#f
3.00009155413138
#f
3.000000001396984
#t
3.000000001396984
#t
3.00009155413138
#f
3.000000001396984
#t
3.000000001396984
#t
3.023529411764706
#f
3.00009155413138
#f
3.000000001396984
#t
3.000000001396984
#t
3.00009155413138
#f
3.000000001396984
#t
3.000000001396984
#t
3.4
#f
3.023529411764706
#f
3.00009155413138
#f
3.000000001396984
#t
3.000000001396984
#t
3.00009155413138
#f
3.000000001396984
#t
3.000000001396984
#t
3.023529411764706
#f
3.00009155413138
#f
3.000000001396984
#t
3.000000001396984
#t
3.00009155413138
#f
3.000000001396984
#t
3.000000001396984
#t
5.
#f
3.4
#f
3.023529411764706
#f
3.00009155413138
#f
3.000000001396984
#t
3.000000001396984
#t
3.00009155413138
#f
3.000000001396984
#t
3.000000001396984
#t
3.023529411764706
#f
3.00009155413138
#f
3.000000001396984
#t
3.000000001396984
#t
3.00009155413138
#f
3.000000001396984
#t
3.000000001396984
#t
3.4
#f
3.023529411764706
#f
3.00009155413138
#f
3.000000001396984
#t
3.000000001396984
#t
3.00009155413138
#f
3.000000001396984
#t
3.000000001396984
#t
3.023529411764706
#f
3.00009155413138
#f
3.000000001396984
#t
3.000000001396984
#t
3.00009155413138
#f
3.000000001396984
#t
3.000000001396984
#t
;Value: 3.000000001396984

It seems that new-if-syntax sometimes is not working.
|#


#| Trying to figure out

(new-if #t
        (begin (display "tbody")
               (newline)
               3)
        (begin (display "fbody")
               (newline)
               9))
=>
fbody
tbody
;Value: 3

So now I can answer the question in sicp. The program new-if is not
a special form, it's a procesure, and all of its arguments will be
evaluated when evaluating new-if. So the above combination is going to
be substituted as
(new-if #t 3 9) => (cond (#t 3)
                         (else 9)) => 3
with both of the side effects being produced. So, if using new-if in
my-sqrt, (new-if (good-enough? guess x)
                 guess
                 (sqrt-iter (improve guess x) x)) is going to evaluate
all of its operands, causing an infinite loop.
|#


#| The stranger problem

If I define new-if-syntax2 as follows

(define-syntax new-if-syntax2
  (syntax-rules ()
    ((_ pred then alt)
     (cond (pred then)
           (else alt)))))

(define (my-sqrt-new-if-syntax2 x)
  (define (average x y)
    (/ (+ x y) 2))
  (define (good-enough? guess x)
    (< (abs (- (square guess) x)) 0.0001))
  (define (improve guess x)
    (average guess (/ x guess)))
  (define (sqrt-iter guess x)
    (display guess)
    (newline)
    (display (good-enough? guess x))
    (newline)
    (new-if-syntax2 (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x) x)))
  (sqrt-iter 1.0 x))

Then run

(my-sqrt-new-if-syntax2 9)
=>
;Value: my-sqrt-new-if-syntax2

1.
#f
5.
#f
3.4
#f
3.023529411764706
#f
3.00009155413138
#f
3.000000001396984
#t
;Value: 3.000000001396984

This is the correct answer, so now i know what's wrong. in
new-if-syntax, which is defined as
(define-syntax new-if-syntax
  (syntax-rules ()
    ((_ pred then else)
     (cond (pred then)
           (else else)))))

I used `else' as the name of the oprand which is going to be the else
branch of `cond', confounded the else keyword of `cond'
|#

(define-syntax new-if-better
  (syntax-rules ()
    ((_ pred then)
     (cond (pred then)))
    ((_ pred then alt)
     (cond (pred then)
           (else alt)))))

(let ((a 3))
  (new-if-better (> a 3)
                 (display "a > 3")
                 (display "a <= 3")))

;; this is going be yield an error message:
;; ;Ill-formed special form: (new-if-syntax2 (> a 3) (display "a > 3"))
(let ((a 4))
  (new-if-syntax2 (> a 3)
                  (display "a > 3")))

;; but this one is working
(let ((a 4))
  (new-if-better (> a 3)
                  (display "a > 3")))


#|
Why my-sqrt-new-if-syntax is able to converge finally?

Let me expand that special form (btw, is there any procesure in mit
scheme to do special form expansion like macroexpand in CL?)

(new-if-syntax (good-enough? guess x)
               guess
               (sqrt-iter (improve guess x) x))
=> (cond ((good-enough? guess x) guess)
         ((sqrt-iter (improve guess x) x) (sqrt-iter (improve guess x) x)))
=> (cond __skip((good-enough? guess x) guess)__
         ((sqrt-iter 5. x) (sqrt-iter 5. x)))
=> (cond __skip((good-enough? guess x) guess)__
         ((new-if-syntax (good-enough? 5. x)
               5.
               (sqrt-iter (improve 5. x) x))
          (new-if-syntax (good-enough? 5. x)
               5.
               (sqrt-iter (improve 5. x) x))))
=> (cond __skip((good-enough? guess x) guess)__
         ((cond __skip((good-enough? 5. x) 5.)__
                ((sqrt-iter 3.4 x) (sqrt-iter 3.4 x)))
          (cond __skip((good-enough? 5. x) 5.)__
                ((sqrt-iter 3.4 x) (sqrt-iter 3.4 x)))))
=> (cond __skip((good-enough? guess x) guess)__
         ((cond __skip((good-enough? 5. x) 5.)__
                ... 
                ;; finally `guess' will converge and `good-enough?'
                ;; will return #t, and the `cond' combination is
                ;; returning 3.000000001396984 which has the same
                ;; effect as #t on the predication of `cond'
                ((cond #t 3.000000001396984
                       ;; the following branch is not going to be
                       ;; evaluated any more
                       ((sqrt-iter 3.000000001396984 x)
                        (sqrt-iter 3.000000001396984 x)))
                 (cond #t 3.000000001396984
                       ;; the following branch is not going to be
                       ;; evaluated any more
                       ((sqrt-iter 3.000000001396984 x)
                        (sqrt-iter 3.000000001396984 x)))) ... )
          (cond __skip((good-enough? 5. x) 5.)__ ;; this branch is
                                                 ;; identical to the
                                                 ;; previous branch
                ... 
                ((sqrt-iter 3.4 x) (sqrt-iter 3.4 x)))))
=> (cond __skip((good-enough? guess x) guess)__
         ((cond __skip((good-enough? 5. x) 5.)__
                ... 
                ;; finally `guess' will converge and `good-enough?'
                ;; will return #t, and the `cond' combination is
                ;; returning 3.000000001396984 which has the same
                ;; effect as #t on the predication of `cond'
                ;; then it's time to return back
                (3.000000001396984
                 3.000000001396984) ... )
          (cond __skip((good-enough? 5. x) 5.)__
                ... 
                ;; finally `guess' will converge and `good-enough?'
                ;; will return #t, and the `cond' combination is
                ;; returning 3.000000001396984 which has the same
                ;; effect as #t on the predication of `cond'
                ;; then it's time to return back
                (3.000000001396984
                 3.000000001396984) ... )))
=> (cond __skip((good-enough? guess x) guess)__
         (3.000000001396984
          3.000000001396984))
=> 3.000000001396984

Hmm, beautiful! Finally this puzzle is solve :^) From this example
(acctually it's the previous mistake made by me which triggered this
kind of exploration) we can see the real power of scheme/lisp. That is
to say, scheme/lisp is syntax-less, meaning you can write programs
with high degree of freedom. I think it's kind of a magic that even I
made a mistake in the code, the program is able to produce the correct
answer (inefficiently) . Isn't it cool!
|#
