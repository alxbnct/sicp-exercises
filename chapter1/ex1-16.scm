;; Time: O(n) Space: O(n)
;; This is a linear recursion.
(define (expt a n)
  (dbg a n)
  (if (= n 1)
      a
      (* a (expt a (-1+ n)))))

#|
(expt 2 10)
=>
 a: 2,	 n: 10,	
 a: 2,	 n: 9,	
 a: 2,	 n: 8,	
 a: 2,	 n: 7,	
 a: 2,	 n: 6,	
 a: 2,	 n: 5,	
 a: 2,	 n: 4,	
 a: 2,	 n: 3,	
 a: 2,	 n: 2,	
 a: 2,	 n: 1,	
;Value: 1024
|#

;; Time: O(n) Space: O(1)
;; This is a tail recursive
(define (expt-iter a n)
  (define (in:expt-iter ret iter)
    (dbg ret iter)
    (if (= iter n)
        ret
        (in:expt-iter (* ret a) (1+ iter))))
  (in:expt-iter a 1))

#|
(expt-iter 2 10)
=>
 ret: 2,	 iter: 1,	
 ret: 4,	 iter: 2,	
 ret: 8,	 iter: 3,	
 ret: 16,	 iter: 4,	
 ret: 32,	 iter: 5,	
 ret: 64,	 iter: 6,	
 ret: 128,	 iter: 7,	
 ret: 256,	 iter: 8,	
 ret: 512,	 iter: 9,	
 ret: 1024,	 iter: 10,	
;Value: 1024
|#

#| Not working
(define (expt-iter-log a n)
  (dbg a n)
  (if (= n 1)
      a
      (if (even? n)
          (expt-iter-log (* a a) (floor (/ n 2)))
          (expt-iter-log (* a a a) (floor (/ n 2))))
      ))
|#

;; Time: O(log n) Space: O(1)
;; This is tail recursive!
;; Cool! This is also called "successive squaring", and I didn't
;; realise that I was solving exersise 16 when I was writing this
;; procesure. I think this is not quite un-straightforward as sicp
;; commented on page 59 because this is the first idea I've come up
;; with :^)
(define (expt-iter-log a n)
  (define (in:expt a n slot)
    (dbg a n slot)
    (if (= n 1)
        (* a slot)
        (if (even? n)
            (in:expt (* a a) (floor (/ n 2)) slot)
            (in:expt (* a a) (floor (/ n 2)) (* a slot)))))
  (in:expt a n 1))

#|
(expt-iter-log 2 10)
=>
 a: 2,	 n: 10,	 slot: 1,	
 a: 4,	 n: 5,	 slot: 1,	
 a: 16,	 n: 2,	 slot: 4,	
 a: 256,	 n: 1,	 slot: 4,	
;Value: 1024

(expt-iter-log 324 25)
=>
 a: 324,	 n: 25,	 slot: 1,	
 a: 104976,	 n: 12,	 slot: 324,	
 a: 11019960576,	 n: 6,	 slot: 324,	
 a: 121439531096594251776,	 n: 3,	 slot: 324,	
 a: 14747559712960682275277163588165279154176,	 n: 1,	 slot: 39346408075296537575424,	
;Value: 580263502580954076834176784379033815974530084312159480524570624
|#

;; other solution from http://community.schemewiki.org/?sicp-ex-1.16
(define (iter-fast-expt b n) 
  (define (iter N B A)
    (dbg N B A)
    (cond ((= 0 N) A)
          ((even? N) (iter (/ N 2) (square B) A)) 
          (else (iter (- N 1) B (* B A)))))
  (iter n b 1))

#|
(iter-fast-expt 2 10)
=>
 n: 10,	 b: 2,	 a: 1,	
 n: 5,	 b: 4,	 a: 1,	
 n: 4,	 b: 4,	 a: 4,	
 n: 2,	 b: 16,	 a: 4,	
 n: 1,	 b: 256,	 a: 4,	
 n: 0,	 b: 256,	 a: 1024,	
;Value: 1024

(iter-fast-expt 324 25)
=>
 n: 25,	 b: 324,	 a: 1,	
 n: 24,	 b: 324,	 a: 324,	
 n: 12,	 b: 104976,	 a: 324,	
 n: 6,	 b: 11019960576,	 a: 324,	
 n: 3,	 b: 121439531096594251776,	 a: 324,	
 n: 2,	 b: 121439531096594251776,	 a: 39346408075296537575424,	
 n: 1,	 b: 14747559712960682275277163588165279154176,	 a: 39346408075296537575424,	
 n: 0,	 b: 14747559712960682275277163588165279154176,	 a: 580263502580954076834176784379033815974530084312159480524570624,	
;Value: 580263502580954076834176784379033815974530084312159480524570624

Comparing this to my solution, obviously my solution is superior than
this, because my solution takes fewer iteractions and it's more
straightforward and understandable.
|#


;; Time: O(log n) Space: O(log n)
;; Can this be called "logarithmic recursion" as we can infer?
(define (expt-non-tail-log a n)
  (define (square n)
    (* n n))
  (dbg a n)
  (cond ((= n 1) a)
        ((even? n) (square (expt-non-tail-log a (/ n 2))))
        (else (* a (square (expt-non-tail-log a (floor (/ n 2))))))))

#|
(expt-non-tail-log 2 10)
 a: 2,	 n: 10,	
 a: 2,	 n: 5,	
 a: 2,	 n: 2,	
 a: 2,	 n: 1,	
;Value: 1024

(expt-non-tail-log 324 25)
=>
 a: 324,	 n: 25,	
 a: 324,	 n: 12,	
 a: 324,	 n: 6,	
 a: 324,	 n: 3,	
 a: 324,	 n: 1,	
;Value: 580263502580954076834176784379033815974530084312159480524570624
|#

;; In sicp it's slightly different
(define (expt-non-tail-log-sicp a n)
  (define (square n)
    (* n n))
  (dbg a n)
  (cond ((= n 0) 1)
        ((even? n) (square (expt-non-tail-log a (/ n 2))))
        (else (* a (expt-non-tail-log a (-1+ n))))))

#|
(expt-non-tail-log-sicp 2 10)
=>
 a: 2,	 n: 10,	
 a: 2,	 n: 5,	
 a: 2,	 n: 2,	
 a: 2,	 n: 1,	
;Value: 1024

(expt-non-tail-log-sicp 324 25)
=>
;Value: expt-non-tail-log-sicp
 a: 324,	 n: 25,	
 a: 324,	 n: 24,	
 a: 324,	 n: 12,	
 a: 324,	 n: 6,	
 a: 324,	 n: 3,	
 a: 324,	 n: 1,	
;Value: 580263502580954076834176784379033815974530084312159480524570624

(expt-non-tail-log-sicp 324 24)
=>
 a: 324,	 n: 24,	
 a: 324,	 n: 12,	
 a: 324,	 n: 6,	
 a: 324,	 n: 3,	
 a: 324,	 n: 1,	
;Value: 1790936736360969372944990075243931530785586679975800865816576

(expt-non-tail-log 324 24)
=>
 a: 324,	 n: 24,	
 a: 324,	 n: 12,	
 a: 324,	 n: 6,	
 a: 324,	 n: 3,	
 a: 324,	 n: 1,	
;Value: 1790936736360969372944990075243931530785586679975800865816576

Obviously my version of `expt-non-tail-log' is better because when `n'
is odd, it can do one iteration less. (When `n' is even they are the same).
|#

#| Note: P.59
In the second paragraph, observe that computing b^(2n) using
`fast-expt' requires only one more multiplication than computing
b^(n). Because computing b^(n) requires at least (log_2 n)
multiplications, and computing b^(2n) requires at least (log_2 2n) =
(1 + log_2 n)  multiplications.
|#