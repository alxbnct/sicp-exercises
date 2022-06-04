;; This is very inefficient
;; `*' Time: O(n) Space: O(n)
;; `square' Time: O(n) Space: O(1)
;; `in:fast-expt' Time: O(log n) Space: O(log n)
(define (fast-expt a n)
  (define (* a b)
    (if (= b 0)
        0
        (+ a (* a (-1+ b)))))
  (define (double n)
    (+ n n))

  ;; This procedure is not working when `n' is very large, it will
  ;; produces the following error
  ;; ;Aborting!: maximum recursion depth exceeded
  (define (square n)
    (define (in:square prod iter)
      (dbg prod iter)
      (if (= iter n)
          prod
          (in:square (+ prod n) (1+ iter))))
    (in:square n 1))
  (define (halve n)
    (if (even? n)
        (/ n 2)))
  (define (in:fast-expt a n)
    (dbg a n)
    (cond ((= n 0) 1)
          ((even? n) (square (in:fast-expt a (halve n))))
          (else (* a (in:fast-expt a (-1+ n))))))
  (in:fast-expt a n))

#|
(fast-expt 2 10)
=>
 a: 2,	 n: 10,	
 a: 2,	 n: 5,	
 a: 2,	 n: 4,	
 a: 2,	 n: 2,	
 a: 2,	 n: 1,	
 a: 2,	 n: 0,	
 prod: 2,	 iter: 1,	
 prod: 4,	 iter: 2,	
 prod: 4,	 iter: 1,	
 prod: 8,	 iter: 2,	
 prod: 12,	 iter: 3,	
 prod: 16,	 iter: 4,	
 prod: 32,	 iter: 1,	
 prod: 64,	 iter: 2,	
 prod: 96,	 iter: 3,	
 prod: 128,	 iter: 4,	
 prod: 160,	 iter: 5,	
 prod: 192,	 iter: 6,	
 prod: 224,	 iter: 7,	
 prod: 256,	 iter: 8,	
 prod: 288,	 iter: 9,	
 prod: 320,	 iter: 10,	
 prod: 352,	 iter: 11,	
 prod: 384,	 iter: 12,	
 prod: 416,	 iter: 13,	
 prod: 448,	 iter: 14,	
 prod: 480,	 iter: 15,	
 prod: 512,	 iter: 16,	
 prod: 544,	 iter: 17,	
 prod: 576,	 iter: 18,	
 prod: 608,	 iter: 19,	
 prod: 640,	 iter: 20,	
 prod: 672,	 iter: 21,	
 prod: 704,	 iter: 22,	
 prod: 736,	 iter: 23,	
 prod: 768,	 iter: 24,	
 prod: 800,	 iter: 25,	
 prod: 832,	 iter: 26,	
 prod: 864,	 iter: 27,	
 prod: 896,	 iter: 28,	
 prod: 928,	 iter: 29,	
 prod: 960,	 iter: 30,	
 prod: 992,	 iter: 31,	
 prod: 1024,	 iter: 32,	
;Value: 1024
|#

;; `*' Time: O(n) Space: O(n)
;; `square' Time: O(log n) Space: O(1)
;; `in:fast-expt' Time: O(log n) Space: O(log n)
(define (fast-expt a n)
  (define (* a b)
    (if (= b 0)
        0
        (+ a (* a (-1+ b)))))
  (define (double n)
    (+ n n))

  (define (fast-square n)
    (define (in:square p n slot)
      (cond ((= n 0) 0)
            ((even? n) (in:square (double p) (halve n) slot))
            (else (in:square (+ p slot)))))
    (in:square 0 n 0))
  (define (halve n)
    (if (even? n)
        (/ n 2)))
  (define (in:fast-expt a n)
    (dbg a n)
    (cond ((= n 0) 1)
          ((even? n) (square (in:fast-expt a (halve n))))
          (else (* a (in:fast-expt a (-1+ n))))))
  (in:fast-expt a n))

#|
(fast-expt 435 345)
=>
 a: 435,	 n: 345,	
 a: 435,	 n: 344,	
 a: 435,	 n: 172,	
 a: 435,	 n: 86,	
 a: 435,	 n: 43,	
 a: 435,	 n: 42,	
 a: 435,	 n: 21,	
 a: 435,	 n: 20,	
 a: 435,	 n: 10,	
 a: 435,	 n: 5,	
 a: 435,	 n: 4,	
 a: 435,	 n: 2,	
 a: 435,	 n: 1,	
 a: 435,	 n: 0,	
;Aborting!: maximum recursion depth exceeded

Why? This is just because of the procedure `*' which is linear
recursive, its space complexity is O(n).
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             Testing                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (* a b)
  (if (= b 0)
      0
      (+ a (* a (-1+ b)))))
(define (double n)
  (+ n n))

;; not  working
(define (fast-square n)
  (define (in:square p nn slot)
    (dbg p nn slot)
    (cond ((= nn 0) (+ p slot))
          ((even? nn) (in:square (double p) (halve nn) slot))
          (else (in:square p (-1+ nn) (+ slot n)))))
  (in:square n n 0))
(fast-square 11)

;; only work for even numbers
(define (fast-square n)
  (define (in:square p nn slot)
    (dbg p nn slot)
    (cond ((= nn 1) (+ p slot))
          ((even? nn) (in:square (double p) (halve nn) slot))
          (else (in:square p (-1+ nn) (+ slot p)))))
  (in:square n n (if (even? n) 0 n)))
(fast-square 14)

;; now it's working for positive numbers, but not for negative numbers
;; and 0
(define (fast-square n)
  (define (in:square p nn slot)
    (dbg p nn slot)
    (cond ((= nn 1) (+ p slot))
          ((even? nn) (in:square (double p) (halve nn) slot))
          (else (in:square p (-1+ nn) (+ slot p)))))
  (in:square (abs n) (abs n) 0))
(fast-square 0)

;; this now is working for all integers!
;; Time: O(log n) Space: O(1)
(define (fast-square n)
  (define (in:square p nn slot)
    (dbg p nn slot)
    (cond ((= nn 1) (+ p slot))
          ((even? nn) (in:square (double p) (halve nn) slot))
          (else (in:square p (-1+ nn) (+ slot p)))))
  (in:square (abs n) (abs n) 0))
(fast-square -43534653)
(fast-square 0)

(define (halve n)
  (if (even? n)
      (/ n 2)))
(define (in:fast-expt a n)
  (dbg a n)
  (cond ((= n 0) 1)
        ((even? n) (square (in:fast-expt a (halve n))))
        (else (* a (in:fast-expt a (-1+ n))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `*' Time: O(n) Space: O(n)
;; `square' Time: O(log n) Space: O(1)
;; `in:fast-expt' Time: O(log n) Space: O(log n)
(define (fast-expt a n)
  (define (* a b)
    (if (= b 0)
        0
        (+ a (* a (-1+ b)))))
  (define (double n)
    (+ n n))
  
  (define (fast-square n)
    (define (in:square p nn slot)
      (dbg p nn slot)
      (cond ((= nn 1) (+ p slot))
            ((even? nn) (in:square (double p) (halve nn) slot))
            (else (in:square p (-1+ nn) (+ slot p)))))
    (in:square (abs n) (abs n) 0))
  
  (define (halve n)
    (if (even? n)
        (/ n 2)))
  (define (in:fast-expt a n)
    (dbg a n)
    (cond ((= n 0) 1)
          ((even? n) (fast-square (in:fast-expt a (halve n))))
          (else (* a (in:fast-expt a (-1+ n))))))
  (in:fast-expt a n))

#|
(fast-expt 324 24)
=>
...
 p: 424111157047851297809633158119004747854525065249439029919744,	 nn: 4,	 slot: 94492108169564181706457442767912539367486418978044746137600,	
 p: 848222314095702595619266316238009495709050130498878059839488,	 nn: 2,	 slot: 94492108169564181706457442767912539367486418978044746137600,	
 p: 1696444628191405191238532632476018991418100260997756119678976,	 nn: 1,	 slot: 94492108169564181706457442767912539367486418978044746137600,	
;Value: 1790936736360969372944990075243931530785586679975800865816576

But (fast-expt 324 25) will produce the following error
...
 p: 848222314095702595619266316238009495709050130498878059839488,	 nn: 2,	 slot: 94492108169564181706457442767912539367486418978044746137600,	
 p: 1696444628191405191238532632476018991418100260997756119678976,	 nn: 1,	 slot: 94492108169564181706457442767912539367486418978044746137600,	
;Aborting!: maximum recursion depth exceeded

This is still because of the procedure `*' is linear recursive.
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `*' Time: O(n) Space: O(n)
;; `square' Time: O(log n) Space: O(1)
;; `in:fast-expt' Time: O(log n) Space: O(1)
(define (fast-expt a n)
  (define (* a b)
    (if (= b 0)
        0
        (+ a (* a (-1+ b)))))
  (define (double n)
    (+ n n))
  
  (define (fast-square n)
    (define (in:square p nn slot)
      (dbg p nn slot)
      (cond ((= nn 1) (+ p slot))
            ((even? nn) (in:square (double p) (halve nn) slot))
            (else (in:square p (-1+ nn) (+ slot p)))))
    (in:square (abs n) (abs n) 0))
  
  (define (halve n)
    (if (even? n)
        (/ n 2)))
  (define (in:fast-expt a n slot)
    (dbg a n slot)
    (cond ((= n 0) slot)
          ((even? n) (in:fast-expt (fast-square a) (halve n) slot))
          (else (in:fast-expt a (- n 1) (* a slot)))))
  (in:fast-expt a n 1))

#|
(fast-expt 324 24)
=>
 p: 4480327901140333639941336854183943340032,	 nn: 2,	 slot: 5786903910680014995394489879797392474112,	
 p: 8960655802280667279882673708367886680064,	 nn: 1,	 slot: 5786903910680014995394489879797392474112,	
 a: 14747559712960682275277163588165279154176,	 n: 1,	 slot: 121439531096594251776,	
;Aborting!: maximum recursion depth exceeded

I still didn't realized that I can optimize `*' to O(n) time
complexity and O(1) space complexity.
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (fast-* a b)
  (define (in:* a n slot)
    (dbg a n slot)
    (cond ((= 0 n) slot)
          ((even? n) (in:* (double a) (halve n) slot))
          (else (in:* a (- n 1) (+ slot a)))))
  (in:* a b 0))

#|
(fast-* 3 499)
=>
 a: 3,	 n: 499,	 slot: 0,	
 a: 3,	 n: 498,	 slot: 3,	
 a: 6,	 n: 249,	 slot: 3,	
 a: 6,	 n: 248,	 slot: 9,	
 a: 12,	 n: 124,	 slot: 9,	
 a: 24,	 n: 62,	 slot: 9,	
 a: 48,	 n: 31,	 slot: 9,	
 a: 48,	 n: 30,	 slot: 57,	
 a: 96,	 n: 15,	 slot: 57,	
 a: 96,	 n: 14,	 slot: 153,	
 a: 192,	 n: 7,	 slot: 153,	
 a: 192,	 n: 6,	 slot: 345,	
 a: 384,	 n: 3,	 slot: 345,	
 a: 384,	 n: 2,	 slot: 729,	
 a: 768,	 n: 1,	 slot: 729,	
 a: 768,	 n: 0,	 slot: 1497,	
;Value: 1497
|#

;; i can optimize `fast-*' a little bit
(define (fast-* a b)
  (define (in:* a n slot)
    (dbg a n slot)
    (cond ((= 0 n) slot)
          ((even? n) (in:* (double a) (halve n) slot))
          (else (in:* a (- n 1) (+ slot a)))))
  (if (> a b)
        (in:* a b 0)
        (in:* b a 0)))

#|
(fast-* 3 499)
=>
 a: 499,	 n: 3,	 slot: 0,	
 a: 499,	 n: 2,	 slot: 499,	
 a: 998,	 n: 1,	 slot: 499,	
 a: 998,	 n: 0,	 slot: 1497,	
;Value: 1497
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `*' Time: O(log n) Space: O(1)
;; `square' Time: O(log n) Space: O(1)
;; `in:fast-expt' Time: O(log n) Space: O(1)
(define (fast-expt a n)
  (define (fast-* a b)
    (define (in:* a n slot)
      (dbg a n slot)
      (cond ((= 0 n) slot)
            ((even? n) (in:* (double a) (halve n) slot))
            (else (in:* a (- n 1) (+ slot a)))))
    (if (> a b)
        (in:* a b 0)
        (in:* b a 0)))
  (define (double n)
    (+ n n))
  
  (define (fast-square n)
    (define (in:square p nn slot)
      (dbg p nn slot)
      (cond ((= nn 1) (+ p slot))
            ((even? nn) (in:square (double p) (halve nn) slot))
            (else (in:square p (-1+ nn) (+ slot p)))))
    (in:square (abs n) (abs n) 0))
  
  (define (halve n)
    (if (even? n)
        (/ n 2)))
  (define (in:fast-expt a n slot)
    (dbg a n slot)
    (cond ((= n 0) slot)
          ((even? n) (in:fast-expt (fast-square a) (halve n) slot))
          (else (in:fast-expt a (- n 1) (fast-* a slot)))))
  (in:fast-expt a n 1))

#|
(fast-expt 324 25)
=>
...
 a: 557147053540833693429540700375295476640902376443463053206355968,	 n: 1,	 slot: 23116449040120383404636084003738339333627707868696427318214656,	
 a: 557147053540833693429540700375295476640902376443463053206355968,	 n: 0,	 slot: 580263502580954076834176784379033815974530084312159480524570624,	
 a: 14747559712960682275277163588165279154176,	 n: 0,	 slot: 580263502580954076834176784379033815974530084312159480524570624,	
;Value: 580263502580954076834176784379033815974530084312159480524570624
|#