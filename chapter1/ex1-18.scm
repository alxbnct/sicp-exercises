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