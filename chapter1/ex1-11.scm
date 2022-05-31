#|
I've learnt about
 - tail recursion
 - linear recursion
 - tree recursion
then, what about "graph recursion", is it BFS/DFS?
|#

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (-1+ n))
                 (fib (- n 2))))))

#|
(fib 30)
=> ;Value: 832040
|#

(define (fib-iter n)
  (define (fib prev cur counter)
    (display "prev: ")
    (display prev)
    (display " cur: ")
    (display cur)
    (display " counter: ")
    (display counter)
    (newline)
    (if (> counter (- n 2))
        (if (= n 0)
            0
            cur)
        (fib cur (+ prev cur) (1+ counter))))
  (fib 0 1 0))

#|
(fib-iter 30)
=>
...
prev: 10946 cur: 17711 counter: 21
prev: 17711 cur: 28657 counter: 22
prev: 28657 cur: 46368 counter: 23
prev: 46368 cur: 75025 counter: 24
prev: 75025 cur: 121393 counter: 25
prev: 121393 cur: 196418 counter: 26
prev: 196418 cur: 317811 counter: 27
prev: 317811 cur: 514229 counter: 28
prev: 514229 cur: 832040 counter: 29
;Value: 832040


(fib 0)
=> ;Value: 0

(fib-iter 0)
=>
prev: 0 cur: 1 counter: 0
;Value: 0

|#

(define (fib-iter2 n)
  ; can't call fib-iter here because there's a outer definition using
  ; this name, so you should define another one first to shadow the
  ; outside definition.
  ;(fib-iter 1 0 n)
  (define (fib-iter a b count)
    (if (= 0 count)
        b
        (fib-iter (+ a b) a (- count 1))))
  (fib-iter 1 0 n))

#|
(fib-iter2 0) => ;Value: 0
|#

#|
(define dbg
  (lambda (sym-list)
    (if (not (null? sym-list))
        (begin
          (let ((sym (car sym-list)))
            (display 
             (string-append " " (symbol->string sym) ": " sym ", "))
            (dbg (cdr sym-list))))
        (newline))))

(define-syntax dbg
  (syntax-rules ()
    ((_ sym)
     (begin (display
             (string-append " " (symbol->string (quote sym))  ": "
                            (number->string sym) ", "))
            (newline)))))

;; not working
(define-syntax dbgl
  (syntax-rules ()
    ((_ ...)
     (begin (let loop ((lst ...))
              (if (not (null? lst))
                  (begin (display " " (symbol->string (quote sym)) ": ")
                         (display sym)
                         (display ", ")
                         (loop (cdr lst)))))))))

;; partially working
(define-syntax dbg2
  (syntax-rules ()
    ((_ fst ...)
     (begin (let loop ((lst (list fst ...))
                       (symbol-names (map symbol->string (quote (list fst ...)))))
              (if (not (null? lst))
                  (let ((sym (car lst)))
                    (begin (display (string-append " " (car symbol-names) ": "))
                           (display sym)
                           (display ", ")
                           (loop (cdr lst) (cdr symbol-names))
                           (newline)))))))))

;; not working
(define-syntax dbg3
  (syntax-rules ()
    ((_ sym)
     (begin (display (string-append " " (symbol->string (quote sym))))
            (display sym)
            (display ", ")
            (newline)))
    ((_ sym ...)
     (begin (display (string-append " " (symbol->string (quote sym))))
            (display sym)
            (display ", ")
            (dbg3 ...)
            (newline)))))

;; not working
(define-syntax dbg4
  (syntax-rules ()
    ((_ sym)
     (begin (display (string-append " " (symbol->string (quote sym))))
            (display sym)
            (display ", ")
            (newline)))
    ((_ '(lst))
     ())))

;; working!
(define-syntax dbg5
  (syntax-rules ()
    ((_ fst ...)
     (begin (let loop ((lst (list fst ...))
                       (symbol-names (map symbol->string (quote (fst ...)))))
              (if (not (null? lst))
                  (let ((sym (car lst)))
                    (begin (display (string-append " " (car symbol-names) ": "))
                           (display sym)
                           (display ", ")
                           (loop (cdr lst) (cdr symbol-names))
                           (newline)))))))))

#|

;; finer
(define-syntax dbg6
  (syntax-rules ()
    ((_ fst ...)
     (begin (let loop ((lst (list fst ...))
                       (symbol-names (map symbol->string (quote (fst ...)))))
              (if (not (null? lst))
                  (let ((sym (car lst)))
                    (begin (display (string-append " " (car symbol-names) ": "))
                           (display sym)
                           (display ",\t")
                           ;; when it's the last element of the list,
                           ;; print a newline
                           (if (null? (cdr lst)) (newline))
                           (loop (cdr lst) (cdr symbol-names))))))))))

#| example of dbg6
(define (my-sqrt-cauchy x)
  (define (good-enough? guess previous-guess)
    (< (abs (- guess previous-guess)) 0.0001))
  (define (average x y)
    (/ (+ x y) 2))
  (define (improve guess x)
    (average guess (/ x guess)))
  (define (sqrt-iter guess previous-guess x iter)
    (dbg6 iter guess)
    (if (good-enough? guess previous-guess)
        guess
        (sqrt-iter (improve guess x) guess x (1+ iter))))
  (sqrt-iter 1.0 -1 x 1))

(my-sqrt-cauchy 59)
=>
 iter: 1,	 guess: 1.,	
 iter: 2,	 guess: 30.,	
 iter: 3,	 guess: 15.983333333333333,	
 iter: 4,	 guess: 9.83733924226625,	
 iter: 5,	 guess: 7.91744797709882,	
 iter: 6,	 guess: 7.684672057337308,	
 iter: 7,	 guess: 7.681146556937517,	
 iter: 8,	 guess: 7.681145747868651,	
;Value: 7.681145747868651
|#

;; finer
(define-syntax dbg7
  (syntax-rules ()
    ((_ fst ...)
     (begin (let loop ((lst (list fst ...))
                       (symbol-names (map symbol->string (quote (fst ...)))))
              (if (not (null? lst))
                  (begin (display (string-append " " (car symbol-names) ": "))
                         (display (car lst))
                         (display ",\t")
                         ;; when it's the last element of the list,
                         ;; print a newline
                         (if (null? (cdr lst)) (newline))
                         (loop (cdr lst) (cdr symbol-names)))))))))

#| macro expansion test
(define pi 3.14)
(unsyntax (syntax '(dbg7 pi)
                  (nearest-repl/environment)))
=>
;Value:
((let ()
   (define .loop.1-0
     (named-lambda (loop .lst.2-0 .symbol-names.2-0)
       (if (not (null? .lst.2-0))
           (begin
             (display (string-append " " (car .symbol-names.2-0) ": "))
             (display (car .lst.2-0))
             (display ",\t")
             (if (null? (cdr .lst.2-0)) (newline))
             (.loop.1-0 (cdr .lst.2-0) (cdr .symbol-names.2-0))))))
   .loop.1-0)
 (list pi)
 (map symbol->string (quote (pi))))

If I evaluate the expanded form, I can get:
 pi: 3.14,	
;Unspecified return value

It's really amazing, I currently know nothing about this
`((let () (define loop-name ...) loop-name) args ...)' syntax.
|#

(define (count-change amount)
  (define (cc amount kinds-of-coins)
;    (dbg amount)
;;    (let ((asdf 9))
;;      (dbg6 amount kinds-of-coins asdf asdf))
;;;    (dbg7 amount kinds-of-coins)

;    (display "amount: ")
;    (display amount)
;    (display " kinds of coins: ")
;    (display kinds-of-coins)
;    (newline)
    (cond ((= kinds-of-coins 1) 1)
          ((or (< amount 0) (<= kinds-of-coins 0)) 0)
          (else (+ (cc amount
                       (-1+ kinds-of-coins))
                   (cc (- amount (first-denomination kinds-of-coins))
                       kinds-of-coins)))))
  (define (first-denomination kinds-of-coins)
    ;; I can use case syntax here
    (cond ((= kinds-of-coins 1) 1)
          ((= kinds-of-coins 2) 5)
          ((= kinds-of-coins 3) 10)
          ((= kinds-of-coins 4) 25)
          ((= kinds-of-coins 5) 50)))
  (cc amount 5))

#|
The above function definition shows that when defining a procedure,
its parameters are shadowing variables outside.

(count-change 100)
=>
 amount: 100,	 kinds-of-coins: 5,	
 amount: 50,	 kinds-of-coins: 5,	
 amount: 0,	 kinds-of-coins: 5,	; return (+ (cc ...) (cc ...))
 amount: -50,	 kinds-of-coins: 5,	; return 0 because amount < 0
 amount: 0,	 kinds-of-coins: 4,	
 amount: -25,	 kinds-of-coins: 4,	
 amount: 0,	 kinds-of-coins: 3,	
 amount: -10,	 kinds-of-coins: 3,	
 amount: 0,	 kinds-of-coins: 2,	
 amount: -5,	 kinds-of-coins: 2,	
 amount: 0,	 kinds-of-coins: 1,	
 amount: 50,	 kinds-of-coins: 4,	
 amount: 25,	 kinds-of-coins: 4,	
 amount: 0,	 kinds-of-coins: 4,	
 amount: -25,	 kinds-of-coins: 4,	
 amount: 0,	 kinds-of-coins: 3,	
 amount: -10,	 kinds-of-coins: 3,	
 amount: 0,	 kinds-of-coins: 2,	
 amount: -5,	 kinds-of-coins: 2,	
 amount: 0,	 kinds-of-coins: 1,	
...
;Value: 292

Is there an iterative way to do that? Probabliy there is, but the tree
recursion way of thinking the problem is more natural and simpler I suppose.

But wait, why the answer is correct? It's not sured whether the code
has bug or not, how can you prove the correctness the the program?

Maybe i can start with some small number to calculate by hand, then
compare the results between them.

(count-change 10)
by hand => 10 -> 5 + 5                                             -+
              -> 5 + 1 + 1 + 1 + 1 + 1                              |==> 4 ways
              -> 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1              |
              -> 10                                                -+

(count-change 12)
by hand => 10 -> 5 + 5 + 1 + 1                                     -+
              -> 5 + 1 + 1 + 1 + 1 + 1 + 1 + 1                      |==> 4 ways
              -> 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1      |
              -> 10 + 1 + 1                                        -+

(count-change 10)
=>
 amount: 10,	 kinds-of-coins: 5,	
 amount: -40,	 kinds-of-coins: 5,	
 amount: 10,	 kinds-of-coins: 4,	
 amount: -15,	 kinds-of-coins: 4,	
 amount: 10,	 kinds-of-coins: 3,	
 amount: 0,	 kinds-of-coins: 3,	
 amount: -10,	 kinds-of-coins: 3,	
 amount: 0,	 kinds-of-coins: 2,	
 amount: -5,	 kinds-of-coins: 2,	
 amount: 0,	 kinds-of-coins: 1,	
 amount: 10,	 kinds-of-coins: 2,	
 amount: 5,	 kinds-of-coins: 2,	
 amount: 0,	 kinds-of-coins: 2,	
 amount: -5,	 kinds-of-coins: 2,	
 amount: 0,	 kinds-of-coins: 1,	
 amount: 5,	 kinds-of-coins: 1,	
 amount: 10,	 kinds-of-coins: 1,	
;Value: 4

(count-change 12)
=>
 amount: 12,	 kinds-of-coins: 5,	
 amount: -38,	 kinds-of-coins: 5,	
 amount: 12,	 kinds-of-coins: 4,	
 amount: -13,	 kinds-of-coins: 4,	
 amount: 12,	 kinds-of-coins: 3,	
 amount: 2,	 kinds-of-coins: 3,	
 amount: -8,	 kinds-of-coins: 3,	
 amount: 2,	 kinds-of-coins: 2,	
 amount: -3,	 kinds-of-coins: 2,	
 amount: 2,	 kinds-of-coins: 1,	
 amount: 12,	 kinds-of-coins: 2,	
 amount: 7,	 kinds-of-coins: 2,	
 amount: 2,	 kinds-of-coins: 2,	
 amount: -3,	 kinds-of-coins: 2,	
 amount: 2,	 kinds-of-coins: 1,	
 amount: 7,	 kinds-of-coins: 1,	
 amount: 12,	 kinds-of-coins: 1,	
;Value: 4

(count-change 300)
=> ;Value: 9590

|#

#| case syntax
(let ((a 10))
  (case a
    ((1) 10)
    ((2) 20)
    ((3) 50)
    ((10) 80)
    (else 45)))
;Value: 80

(unsyntax (syntax '(case 10
                     ((1) 10)
                     ((2) 20)
                     ((3) 50)
                     ((10) 80)
                     (else 45))
                  (nearest-repl/environment)))
;Value:
(let ((#[uninterned-symbol 22 key] 10))
  (cond ((eq? 1 #[uninterned-symbol 22 key]) 10)
        ((eq? 2 #[uninterned-symbol 22 key]) 20)
        ((eq? 3 #[uninterned-symbol 22 key]) 50)
        ((eq? 10 #[uninterned-symbol 22 key]) 80)
        (else 45)))
|#

(define (f n)
;  (dbg7 n)
  (if (< n 3)
      n
      (+ (f (- n 1))
         (* 2 (f (- n 2)))
         (* 3 (f (- n 3))))))

(f 10)

;; not working
(define (f-iter n)
  (let loop ((sum n)
             (f:n-1 2)
             (f:n-2 1)
             (f:n-3 0))
    (if (< sum 3)
        sum
        (loop (+ f:n-1
                 (* 2 f:n-2)
                 (* 3 f:n-3))
              sum
              f:n-1
              f:n-2))))

(define (f-iter n)
  (let loop ((iter 0)
             (f:n-1 2)
             (f:n-2 1)
             (f:n-3 0))
    (dbg7 iter f:n-1 f:n-2 f:n-3)
    (if (< n 3)
        n
        (begin (if (= iter (- n 3))     ; we need to subtract 3 from
                                        ; iter because we've already
                                        ; know the first 3
                   (+ f:n-1
                      (* 2 f:n-2)
                      (* 3 f:n-3))
                   (loop (1+ iter)
                         (+ f:n-1
                            (* 2 f:n-2)
                            (* 3 f:n-3))
                         f:n-1
                         f:n-2))))
    ))


(f-iter 10)

(f 15)
(f-iter 15)