#|

|#

;; wrong
(define (pascal l n)
  (if (< l 2)
      1
      (+ (pascal (-1+ l) n)
         (pascal (-1+ l) (-1+ n)))))

(define (pascal layer n)
  (newline)
  (dbg7 layer n)
  (cond ((or (= layer 1)
             (= n 1)
             (= n layer)) 1)
    ;    ((> n layer) 0)
        (else (let* ((pas#l-1:n (pascal (-1+ layer) n))
                     (pas#l-1:n-1 (pascal (-1+ layer) (-1+ n)))
                     (sum (+ pas#l-1:n pas#l-1:n-1)))
                (dbg7 pas#l-1:n pas#l-1:n-1 sum)
                sum))))

#|
(pascal 4 2)
=>
 layer: 4,	 n: 2,	

 layer: 3,	 n: 2,	

 layer: 2,	 n: 2,	

 layer: 2,	 n: 1,	
 pas#l-1:n: 1,	 pas#l-1:n-1: 1,	 sum: 2,	

 layer: 3,	 n: 1,	
 pas#l-1:n: 2,	 pas#l-1:n-1: 1,	 sum: 3,	
;Value: 3

As I can see here, n !> layer because I set it to 1 when n = layer. So
the function and be rewrite as follows.
|#

(define (pascal layer n)
  (if (or (= n 1)
          (= n layer)) 1
          (+ (pascal (-1+ layer) n)
             (pascal (-1+ layer) (-1+ n)))))

;; inefficient way
(define (print-pascal layer)
  (let loop ((lr 1))
    (if (<= lr layer)
        (begin (let loop2 ((n 1))
                 (if (<= n lr)
                     (begin (display (pascal lr n))
                            (display " ")
                            (loop2 (1+ n)))))
               (newline)
               (loop (1+ lr))))))

#|
(print-pascal 10)
=>
1 
1 1 
1 2 1 
1 3 3 1 
1 4 6 4 1 
1 5 10 10 5 1 
1 6 15 20 15 6 1 
1 7 21 35 35 21 7 1 
1 8 28 56 70 56 28 8 1 
1 9 36 84 126 126 84 36 9 1
;Unspecified return value
|#

;; print while calculating - impossible in fact the order of
;; calculating each pascal number is not for printing, i need to store
;; them into some sort of map, but does scheme has map built-in ? It
;; might be tedious and buggy to implement map for this simple
;; example, so now i will just leave it alone.
;; not working!
(define (print-pascal-eff layer)
  (define (in:pascal lr n)
    (cond ((= n 1) (display 1) (display " ") 1)
          ((= n lr) (display 1) (newline) 1)
          (else (let* ((pas:n (in:pascal (-1+ lr) n))
                      (pas:n-1 (in:pascal (-1+ lr) (-1+ n))))
                  (display pas:n-1)
                  (display " ")
                  (display pas:n)
                  (display " ")
                  (+ pas:n pas:n-1)))))
  (in:pascal layer (-1+ layer)))

(print-pascal-eff 5)

#|
(cond (#t (display "true") 3))
true
;Value: 3

This show that in a branch of `cond' you can multiple s-exps.
|#