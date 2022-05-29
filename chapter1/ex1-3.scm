#|
(define sum-two-larger
  (lambda (a b c)
    (let* ((lst (list a b c))
           (max-el (max lst)))
      (+ max-el (max (filter (lambda (e) (= max-el e)) lst))))))


(define sum-two-larger
  (lambda (a b c)
    (let* ((lst (list a b c))
          (max-el (max a b c))
          (rest (filter (lambda (e) (not (= max-el e))) lst)))
      (+ max-el (max (car rest) (cdr rest))))))
|#

(define sum-two-larger
  (lambda (a b c)
    (let* ((lstt (list a b c))
           (sorted (sort lstt >))
           (l1 (car sorted))
           (l2 (cadr sorted)))
      (+ l1 l2))))