#|
It's very interesting that scheme allows combinations whose operators can be
compound expressions. The following procedure whose operator is a expression
which returns a primitive operator/procedure +/- depending on the sign of b,
takes two arguments and apply the operator/procedure to them.
|#


(define (a-plus-abs-b a b)
  ((if (< b 0) - +) a b))

(a-plus-abs-b 3 -34)