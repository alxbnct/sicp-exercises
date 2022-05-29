(define (my+ a b)
  (display "a = ")
  (display a)
  (display ", b = ")
  (display b)
  (newline)
  (if (= a 0) b (1+ (my+ (-1+ a) b))))

(define (his+ a b)
  (display "a = ")
  (display a)
  (display ", b = ")
  (display b)
  (newline)
  (if (= a 0) b (his+ (-1+ a) (1+ b))))

#|

(my+ 9 3)
=>
a = 9, b = 3
a = 8, b = 3
a = 7, b = 3
a = 6, b = 3
a = 5, b = 3
a = 4, b = 3
a = 3, b = 3
a = 2, b = 3
a = 1, b = 3
a = 0, b = 3
;Value: 12

(his+ 9 3)
=>
a = 9, b = 3
a = 8, b = 4
a = 7, b = 5
a = 6, b = 6
a = 5, b = 7
a = 4, b = 8
a = 3, b = 9
a = 2, b = 10
a = 1, b = 11
a = 0, b = 12
;Value: 12

|#

#| Substitution module
(my+ 9 3)
=> (if (> 9 0) 3 (1+ (my+ (-1+ 9) 3)))
=> (if #f 3 (1+ (my+ 8 3)))
=> (if #f 3 (1+ (if (> 8 0) 3 (1+ (my+ (-1+ 8) 3)))))
=> (if #f 3 (1+ (if #f 3 (1+ (my+ 7 3)))))
...
=> (if #f 3 (1+ (if #f 3 (1+ ... (1+ (if #f 3 (1+ (if (> 0 0) 3 (1+ (my+ (-1+ 0) 3))))))))))
=> (if #f 3 (1+ (if #f 3 (1+ ... (1+ (if #f 3 (1+ (if #t 3 (1+ (my+ (-1+ 0) 3))))))))))

                  9 nested if statements
   /------------------------^-----------------------\
=> (if #f 3 (1+ (if #f 3 (1+ ... (1+ (if #f 3 (1+ 3))))))
=> (if #f 3 (1+ (if #f 3 (1+ ... (1+ (if #f 3 4))))))
...
=> (if #f 3 (1+ (if #f 3 (1+ 10))))
=> (if #f 3 (1+ (if #f 3 11)))
=> (if #f 3 (1+ 11))
=> 12

Or we can simplify this process by getting rid of the #f branch
(my+ 9 3)
=> (if (> 9 0) 3 (1+ (my+ (-1+ 9) 3)))
=> (if #f 3 (1+ (my+ 8 3)))
=> (1+ (my+ 8 3))
=> (1+ (if (> 8 0) 3 (1+ (my+ (-1+ 8) 3))))
=> (1+ (1+ (my+ 7 3)
...
 1 adds to 3 for 9 times
   /--------^--------\
=> (1+ (1+ (1+ ... (1+ (my+ 0 3))))
...
=> (1+ (1+ (1+ 9)))
=> 12

Apparently, this procedure has to use some sort of stack to store
function parameters for later calculations, and it's called a linear
recursion. So itss space complexity is O(n), and its time complexity
is O(n) as well. It's not effient in space complexity.

(his+ 9 3)
=> (if (= 0 9) 3 (his+ (-1+ 9) (1+ 3)))
=> (if #f 3 (his+ 8 4))
=> (his+ 8 4) ; getting rid of the #f branch
=> (if (= 0 8) 4 (his+ (-1+ 8) (1+ 4)))
=> (if #f 4 (his+ 7 5))
=> (his+ 7 5)

=> (his+ 6 6)
=> (his+ 5 7)
=> (his+ 4 8)
=> (his+ 3 9)
=> (his+ 2 10)
=> (his+ 1 11)
=> (his+ 0 12)
=> (if (= 0 0) 12 __skip(his+ (-1+ 0) (1+ 12))__)
=> 12

This is a tail recursion which the compiler will not produce a
function call for each iteration, instead it will be just a loop which
doesn't need a stack to store function parameters, so it's space
efficient with the space complexity of O(1), which is much more
efficient than the linear recursion one. And this is a iterative
procedure with time complexity of O(n). With tail recursion we can
write memory efficient loops even without any builtin loop statements.
|#

