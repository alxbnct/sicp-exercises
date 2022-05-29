#|
Proof: Let f(n) = fib(n)
By definition, f(n) = f(n-1) + f(n-2)
=> f(n) - f(n-1) - f(n-2) = 0                                          ; (1)
Let phi = (1 + sqrt(5))/2, psi = (1 - sqrt(5))/2, hence
phi + psi = 1, phi * psi = -1                                          ; (2)
Apply (2) to (1), then
f(n) - (phi + psi) f(n-1) + phi * psi * f(n-2) = 0,
=> f(n) - phi * f(n-1) = psi * f(n-1) - phi * psi * f(n-2)
   = psi (f(n-1) - phi * f(n-2))                                       ; (3)
Notice that in (3) there's a recursive definition of f(n) - phi *
f(n-1), and we can continue to apply it the RHS of (3) as follows
f(n) - (phi + psi) f(n-1) + phi * psi * f(n-2) = 0,
=> f(n) - phi * f(n-1) = psi * f(n-1) - phi * psi * f(n-2)
   = psi (f(n-1) - phi * f(n-2))
   = psi [ psi (f(n-2) - phi * f(n-3)) ]
   = psi^2 [ f(n-2) - phi * f(n-3) ]
   = ...
   = psi^(n-2) [ f(2) - phi * f(1) ]
   = psi^(n-1) [ f(1) - phi * f(0) ]
   = psi^(n-1)                                                         ; (4)
Since phi and psi are equal status (that's to say it's equivalent to
exchange phi and psi in (4)),
=> f(n) - psi * f(n-1) = phi^(n-1)                                     ; (5)
To eliminate f(n-1), we do (5) * phi - (4) * psi
=> phi * f(n) - phi*psi * f(n-1) - psi * f(n) + psi*phi * f(n-1)
   = (phi - psi) * f(n)
   = phi^n - psi^n
=> f(n) = (phi^n - psi^n) / (phi - psi)
        = (phi^n - psi^n) / sqrt(5)
For psi ~~ -0.618, | psi^n | < 1, so f(n) ~~ phi^n / sqrt(5)           q.e.d
|#

