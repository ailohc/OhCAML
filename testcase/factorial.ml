letrec factorial (n) = if iszero n then 1 else (factorial (n - 1) * n) in
  (factorial read)