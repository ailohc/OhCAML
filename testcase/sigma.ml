letrec sigma (f) = proc (a) (proc (b) (
    if iszero ((f a) - (f b)) then 0 else (f a) + (((sigma f) (a + 1)) b)
  )) in
  (((sigma (proc (x) (2 * x))) read) read)