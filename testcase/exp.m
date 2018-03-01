letrec exp (x) = proc (y) (if iszero y then 1 else x * ((exp x) (y - 1)))
    in ((exp read) 3)