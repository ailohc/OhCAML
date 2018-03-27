let div = proc (x) (proc (y) (if iszero y then x * y else x / y)) in
    ((div read) read)