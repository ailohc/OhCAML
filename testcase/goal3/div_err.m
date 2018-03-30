let div = proc (x) (proc (y) (if iszero y then x / y else 0)) in
    ((div read) read)