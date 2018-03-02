let f = proc (x) (proc (y) (if iszero (x - y) then x * y else x / y)) in
    ((f read) read)