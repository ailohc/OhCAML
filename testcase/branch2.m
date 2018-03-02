let f = proc (x) (proc (y) (if iszero (x - y) then 0 else 1)) in
    ((f read) read)