letrec f (x) = if iszero x then 0 else (f (x - 1))
    in (f read)