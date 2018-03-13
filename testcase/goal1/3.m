letrec f (x) = proc (y) (
    if iszero x then 1
    else (
        if iszero (y - x) then 1
        else (
            ((f (x - 1)) (y - 1)) + ((f (x - 1)) y)
        )
    )
) in
((f read) read)