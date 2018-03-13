letrec f (n1) = proc (n2) (
    if iszero n2 then 1 else (
        if iszero (n1 - n2) then 1 else (
            ((f (n1 - 1)) n2 - 1) + ((f (n1 - 1)) n2 - 1)
        )
    )
) in
((f read) read)