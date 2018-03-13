letrec fact (n) = if iszero n then 1 else (
    if iszero (n - 1) then 1 else n * (fact (n - 1))
) in
letrec f (n1) = proc (n2) (
    (fact n1) / ((fact n2) * (fact (n1 - n2)))
) in
((f read) read)