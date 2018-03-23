let check = proc (a) (proc (b) (proc (c) (
    if (a > b) then
        (if (a <= c) then
            iszero ((c * c) - (a * a) - (b * b))
        else
            iszero ((a * a) - (b * b) - (c * c)))
    else
        (if (b <= c) then
            iszero ((c * c) - (a * a) - (b * b))
        else
            iszero ((b * b) - (a * a) - (c * c)))
)))
in (((check read) read) read)