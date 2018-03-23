let check = proc (a) (proc (b) (proc (c) (
    if (a <= b) then
        (if (b > c) then
            iszero ((b * b) - (a * a) - (c * c))
        else
            iszero ((c * c) - (a * a) - (b * b)))   
    else
        (if (a > c) then
            iszero ((a * a) - (b * b) - (c * c))
        else
            iszero ((c * c) - (a * a) - (b * b)))
)))
in (((check read) read) read)