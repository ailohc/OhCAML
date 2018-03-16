let check = proc (a) (proc (b) (proc (c) (
    if a > b then
        if a > c then
            a * a - b * b - c * c = 0
        else
            c * c - a * a - b * b = 0
    else
        if b > c then
            b * b - a * a - c * c = 0
        else
            c * c - a * a - b * b = 0
)))