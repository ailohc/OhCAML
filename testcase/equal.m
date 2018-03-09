let equal = proc (x) (proc (y) (if iszero (x - y) then true else false)) in
    ((equal read) read)