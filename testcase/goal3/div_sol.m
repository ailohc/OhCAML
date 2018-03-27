let div = proc (x) (proc (y) (x / y)) in
    ((div read) read)