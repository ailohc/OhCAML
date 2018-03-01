let add = proc (x) (proc (y) (x + y)) in
    ((add read) read)