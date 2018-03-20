let add = proc (x) (proc (y) (x + y)) in
let n1 = read in
let n2 = read in
let r1 = ((add n1) 4) in
let r2 = ((add n2) n1) in
let r3 = ((add r1) r2) in
    ((add r3) 7)
