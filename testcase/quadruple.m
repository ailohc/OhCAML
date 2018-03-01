let double = proc (x) (x + x) in
let quadruple = proc (x) (2 * (double x)) in
    (quadruple read)