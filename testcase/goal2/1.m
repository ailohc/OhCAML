let pita = proc (n1) (proc (n2) (
    proc (n3) (
        if n1 > n2 then (
            if n1 < n3 then (
                    if n3 * n3 = n2 * n2 + n1 * n1 then true else false 
                )
            else (
                    if n1 * n1 = n2 * n2 + n3 * n3 then true else false
                )
            )
        else (
            if n2 > n3 then (
                if n2 < n1 then (
                    if n1 * n1 = n2 * n2 + n3 * n3 then true else false
                ) 
                else (
                    if n2 * n2 = n1 * n1 + n3 * n3 then true else false
                )
            )
            else (
                if n3 < n1 then (
                    if n1 * n1 = n2 * n2 + n3 * n3 then true else false
                )
                else (
                    if n3 * n3 = n1 * n1 + n2 * n2 then true else false
                )
            )
        )
        )
    )) in
    (((pita read) read) read)