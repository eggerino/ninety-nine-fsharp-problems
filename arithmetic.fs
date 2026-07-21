module arithmetic

let isPrime n =
    let n = abs n

    let rec aux x =
        if x * x > n then true
        elif n % x = 0 then false
        else aux (x + 1)

    n > 1 && aux 2
