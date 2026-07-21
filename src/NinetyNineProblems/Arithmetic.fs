namespace NinetyNineProblems

module Arithmetic =

    let isPrime n =
        let n = abs n

        let rec aux x =
            if x * x > n then true
            elif n % x = 0 then false
            else aux (x + 1)

        n > 1 && aux 2

    let rec gcd a b =
        let a, b = if a > b then a, b else b, a

        if b = 0 || a = b then a else gcd (a - b) b

    let coprime a b = gcd a b = 1

    let phi m =
        let rec aux acc =
            function
            | [] -> acc
            | h :: t -> if coprime h m then aux (acc + 1) t else aux acc t

        aux 0 [ 1 .. m - 1 ]

    let factors n =
        let rec aux acc d n =
            if n = 1 then acc
            elif n % d = 0 then aux (d :: acc) 2 (n / d)
            else aux acc (d + 1) n

        List.rev (aux [] 2 n)

    let factors2 n =
        let rec aux d n =
            if n = 1 then
                []
            elif n % d <> 0 then
                aux (d + 1) n
            else
                match aux 2 (n / d) with
                | [] -> [ d, 1 ]
                | (x, c) :: t when x = d -> (x, c + 1) :: t
                | l -> (d, 1) :: l

        aux 2 n

    let phiImproved m =
        let getFactor p m = (p - 1) * pown p (m - 1)

        let rec aux acc =
            function
            | [] -> acc
            | (p, m) :: t -> aux (acc * getFactor p m) t

        aux 1 (factors2 m)

    let timeit f x =
        let sw = System.Diagnostics.Stopwatch.StartNew()
        let res = f x
        sw.Stop()
        sw.Elapsed.TotalMilliseconds, res

    let allPrimes start stop = List.filter isPrime [ start..stop ]

    let goldbach n =
        let rec aux x =
            let other = n - x
            if isPrime x && isPrime other then x, other else aux (x + 1)

        aux 2

    let goldbachList start stop =
        let isEven x = x % 2 = 0
        [ start..stop ] |> List.filter isEven |> List.map (fun x -> (x, goldbach x))

    let isBigGoldbach x =
        let _, (s1, s2) = x
        s1 > 50 && s2 > 50
