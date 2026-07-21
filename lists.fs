module lists

let rec last =
    function
    | [] -> None
    | [ x ] -> Some x
    | _ :: rest -> last rest

let rec lastTwo =
    function
    | [] -> None
    | [ _ ] -> None
    | [ x; y ] -> Some(x, y)
    | _ :: rest -> lastTwo rest

let rec at i =
    function
    | [] -> None
    | x :: rest -> if i = 0 then Some x else at (i - 1) rest

let length list =
    let rec aux acc =
        function
        | [] -> acc
        | _ :: rest -> aux (acc + 1) rest

    aux 0 list

let rev list =
    let rec aux acc =
        function
        | [] -> acc
        | x :: rest -> aux (x :: acc) rest

    aux [] list

let isPalindrome list =
    let rec aux fst snd =
        match fst, snd with
        | [], [] -> true
        | x :: xs, y :: ys when x = y -> aux xs ys
        | _ -> false

    aux (rev list) list

type 'a node =
    | One of 'a
    | Many of 'a node list

let rec flatten =
    function
    | [] -> []
    | One x :: rest -> x :: flatten rest
    | Many xs :: rest -> flatten xs @ flatten rest

let rec compress =
    function
    | x :: (y :: _ as rest) -> if x = y then compress rest else x :: compress rest
    | oneOrEmpty -> oneOrEmpty

let pack list =
    let rec aux cur list =
        match cur, list with
        | [], [] -> []
        | _, [] -> [ cur ]
        | [], x :: rest -> aux [ x ] rest
        | y :: _, x :: rest -> if x = y then aux (x :: cur) rest else cur :: aux [ x ] rest

    aux [] list

let encode list =
    let rec aux cnt acc =
        function
        | [] -> acc
        | [ x ] -> (cnt + 1, x) :: acc
        | x :: (y :: _ as rest) ->
            if x = y then
                aux (cnt + 1) acc rest
            else
                aux 0 ((cnt + 1, x) :: acc) rest

    List.rev (aux 0 [] list)

type 'a rle =
    | One of 'a
    | Many of int * 'a

let encode2 list =
    let toItem cnt item =
        match cnt with
        | 1 -> One item
        | _ -> Many(cnt, item)

    let rec aux cnt acc =
        function
        | [] -> acc
        | [ x ] -> toItem (cnt + 1) x :: acc
        | x :: (y :: _ as rest) ->
            if x = y then
                aux (cnt + 1) acc rest
            else
                aux 0 (toItem (cnt + 1) x :: acc) rest

    List.rev (aux 0 [] list)

let decode list =
    let rec pushMany acc =
        function
        | 1, x -> x :: acc
        | cnt, x -> pushMany (x :: acc) (cnt - 1, x)

    let rec aux acc =
        function
        | [] -> acc
        | One x :: rest -> aux (x :: acc) rest
        | Many(cnt, item) :: rest -> aux (pushMany acc (cnt, item)) rest

    List.rev (aux [] list)

let rec duplicate =
    function
    | [] -> []
    | x :: rest -> x :: x :: duplicate rest

let replicate list n =
    let rec aux cnt acc =
        function
        | [] -> acc
        | x :: rest as list ->
            if cnt = n then
                aux 0 acc rest
            else
                aux (cnt + 1) (x :: acc) list

    List.rev (aux 0 [] list)

let drop list n =
    let rec aux cnt acc =
        function
        | [] -> acc
        | x :: rest ->
            if cnt = n then
                aux 1 acc rest
            else
                aux (cnt + 1) (x :: acc) rest

    List.rev (aux 1 [] list)

let split list n =
    let rec aux cnt acc =
        function
        | [] -> acc, []
        | x :: rest ->
            if cnt = n then
                x :: acc, rest
            else
                aux (cnt + 1) (x :: acc) rest

    let fst, snd = aux 1 [] list
    List.rev fst, snd

let slice list start stop =
    let rec skip n =
        function
        | [] -> []
        | x :: rest as xs -> if n = 0 then xs else skip (n - 1) rest

    let rec take n acc =
        function
        | [] -> acc
        | x :: rest -> if n = 0 then acc else take (n - 1) (x :: acc) rest

    list |> skip start |> take (stop - start + 1) [] |> List.rev

let rotate list n =
    let len = List.length list
    let n = n % len
    let fst, snd = split list n
    snd @ fst

let rec removeAt n =
    function
    | [] -> []
    | x :: rest -> if n = 0 then rest else x :: removeAt (n - 1) rest

let rec insertAt item n =
    function
    | [] -> [ item ]
    | x :: rest as xs ->
        if n = 0 then
            item :: xs
        else
            x :: insertAt item (n - 1) rest

let rec range start stop =
    if start = stop then
        [ start ]
    else
        start :: range (start + 1) stop

let randSelect list n =
    let rng = System.Random.Shared

    let rec extract n acc =
        function
        | [] -> raise (System.Exception "Not reachable")
        | h :: t -> if n = 0 then h, acc @ t else extract (n - 1) (h :: acc) t

    let extractRand list =
        extract (rng.Next() % List.length list) [] list

    let rec aux cnt acc =
        function
        | [] -> acc
        | list ->
            if cnt = n then
                acc
            else
                let item, rest = extractRand list
                aux (cnt + 1) (item :: acc) rest

    aux 0 [] list

let lottoSelect n m = randSelect (range 1 m) n

let permutation list = randSelect list (List.length list)

let rec extract k list =
    if k <= 0 then
        [ [] ]
    else
        match list with
        | [] -> []
        | x :: rest -> (extract (k - 1) rest |> List.map (fun r -> x :: r)) @ extract k rest

let rec insert key item =
    function
    | [] -> [ item ]
    | x :: rest as xs ->
        if key item < key x then
            item :: xs
        else
            x :: insert key item rest

let sort list =
    let rec aux acc =
        function
        | [] -> List.map (fun (_, value) -> value) acc
        | x :: rest -> aux (insert (fun (key, _) -> key) x acc) rest

    aux [] list

let lengthSort lists =
    lists |> List.map (fun l -> List.length l, l) |> sort

let frequencySort lists =
    let lengthPairs = lists |> List.map (fun l -> List.length l, l)

    let rec aux acc =
        function
        | [] -> acc
        | (l, _) :: rest ->
            aux
                (Map.change
                    l
                    (fun x ->
                        match x with
                        | Some value -> Some(value + 1)
                        | None -> Some 1)
                    acc)
                rest

    let freqMap = aux Map.empty lengthPairs

    let getFreq l =
        match Map.tryFind l freqMap with
        | Some x -> x
        | None -> 0

    let freqPairs = lengthPairs |> List.map (fun (l, value) -> getFreq l, value)

    sort freqPairs
