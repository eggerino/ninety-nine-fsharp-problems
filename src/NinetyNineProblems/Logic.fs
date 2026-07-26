namespace NinetyNineProblems

// Problems 40 - 43

module Logic =

    type BoolExpr =
        | Var of string
        | Not of BoolExpr
        | And of BoolExpr * BoolExpr
        | Or of BoolExpr * BoolExpr

    let eval bindings expr =
        let rec resolveVar var =
            function
            | [] -> raise (System.Exception $"Unbound variable {var} in expression")
            | (name, value) :: _ when name = var -> value
            | _ :: t -> resolveVar var t

        let rec aux =
            function
            | Var name -> resolveVar name bindings
            | Not expr -> not (aux expr)
            | And(left, right) -> aux left && aux right
            | Or(left, right) -> aux left || aux right

        aux expr

    let table2 a b expr =
        let aux aval bval =
            let bindings = [ a, aval; b, bval ]
            aval, bval, eval bindings expr

        [ aux true true; aux true false; aux false true; aux false false ]

    let table vars expr =
        let rec genBindings =
            function
            | [] -> [ [] ]
            | h :: t ->
                let prev = genBindings t

                List.map (fun x -> (h, true) :: x) prev
                @ List.map (fun x -> (h, false) :: x) prev

        List.map (fun b -> b, eval b expr) (genBindings vars)

    let rec gray =
        function
        | n when n <= 0 -> []
        | 1 -> [ "0"; "1" ]
        | n ->
            let prev = gray (n - 1)
            List.map (fun x -> "0" + x) prev @ List.map (fun x -> "1" + x) prev

    let sort key list =
        let rec insert item =
            function
            | [] -> [ item ]
            | h :: _ as l when key item < key h -> item :: l
            | h :: t -> h :: insert item t

        let rec aux acc =
            function
            | [] -> acc
            | h :: t -> aux (insert h acc) t

        aux [] list

    type 'a Tree =
        | Leaf of 'a
        | Node of 'a Tree * 'a Tree


    let huffman fs =
        let sorted = List.map (fun x -> Leaf(fst x)) (sort snd fs)

        let rec buildTree =
            function
            | [] -> None
            | [ x ] -> Some x
            | x :: y :: rest -> buildTree (Node(x, y) :: rest)

        match buildTree sorted with
        | None -> []
        | Some(Leaf x) -> [ (x, "0") ]
        | Some(Node _ as n) ->
            let rec aux code =
                function
                | Leaf x -> [ (x, code) ]
                | Node(left, right) -> aux (code + "1") left @ aux (code + "0") right

            aux "" n
