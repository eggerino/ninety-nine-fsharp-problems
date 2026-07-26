namespace NinetyNineProblems

// Problem 61 - 65

module MultiwayTrees =

    type 'a MultTree = T of 'a * 'a MultTree list

    let rec stringOfTree (T(x, cs)) = $"{x}{stringOfTreeChildren cs}^"

    and stringOfTreeChildren =
        function
        | [] -> ""
        | h :: t -> stringOfTree h + stringOfTreeChildren t

    let treeOfString str =
        let rec parse =
            function
            | [] -> failwith "No more characters but a node was expected"
            | value :: t ->
                let children, rest = parseChildren t
                T(value, children), rest

        and parseChildren =
            function
            | [] -> failwith "No more characters but a node or a '^' was expected"
            | '^' :: t -> [], t
            | lst ->
                let value, rest = parse lst
                let other, rest = parseChildren rest
                value :: other, rest

        str |> Seq.toList |> parse |> fst

    let rec countNodes (T(_, children)) = 1 + countNodesChildren children

    and countNodesChildren =
        function
        | [] -> 0
        | h :: t -> countNodes h + countNodesChildren t

    let ipl tree =
        let rec aux depth (T(_, children)) =
            depth + auxChildren (depth + 1) children

        and auxChildren depth =
            function
            | [] -> 0
            | h :: t -> aux depth h + auxChildren depth t

        aux 0 tree

    let bottomUp t =
        let rec aux (T(value, children)) acc =
            List.foldBack aux children (value :: acc)

        aux t []

    let lispy t =
        let rec spaceConcat =
            function
            | [] -> ""
            | [ x ] -> $"{x}"
            | h :: t -> $"{h} " + spaceConcat t

        let rec aux =
            function
            | T(value, []) -> $"{value}"
            | T(value, children) -> $"({value} {auxChildren children})"

        and auxChildren =
            function
            | [] -> ""
            | [ x ] -> aux x
            | h :: t -> aux h + " " + auxChildren t

        aux t
