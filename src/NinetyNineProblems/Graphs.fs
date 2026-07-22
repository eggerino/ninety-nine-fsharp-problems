namespace NinetyNineProblems

module Graphs =

    type 'a GraphTerm when 'a: comparison = { nodes: 'a Set; edges: ('a * 'a) Set }

    let graphTermOfHuman (str: string) =
        let rec advance ptr =
            if ptr >= str.Length || str[ptr] = '-' || str[ptr] = ' ' then
                ptr
            else
                advance (ptr + 1)

        let rec parse nodes edges prev ptr =
            if ptr >= str.Length then
                nodes, edges, prev, ptr
            else
                let nextPtr = advance ptr
                let node = str.Substring(ptr, nextPtr - ptr)

                let ptr = nextPtr
                let nodes = Set.add node nodes

                let edges =
                    match prev with
                    | None -> edges
                    | Some p ->
                        let edge = if p < node then p, node else node, p
                        Set.add edge edges

                let prev =
                    if ptr >= str.Length || str[ptr] = ' ' then
                        None
                    else
                        Some node

                parse nodes edges prev (ptr + 1)

        let nodes, edges, _, _ = parse Set.empty Set.empty None 0

        { nodes = nodes; edges = edges }

    let filterMap f list =
        list |> Seq.map f |> Seq.filter Option.isSome |> Seq.map Option.get

    let rec contains x =
        function
        | [] -> false
        | h :: _ when h = x -> true
        | _ :: t -> contains x t

    let neighbors g a =
        g.edges
        |> filterMap (fun (x, y) ->
            if x = a then Some y
            elif y = a then Some x
            else None)

    let rec pathsList g a =
        function
        | [] -> raise (System.Exception "Not reachable")
        | h :: _ as p ->
            if h = a then
                [ p ]
            else
                neighbors g h
                |> Seq.filter (fun n -> not (contains n p))
                |> Seq.map (fun n -> pathsList g a (n :: p))
                |> Seq.concat
                |> Seq.toList

    let paths g a b = pathsList g a [ b ]

    let cycles g a =
        neighbors g a
        |> Seq.map (fun n -> pathsList g a [ n ])
        |> Seq.concat
        |> Seq.map (fun x -> x @ [ a ])
        |> Seq.toList
