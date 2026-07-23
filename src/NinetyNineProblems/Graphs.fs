namespace NinetyNineProblems

module Graphs =

    type ('a, 'b) Graph when 'a: comparison and 'b: comparison =
        { nodes: 'a Set
          edges: ('a * 'a * 'b) Set }

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
                        let edge = if p < node then p, node, () else node, p, ()
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

    let rec containsEdge edge list =
        let src, dest = edge

        match list with
        | [] -> false
        | (s, d, _) :: _ when s = src && d = dest -> true
        | _ :: t -> containsEdge edge list

    let removeWeight (src, dest, _) = src, dest

    let neighbors g a =
        g.edges
        |> filterMap (fun (x, y, _) ->
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

    let split vertex visited edges =
        let isConsumed edge =
            let src, dest, _ = edge
            src = vertex && contains dest visited || dest = vertex && contains src visited

        Set.partition isConsumed edges

    let addEdge edge =
        function
        | [] -> [ [ edge ] ]
        | edges -> edges |> List.map (fun edgeList -> edge :: edgeList) |> List.rev

    let sTree graph =
        let rec aux acc visited edges =
            function
            | [] -> acc
            | vertex :: rest ->
                let consumedEdges, remainingEdges = split vertex visited edges

                if Set.isEmpty consumedEdges then
                    []
                else
                    let acc = consumedEdges |> Set.fold (fun a p -> addEdge p acc @ a |> List.rev) []
                    aux acc (vertex :: visited) remainingEdges rest

        let vertices = Seq.toList graph.nodes

        aux [] [ List.head vertices ] graph.edges (List.tail vertices)

    let isTree graph = List.length (sTree graph) > 0

    let isConnected graph = List.length (sTree graph) > 0

    let totalWeight edges =
        List.fold (fun acc (_, _, w) -> acc + w) 0 edges

    let msTree graph =
        match sTree graph with
        | [] -> []
        | trees -> List.maxBy totalWeight trees

    let all predicate seq =
        Seq.fold (fun acc x -> acc && predicate x) true seq

    let any predicate seq =
        Seq.fold (fun acc x -> acc || predicate x) false seq

    let rec setPermutations set =
        if Set.count set = 1 then
            Seq.map (fun x -> [ x ]) set
        else

            let mapping x =
                let withoutX = Set.remove x set
                let subpermutations = setPermutations withoutX
                Seq.map (fun sp -> x :: sp) subpermutations

            set |> Seq.map mapping |> Seq.concat

    let isIsomorphic graph1 graph2 =
        let testGraphSize a b =
            // Check if the number of nodes and edges match
            // Number of nodes need to match for any bijektiv function to exist

            Set.count a.nodes = Set.count b.nodes && Set.count a.edges = Set.count b.edges

        let testMapping a b f =
            // Check if every edge in graph a when mapped with the bijection f is also an edge of graph b

            let containsEdge src dest edges =
                any (fun (s, d, _) -> s = src && d = dest || d = src && s = dest) edges

            all (fun (s, d, _) -> containsEdge (f s) (f d) b.edges) a.edges

        let generateMappings a b =
            // Generate every possbile mappings from the nodes of a to the nodes of b

            let nodesOfA = Seq.toList a.nodes // Have them in a list for the same iteration order
            let permutationsOfNodesOfB = setPermutations b.nodes

            let generateMapping nodes =
                Seq.zip nodesOfA nodes |> Seq.fold (fun acc (x, y) -> Map.add x y acc) Map.empty

            permutationsOfNodesOfB |> Seq.map generateMapping

        let bijection mapping x = Map.find x mapping

        if not (testGraphSize graph1 graph2) then
            false

        else
            let fs = Seq.map bijection (generateMappings graph1 graph2)
            any (testMapping graph1 graph2) fs

    let degree graph node =
        Seq.fold (fun acc (src, dest, _) -> if src = node || dest = node then acc + 1 else acc) 0 graph.edges

    let sortedNodes graph =
        graph.nodes
        |> Seq.map (fun node -> node, degree graph node)
        |> Seq.sortByDescending (fun (_, d) -> d)
        |> Seq.map fst
        |> Seq.toList
