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

    module M =
        type 'a T when 'a: comparison = Map<'a, 'a list>

        let ofAdjacency<'a when 'a: comparison> (x: ('a * 'a list) list) : 'a T =
            let addEdge a b graph =
                let addSingle src dest graph =
                    Map.change src (Option.map (Set.add dest)) graph

                graph |> addSingle a b |> addSingle b a

            let withoutEdges = x |> Seq.map (fun (node, _) -> node, Set.empty) |> Map.ofSeq

            let edges =
                x
                |> Seq.map (fun (node, links) -> Seq.map (fun link -> node, link) links)
                |> Seq.concat

            let setGraph =
                edges |> Seq.fold (fun acc (src, dest) -> addEdge src dest acc) withoutEdges

            let graph = setGraph |> Map.map (fun _ links -> Set.toList links)

            graph

        type ('a, 'b) State when 'a: comparison = { seen: 'a Set; acc: 'b }

        let dfsFold graph node folder init =
            let initialState = { seen = Set.empty; acc = init }

            let visitNode state node =
                let seen = Set.add node state.seen
                let acc = folder state.acc node
                { seen = seen; acc = acc }

            let rec aux state node =
                if Set.contains node state.seen then
                    state
                else
                    let newState = visitNode state node
                    let neighbors = Map.find node graph
                    List.fold aux newState neighbors

            (aux initialState node).acc

    let splitConnected graph =
        let setRemoveMany xs set =
            Seq.fold (fun acc x -> Set.remove x acc) set xs

        let listAccFolder acc x = x :: acc

        let rec findConnectedNodes acc nodeSet =
            if Set.isEmpty nodeSet then
                acc

            else
                let node = Seq.head nodeSet
                let subgraphNodes = M.dfsFold graph node listAccFolder []
                let reducedNodeSet = setRemoveMany subgraphNodes nodeSet

                findConnectedNodes (subgraphNodes :: acc) reducedNodeSet

        let graphOfNodes nodes =
            nodes |> Seq.map (fun node -> node, Map.find node graph) |> Map.ofSeq


        let nodeSet = Set.ofSeq graph.Keys
        let connectedNodes = findConnectedNodes [] nodeSet

        List.map graphOfNodes connectedNodes

    let isBipartite graph =
        let checkNeighbors set1 set2 =
            all (fun node -> all (fun nb -> Set.contains nb set2) (Map.find node graph)) set1

        let checkConnected graph =
            if Map.isEmpty graph then
                true
            else
                let folder (set1, set2) node = set2, Set.add node set1

                let node = Seq.head graph.Keys
                let set1, set2 = M.dfsFold graph node folder (Set.empty, Set.empty)

                checkNeighbors set1 set2 && checkNeighbors set2 set1

        all checkConnected (splitConnected graph)

    let regulars k n =
        // Internal graph datastructure is Map<int, int list>
        let addLinks node links graph =
            let addSingle src dest graph =
                Map.change src (Option.map (fun links -> dest :: links)) graph

            let addEdge a b graph = addSingle a b (addSingle b a graph)

            Seq.fold (fun acc link -> addEdge node link acc) graph links

        let order graph node = Map.find node graph |> List.length

        let checkOrder limit nodes graph =
            nodes |> Seq.map (order graph) |> all (fun o -> o <= limit)

        let rec generateGraphs acc =
            function
            | [] -> acc
            | node :: others ->
                let addAllNodeLinks g =
                    let nodeOrder = order g node

                    if nodeOrder = k then
                        Seq.ofList [ g ] // Cannot add more links to the node. Only the current graph does not violate order limit of the current node

                    else
                        let numNewLinks = k - nodeOrder
                        let allPossibleLinks = Lists.extract numNewLinks others
                        Seq.map (fun links -> addLinks node links g) allPossibleLinks

                let newAcc =
                    acc |> Seq.map addAllNodeLinks |> Seq.concat |> Seq.filter (checkOrder k others)

                generateGraphs newAcc others

        let toGraph (g: Map<int, int list>) =
            let nodes = Set.ofSeq g.Keys

            let edges =
                g.Keys
                |> Seq.map (fun k -> Seq.map (fun v -> k, v, 'a') (Map.find k g))
                |> Seq.concat
                |> Set.ofSeq

            { nodes = nodes; edges = edges }

        let filterIsomorphics graphs =
            let rec checkMany graph =
                function
                | [] -> true
                | head :: _ when isIsomorphic graph head -> false
                | _ :: tail -> checkMany graph tail

            let rec aux acc =
                function
                | [] -> acc
                | graph :: others when checkMany graph others -> aux (graph :: acc) others
                | _ :: tail -> aux acc tail

            aux [] graphs

        let nodes = [ 1..n ]
        let empty = Map.ofSeq (Seq.map (fun node -> node, []) nodes)

        nodes
        |> generateGraphs [ empty ]
        |> Seq.map toGraph
        |> Seq.toList
        |> filterIsomorphics
