namespace NinetyNineProblems

module Misc =

    module EightQueensProblem =

        let occupiedByRow = Set.ofSeq

        let occupiedByDiag acc queens =
            queens
            |> Seq.zip (Seq.initInfinite (fun idx -> idx + 1))
            |> Seq.fold (fun a (d, r) -> a |> Set.add (r + d) |> Set.add (r - d)) acc

        let occupied queens =
            occupiedByDiag (occupiedByRow queens) queens

        let queensPositions n =
            let rec generate acc =
                function
                | cnt when cnt = n -> acc
                | cnt ->
                    let allPossibles queens =
                        let occ = occupied queens

                        seq { 1..n }
                        |> Seq.filter (fun field -> not (Set.contains field occ))
                        |> Seq.map (fun field -> field :: queens)

                    let newAcc = acc |> Seq.map allPossibles |> Seq.concat
                    generate newAcc (cnt + 1)

            generate [ [] ] 0 |> Seq.toList

    module VonKochsConjecture =

        let permutations list =
            let rec ps list taken =
                seq {
                    if Set.count taken = List.length list then
                        yield []
                    else
                        for l in list do
                            if not (Set.contains l taken) then
                                for perm in ps list (Set.add l taken) do
                                    yield l :: perm
                }

            ps list Set.empty

        let bind nodes labels =
            let map = Seq.foldBack2 Map.add nodes labels Map.empty

            fun node -> Map.find node map

        let edgeLabel f (src, dest) = abs (f src - f dest)

        let checkLabeling edges f =
            let n = Seq.length edges + 1
            let edgeLagels = edges |> Seq.map (edgeLabel f) |> Set.ofSeq
            Graphs.all (fun label -> Set.contains label edgeLagels) [ 1 .. (n - 1) ]

        let toLabeled tree f =
            let nodes, edges = tree

            let labeledNodes = List.map (fun node -> node, f node) nodes

            let labeledEdges =
                List.map (fun (src, dest) -> src, dest, edgeLabel f (src, dest)) edges

            labeledNodes, labeledEdges

        let label tree =
            let nodes, edges = tree
            let n = List.length nodes
            let nodeLabels = [ 1..n ]

            permutations nodeLabels
            |> Seq.map (bind nodes)
            |> Seq.filter (checkLabeling edges)
            |> Seq.head
            |> toLabeled tree

    module KnightsTour =

        let moves pos =
            let r, c = pos

            [ r + 2, c + 1
              r + 1, c + 2
              r + 2, c - 1
              r + 1, c - 2
              r - 2, c - 1
              r - 1, c - 2
              r - 2, c + 1
              r - 1, c + 2 ]

        let isOnBoard n pos =
            let r, c = pos
            1 <= r && r <= n && 1 <= c && c <= n

        let jump n pos =
            let rec aux acc pos =
                if Set.contains pos acc || not (isOnBoard n pos) then
                    acc

                else
                    let newAcc = Set.add pos acc
                    List.fold aux newAcc (moves pos)

            aux Set.empty pos |> Set.toList

    module NeverEndingSequence =

        type 'a Node = Node of 'a * 'a Stream
        and 'a Stream = unit -> 'a Node

        let hd (s: 'a Stream) =
            let (Node(head, _)) = s ()
            head

        let tl (s: 'a Stream) =
            let (Node(_, tail)) = s ()
            tail

        let rec take n (s: 'a Stream) =
            if n = 0 then
                []
            else
                let (Node(h, t)) = s ()
                h :: take (n - 1) t

        let rec unfold generator state () =
            let value, x = generator state
            Node(value, unfold generator x)

        let rec bang x () = Node(x, bang x)

        let rec ints i () = Node(i, ints (i + 1))

        let rec map f (s: 'a Stream) () =
            let (Node(h, t)) = s ()
            Node(f h, map f t)

        let rec filter f (s: 'a Stream) () =
            let (Node(h, t)) = s ()
            if f h then Node(h, filter f t) else filter f t ()

        let rec iter f (s: 'a Stream) =
            let (Node(h, t)) = s ()
            f h
            iter f t
