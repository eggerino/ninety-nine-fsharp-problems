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
