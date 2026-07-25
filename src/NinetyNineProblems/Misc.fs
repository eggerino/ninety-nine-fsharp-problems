namespace NinetyNineProblems

module Misc =

    module Util =

        let all p s =
            Seq.fold (fun acc x -> acc && p x) true s

        let rec optionPermutations =
            function
            | [] -> Seq.ofList [ [] ]
            | [ head ] -> Seq.map (fun x -> [ x ]) head
            | head :: tail ->
                optionPermutations tail
                |> Seq.map (fun perm -> Seq.map (fun x -> x :: perm) head)
                |> Seq.concat

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

    module AritmeticPuzzle =

        let rec evalAll =
            function
            | [] -> raise (System.Exception "unreachable")
            | [ n ] -> seq { n, n.ToString() }
            | n :: tail ->
                let aux (cur, str) =
                    seq {
                        cur + n, $"({str}) + {n}"
                        cur - n, $"({str}) - {n}"
                        cur * n, $"({str}) * {n}"
                        cur / n, $"({str}) / {n}"
                    }

                evalAll tail |> Seq.map aux |> Seq.concat

        let check (a, b) = fst a = fst b

        let splitAtRev list i =
            let l1, l2 = List.splitAt i list
            List.rev l1, List.rev l2

        let combinate (xs, ys) =
            let yList = Seq.toList ys
            Seq.map (fun x -> List.map (fun y -> x, y) yList) xs |> Seq.concat

        let solve list =
            let n = List.length list

            if n < 2 then
                raise (System.Exception "Not solvable with less than 2 elements")
            else
                let (_, left), (_, right) =
                    seq { 1 .. (n - 1) }
                    |> Seq.map (splitAtRev list)
                    |> Seq.map (fun (l, r) -> evalAll l, evalAll r)
                    |> Seq.map combinate
                    |> Seq.concat
                    |> Seq.filter check
                    |> Seq.head

                left + " = " + right

    module EnglishNumberWords =

        let wordOfDigit =
            function
            | 0 -> "zero"
            | 1 -> "one"
            | 2 -> "two"
            | 3 -> "three"
            | 4 -> "four"
            | 5 -> "five"
            | 6 -> "six"
            | 7 -> "seven"
            | 8 -> "eight"
            | 9 -> "nine"
            | _ -> ""

        let rec toDigits n =
            if n = 0 then
                []
            else
                let d = n % 10
                let n = n / 10
                toDigits n @ [ d ]

        let concatWith delimiter strs =
            let rec aux acc =
                function
                | [] -> acc
                | [ str ] -> acc + str
                | str :: tail -> aux (acc + str + delimiter) tail

            aux "" strs

        let fullWords number =
            number |> toDigits |> List.map wordOfDigit |> concatWith "-"

    module SyntaxChecker =

        let letterParser =
            function
            | head :: tail when System.Char.IsLetter head -> Some tail
            | _ -> None

        let digitParser =
            function
            | head :: tail when System.Char.IsDigit head -> Some tail
            | _ -> None

        let charParser c =
            function
            | head :: tail when head = c -> Some tail
            | _ -> None

        let minusParser = charParser '-'

        let identifier str =
            let str = Seq.toList str

            match letterParser str with
            | None -> false
            | Some(str) ->

                let rec aux =
                    function
                    | [] -> true
                    | str ->
                        let str =
                            match minusParser str with
                            | None -> str
                            | Some(x) -> x

                        match letterParser str, digitParser str with
                        | None, None -> false
                        | Some(x), _ -> aux x
                        | _, Some(x) -> aux x

                aux str

    module Sudoku =

        type Board = int array array

        let parseCell =
            function
            | '.' -> 0
            | x -> System.Int32.Parse(x.ToString())

        let parse (str: string) =
            let lines = str.Split System.Environment.NewLine
            lines |> Array.map (fun line -> line |> Seq.map parseCell |> Seq.toArray)

        let row r (board: Board) =
            [ for c in 0..8 do
                  yield board[r][c] ]

        let col c (board: Board) =
            [ for r in 0..8 do
                  yield board[r][c] ]

        let block n (board: Board) =
            let x = n % 3
            let y = n / 3

            [ for dr in 0..2 do
                  for dc in 0..2 do
                      let r = 3 * x + dr
                      let c = 3 * y + dc
                      yield board[r][c] ]

        let checkCells cells =
            let rec aux acc =
                function
                | [] -> true
                | head :: tail when head = 0 -> aux acc tail
                | head :: _ when Set.contains head acc -> false
                | head :: tail -> aux (Set.add head acc) tail

            aux Set.empty cells

        let check f board =
            Util.all (fun i -> checkCells (f i board)) [ 0..8 ]

        let checkRows = check row
        let checkCols = check col
        let checkBlocks = check block

        let checkBoard board =
            checkRows board && checkCols board && checkBlocks board

        let rec solve (board: Board ref) =
            let coords =
                seq {
                    for r in 0..8 do
                        for c in 0..8 do
                            if board.Value[r][c] = 0 then
                                yield r, c
                }
                |> Seq.tryHead

            match coords with
            | None -> true
            | Some(row, col) ->

                let cand =
                    seq {
                        for candidate in 1..9 do
                            board.Value[row][col] <- candidate

                            if checkBoard board.Value then
                                if solve board then
                                    yield candidate

                        board.Value[row][col] <- 0
                    }
                    |> Seq.tryHead

                cand.IsSome

    module Nanograms =

        type Board = bool array array

        let createEmptyBoard nRows nCols =
            Seq.map (fun _ -> Seq.map (fun _ -> false) [ 1..nCols ] |> Seq.toArray) [ 1..nRows ]
            |> Seq.toArray

        let row r (board: Board) = Array.toList board[r]

        let col c (board: Board) =
            [ for r in 0 .. (board.Length - 1) do
                  yield board[r][c] ]

        let countLengths cells =
            let rec aux acc counter =
                function
                | [] ->
                    let final = if counter > 0 then counter :: acc else acc
                    List.rev final
                | head :: tail ->
                    let nextCounter = if head then counter + 1 else 0
                    let nextAcc = if not head && counter > 0 then counter :: acc else acc

                    aux nextAcc nextCounter tail

            aux [] 0 cells

        let enumerate s = Seq.zip s (Seq.initInfinite id)

        let rec listEquals l1 l2 =
            match l1, l2 with
            | [], [] -> true
            | h1 :: t1, h2 :: t2 when h1 = h2 -> listEquals t1 t2
            | _ -> false

        let check (f: int -> Board -> bool list) (board: Board) (def: (int list * int) list) =
            Util.all (fun (ls, i) -> f i board |> countLengths |> listEquals ls) def

        let checkRows = check row
        let checkCols = check col

        let checkBoard board rows cols =
            checkRows board rows && checkCols board cols

        let solve rows cols =
            let nRows = List.length rows
            let nCols = List.length cols
            let cellCount = nRows * nCols

            let mutable board = createEmptyBoard nRows nCols
            let rows = enumerate rows |> Seq.toList
            let cols = enumerate cols |> Seq.toList

            let rec aux idx =
                if idx = cellCount then
                    checkBoard board rows cols
                else
                    let r = idx % nRows
                    let c = idx / nRows
                    let nextIdx = idx + 1

                    board[r][c] <- false

                    if aux nextIdx then
                        true
                    else
                        board[r][c] <- true
                        if aux nextIdx then true else false

            if aux 0 then Some board else None


        let render board =
            let renderCell x = if x then "x" else " "

            let renderRow row =
                row |> Array.map renderCell |> String.concat ""

            board |> Array.map renderRow |> String.concat "\n"

    module CrosswordPuzzle =

        open System

        type Input = string

        type Error =
            | MalformedInput
            | NoSolution

        type Result = Result<string, Error>

        type WordSet = string Set

        type Unsolved = string

        type Solved = string

        type Position = int * int

        type Direction =
            | Horizontal
            | Vertical

        type Length = int

        type WordLocator = Position * Direction * Length

        let readPuzzle (str: Input) : (WordSet * Unsolved) option =
            let sections = str.Split "\n\n" in

            match sections with
            | [| words; unsolved |] -> let ws = words.Split Environment.NewLine |> Set.ofArray in Some(ws, unsolved)
            | _ -> None

        let findWords (str: Unsolved) : WordLocator list =
            let rows = str.Split Environment.NewLine in

            let cols =
                [| for c in 0 .. (rows[0].Length - 1) do
                       yield Array.fold (fun acc (r: string) -> acc + r[c].ToString()) "" rows |] in

            let find (str: string) =
                str.Split " "
                |> Seq.filter (fun x -> x.Length > 1)
                |> Seq.map (fun word -> str.IndexOf word, word.Length) in

            let horizontals =
                rows
                |> Seq.zip (Seq.initInfinite id)
                |> Seq.map (fun (r, row) -> Seq.map (fun (c, l) -> r, c, l) (find row))
                |> Seq.concat
                |> Seq.map (fun (r, c, l) -> (r, c), Horizontal, l)
                |> Seq.toList in

            let verticals =
                cols
                |> Seq.zip (Seq.initInfinite id)
                |> Seq.map (fun (c, row) -> Seq.map (fun (r, l) -> r, c, l) (find row))
                |> Seq.concat
                |> Seq.map (fun (r, c, l) -> (r, c), Vertical, l)
                |> Seq.toList in

            horizontals @ verticals

        let getPossibleWords (ws: WordSet) ((_, _, l): WordLocator) : string list =
            Set.filter (fun w -> String.length w = l) ws |> Seq.toList

        let permutations (def: (WordLocator * string list) list) : (WordLocator * string) list seq =
            let guessPairOptions = List.map (fun (wl, ws) -> List.map (fun w -> wl, w) ws) def in
            Util.optionPermutations guessPairOptions

        let tryRenderSolution (u: Unsolved) (solution: (WordLocator * string) list) : Solved option =
            let mutable grid =
                u.Split Environment.NewLine |> Array.map (fun row -> row.ToCharArray()) in

            let rec tryRenderWord (((r, c), dir, l), (word: string)) =
                if l = 0 then
                    true
                else
                    let char = word[word.Length - l] in

                    let nextPos =
                        match dir with
                        | Horizontal -> r, c + 1
                        | Vertical -> r + 1, c in

                    if grid[r][c] = '.' || grid[r][c] = char then
                        grid[r][c] <- char
                        tryRenderWord ((nextPos, dir, l - 1), word)
                    else
                        false in

            if Util.all tryRenderWord solution then
                grid
                |> Seq.map (fun r -> Seq.fold (fun acc c -> acc + c.ToString()) "" r)
                |> String.concat "\n"
                |> Some
            else
                None

        let isSolution (u: Unsolved) (solution: (WordLocator * string) list) : bool =
            Option.isSome (tryRenderSolution u solution)

        let solve (str: Input) : Result =
            match readPuzzle str with
            | None -> Error MalformedInput
            | Some(words, unsolved) ->
                let wordLocators = findWords unsolved

                let possibleSolutions =
                    wordLocators
                    |> List.map (fun wl -> wl, getPossibleWords words wl)
                    |> permutations

                let firstSolution =
                    possibleSolutions |> Seq.filter (isSolution unsolved) |> Seq.tryHead

                match firstSolution with
                | None -> Error NoSolution
                | Some(solution) -> tryRenderSolution unsolved solution |> Option.get |> Ok

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

    module DiagnonalOfSequenceOfSequences =

        let enumerate s = Seq.zip s (Seq.initInfinite id)

        let tryTail s =
            if Seq.isEmpty s then Seq.empty else Seq.tail s

        let rec skip count s =
            if count = 0 then s else skip (count - 1) (tryTail s)

        let tryAt idx s = s |> skip idx |> Seq.tryHead

        let tryGetMatch (s, idx) =
            match tryAt idx s with
            | Some(x, i) when i = idx -> [ x ]
            | _ -> []

        let diag s =
            s |> Seq.map enumerate |> enumerate |> Seq.map tryGetMatch |> Seq.concat
