namespace NinetyNineProblems

module BinaryTrees =

    type 'a BinaryTree =
        | Empty
        | Node of 'a * 'a BinaryTree * 'a BinaryTree

    let accTrees item left right acc = Node(item, left, right) :: acc

    let foldBackSquared (folder: 'a -> 'b -> 'state -> 'state) (list1: 'a list) (list2: 'b list) (state: 'state) =
        let aux acc1 x1 =
            List.fold (fun acc2 x2 -> folder x1 x2 acc2) acc1 list2

        List.fold aux state list1

    let rec cbalTree n =
        if n = 0 then
            [ Empty ]
        elif n = 1 then
            [ Node('x', Empty, Empty) ]

        elif n % 2 = 1 then
            // Both sides have same amount of nodes
            // One gets "consumed" by the current node itself
            let subtree = cbalTree ((n - 1) / 2)
            foldBackSquared (accTrees 'x') subtree subtree []

        else
            // Sides have different amount of nodes
            let subtree1 = cbalTree (n / 2)
            let subtree2 = cbalTree (n / 2 - 1)

            []
            |> foldBackSquared (accTrees 'x') subtree1 subtree2
            |> foldBackSquared (accTrees 'x') subtree2 subtree1

    let rec isMirror x y =
        match x, y with
        | Empty, Empty -> true
        | Node(_, xl, xr), Node(_, yl, yr) -> isMirror xl yr && isMirror xr yl
        | _ -> false

    let isSymmetric =
        function
        | Empty -> true
        | Node(_, l, r) -> isMirror l r

    let construct list =
        let rec insert item =
            function
            | Empty -> Node(item, Empty, Empty)
            | Node(cur, left, right) as tree ->
                if item = cur then tree // Duplicate item is only needed once -> item can be discarded
                elif item < cur then Node(cur, insert item left, right)
                else Node(cur, left, insert item right)

        let rec aux acc =
            function
            | [] -> acc
            | h :: t -> aux (insert h acc) t

        aux Empty list

    let symCbalTrees n = cbalTree n |> List.filter isSymmetric

    let rec hbalTree n =
        if n = 0 then
            [ Empty ]
        elif n = 1 then
            [ Node('x', Empty, Empty) ]

        else
            let subtrees1 = hbalTree (n - 1)
            let subtrees2 = hbalTree (n - 2)

            []
            |> foldBackSquared (accTrees 'x') subtrees2 subtrees1
            |> foldBackSquared (accTrees 'x') subtrees1 subtrees2
            |> foldBackSquared (accTrees 'x') subtrees1 subtrees1

    let maxNodes h = pown 2 h - 1

    let rec minNodes h =
        if h <= 0 then 0
        elif h = 1 then 1
        else 1 + minNodes (h - 1) + minNodes (h - 2)

    let log2 x = log x / log 2.0

    let minHeight n = n + 1 |> float |> log2 |> ceil |> int

    let maxHeight n =
        let rec aux h =
            if minNodes h <= n then aux (h + 1) else h - 1

        aux 0

    let rec foldRange f init n0 n1 =
        if n0 > n1 then
            init
        else
            foldRange f (f init n0) (n0 + 1) n1

    let addSwapLeftRight trees =
        List.fold
            (fun a n ->
                match n with
                | Node(v, t1, t2) -> Node(v, t2, t1) :: a
                | Empty -> a)
            trees
            trees

    let rec hbalTreeNodesHeight h n =
        assert (minNodes h <= n && n <= maxNodes h)

        if h = 0 then
            [ Empty ]
        else
            let acc = addHbalTreeNode [] (h - 1) (h - 2) n in
            let acc = addSwapLeftRight acc in
            addHbalTreeNode acc (h - 1) (h - 1) n

    and addHbalTreeNode l h1 h2 n =
        let f l n1 =
            let t1 = hbalTreeNodesHeight h1 n1 in
            let t2 = hbalTreeNodesHeight h2 (n - 1 - n1) in
            List.fold (fun l t1 -> List.fold (fun l t2 -> Node('x', t1, t2) :: l) l t2) l t1

        let min_n1 = max (minNodes h1) (n - 1 - maxNodes h2) in
        let max_n1 = min (maxNodes h1) (n - 1 - minNodes h2) in
        foldRange f l min_n1 max_n1

    let hbalTreeNodes n =
        (*
            The implementation is ported from the solution of the problem author
        *)
        let f l h = List.rev (hbalTreeNodesHeight h n) @ l

        foldRange f [] (minHeight n) (maxHeight n)

    let rec countLeaves =
        function
        | Empty -> 0
        | Node(_, Empty, Empty) -> 1
        | Node(_, l, r) -> countLeaves l + countLeaves r

    let rec leaves =
        function
        | Empty -> []
        | Node(x, Empty, Empty) -> [ x ]
        | Node(_, l, r) -> leaves l @ leaves r

    let rec internals =
        function
        | Empty -> []
        | Node(_, Empty, Empty) -> []
        | Node(x, l, r) -> x :: internals l @ internals r

    let atLevel tree n =
        let rec nextLevel acc =
            function
            | [] -> acc
            | Empty :: t -> nextLevel acc t
            | Node(_, l, r) :: t -> nextLevel (l :: r :: acc) t

        let rec findLevel k level =
            if k = n then
                level
            else
                findLevel (k + 1) (nextLevel [] level)

        let rec extractItems =
            function
            | [] -> []
            | Empty :: t -> extractItems t
            | Node(x, _, _) :: t -> x :: extractItems t

        findLevel 1 [ tree ] |> extractItems

    let completeBinaryTree list =
        let rec prepare cnt acc =
            let nextCnt = cnt + 1

            function
            | [] -> acc
            | h :: t -> prepare nextCnt ((h, nextCnt) :: acc) t

        let list = prepare 0 [] list

        let rec getAt n =
            function
            | [] -> None
            | (item, addr) :: t when addr = n -> Some item
            | _ :: t -> getAt n t

        let rec buildTree addr =
            match getAt addr list with
            | None -> Empty
            | Some x -> Node(x, buildTree (2 * addr), buildTree (2 * addr + 1))

        buildTree 1

    let rec map f =
        function
        | Empty -> Empty
        | Node(x, l, r) -> Node(f x, map f l, map f r)

    let move dx dy =
        map (fun (item, x, y) -> item, x + dx, y + dy)

    let moveX dx = move dx 0

    let rec getAnchor =
        function
        | Empty -> 0
        | Node((_, x, _), Empty, _) -> x
        | Node(_, l, _) -> getAnchor l

    let moveOrigin tree = moveX (1 - getAnchor tree) tree

    let addHeights tree =
        let rec aux h =
            function
            | Empty -> Empty
            | Node(x, l, r) -> Node((x, h), aux (h + 1) l, aux (h + 1) r)

        aux 1 tree

    let rec getHeight =
        function
        | Empty -> 0
        | Node(_, l, r) -> 1 + max (getHeight l) (getHeight r)

    let layoutBinaryTree1 tree =
        let tree = addHeights tree

        let rec aux x =
            function
            | Empty -> x, Empty
            | Node((item, h), l, r) ->
                let x, l = aux x l
                let item = item, x, h
                let x, r = aux (x + 1) r
                x, Node(item, l, r)

        aux 1 tree |> snd

    let layoutBinaryTree2 tree =
        let height = getHeight tree
        let tree = addHeights tree

        let rec buildCentered =
            function
            | Empty -> Empty
            | Node((item, h), l, r) ->
                let dist = pown 2 (height - h - 1)
                let l = buildCentered l |> moveX -dist
                let r = buildCentered r |> moveX dist

                Node((item, 0, h), l, r)

        let centered = buildCentered tree

        moveOrigin centered

    let layoutBinaryTree3 tree =
        let tree = addHeights tree

        let rec pathDistance p1 p2 =
            match p1, p2 with
            | h1 :: t1, h2 :: t2 -> max (h1 - h2) (pathDistance t1 t2)
            | _ -> 0

        let rec mergePaths main side =
            match main, side with
            | [], _ -> side
            | _, [] -> main
            | h :: mt, _ :: st -> h :: mergePaths mt st

        let add a b = a + b
        let movePath dx = List.map (add dx)

        let rec build =
            function
            | Empty -> Empty, [], []
            | Node((item, height), left, right) ->
                let left, leftsLeftPath, leftsRightPath = build left
                let right, rightsLeftPath, rightsRightPath = build right

                let distance = 1 + pathDistance leftsRightPath rightsLeftPath / 2

                let left = moveX -distance left
                let leftsLeftPath = movePath -distance leftsLeftPath
                let leftsRightPath = movePath -distance leftsRightPath

                let right = moveX distance right
                let rightsLeftPath = movePath distance rightsLeftPath
                let rightsRightPath = movePath distance rightsRightPath

                let leftPath = 0 :: mergePaths leftsLeftPath rightsLeftPath
                let rightPath = 0 :: mergePaths rightsRightPath leftsRightPath

                Node((item, 0, height), left, right), leftPath, rightPath

        let centered, _, _ = build tree
        moveOrigin centered

    let rec stringOfTree =
        function
        | Empty -> ""
        | Node(x, Empty, Empty) -> $"{x}"
        | Node(x, l, r) -> $"{x}({stringOfTree l},{stringOfTree r})"

    let treeOfString (str: string) =
        let rec parse ptr =
            if ptr >= str.Length || str[ptr] = ',' || str[ptr] = ')' then
                Empty, ptr
            else
                let value = str[ptr]

                if str[ptr + 1] <> '(' then
                    Node(value, Empty, Empty), ptr + 1
                else
                    let left, ptr = parse (ptr + 2) // Consume value and (
                    let right, ptr = parse (ptr + 1) // Consume ,
                    Node(value, left, right), ptr + 1 // Consume )

        fst (parse 0)

    let rec preorder =
        function
        | Empty -> []
        | Node(x, l, r) -> x :: preorder l @ preorder r

    let rec inorder =
        function
        | Empty -> []
        | Node(x, l, r) -> inorder l @ x :: inorder r

    let splitAtValue value list =
        let rec aux acc =
            function
            | [] -> List.rev acc, []
            | h :: t when h = value -> List.rev acc, t
            | h :: t -> aux (h :: acc) t

        aux [] list

    let preInTree pre in_ =
        let rec aux pre in_ =
            match pre, in_ with
            | _, []
            | [], _ -> Empty, pre
            | item :: preTail, _ ->
                let leftIn, rightIn = splitAtValue item in_
                let left, pre = aux preTail leftIn
                let right, pre = aux pre rightIn
                Node(item, left, right), pre

        fst (aux pre in_)

    let rec dotstringOfTree =
        function
        | Empty -> "."
        | Node(x, l, r) -> $"{x}" + dotstringOfTree l + dotstringOfTree r

    let treeOfDotstring (str: string) =
        let rec parse ptr =
            if ptr >= str.Length then
                Empty, ptr
            else
                match str[ptr] with
                | '.' -> Empty, ptr + 1
                | value ->
                    let left, ptr = parse (ptr + 1)
                    let right, ptr = parse ptr
                    Node(value, left, right), ptr

        fst (parse 0)
