namespace NinetyNineProblems

module BinaryTrees =

    type 'a BinaryTree =
        | Empty
        | Node of 'a * 'a BinaryTree * 'a BinaryTree

    let foldSquared (folder: 'state -> 'a -> 'b -> 'state) (state: 'state) (list1: 'a list) (list2: 'b list) =
        let aux acc1 x1 =
            List.fold (fun acc2 x2 -> folder acc2 x1 x2) acc1 list2

        List.fold aux state list1

    let rec cbalTree n =
        let aux acc l r = Node('x', l, r) :: acc

        if n = 0 then
            [ Empty ]
        elif n = 1 then
            [ Node('x', Empty, Empty) ]

        elif n % 2 = 1 then
            // Both sides have same amount of nodes
            // One gets "consumed" by the current node itself
            let subtree = cbalTree ((n - 1) / 2)
            foldSquared aux [] subtree subtree

        else
            // Sides have different amount of nodes
            let subtree1 = cbalTree (n / 2)
            let subtree2 = cbalTree (n / 2 - 1)

            foldSquared aux (foldSquared aux [] subtree1 subtree2) subtree2 subtree1

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
