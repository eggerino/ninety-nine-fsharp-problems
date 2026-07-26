namespace NinetyNineProblems.Test

module Asserter =

    open Xunit

    type 'a Equal = 'a -> 'a -> unit

    let isTrue actual = Assert.True actual

    let isFalse actual = Assert.False actual

    let boolEqual (expected: bool) actual = Assert.Equal(expected, actual)

    let intEqual (expected: int) actual = Assert.Equal(expected, actual)

    let charEqual (expected: char) actual = Assert.Equal(expected, actual)

    let strEqual (expected: string) actual = Assert.Equal(expected, actual)

    let optionEqual (equal: 'a Equal) expected actual =
        match expected, actual with
        | None, None -> ()
        | Some e, Some a -> equal e a
        | _, _ -> Assert.Fail "Different option state"

    let tupleEqual (fstEqual: 'a Equal) (sndEqual: 'b Equal) expected actual =
        let e1, e2 = expected
        let a1, a2 = actual

        fstEqual e1 a1
        sndEqual e2 a2

    let tupleEqual3 (fstEqual: 'a Equal) (sndEqual: 'b Equal) (trdEqual: 'c Equal) expected actual =
        let e1, e2, e3 = expected
        let a1, a2, a3 = actual

        fstEqual e1 a1
        sndEqual e2 a2
        trdEqual e3 a3

    let rec listEqual (equal: 'a Equal) expected actual =
        match expected, actual with
        | [], [] -> ()
        | eHead :: eTail, aHead :: aTail ->
            equal eHead aHead
            listEqual equal eTail aTail
        | _, _ -> Assert.Fail "Different lengths"

    let arrayEqual (equal: 'a Equal) expected actual =
        intEqual (Array.length expected) (Array.length actual)
        Seq.zip expected actual |> Seq.fold (fun _ (e, a) -> equal e a) ()

    let setEqual expected actual =
        intEqual (Set.count expected) (Set.count actual)
        Set.fold (fun _ a -> isTrue (Set.contains a expected)) () actual
