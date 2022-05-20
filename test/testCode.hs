import Prelude
import Logic

--main for testing
main = do
        --testing gates and truth tables
        let vars = ["A", "B"]
        let a = VBool "A"
        let b = VBool "B"
        let exp1 = Buffer a
        let exp2 = Not a
        let exp3 = And [a, b]
        let exp4 = Or [a, b]
        let exp5 = Nand [a, b]
        let exp6 = Nor [a, b]
        let exps1 = [exp1, exp2]
        let exps2 = [exp3, exp4, exp5, exp6]
        let tables = map truthTable exps1 ++ map truthTable exps2
        mapM_ print tables

        let c = VBool "C"
        let d = VBool "Delta"
        let e = VBool "Echo"
        let expSimplify = Or [And [Or [a, b], Nor [a, And [b, c, Not e]]], c,d,Not e]
        print expSimplify
        print $ simplify expSimplify
        print $ truthTable expSimplify