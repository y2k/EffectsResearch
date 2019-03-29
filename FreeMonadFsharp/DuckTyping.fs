module DuckTyping

type AsyncMonad() =
    member __.F(i, f) = async { let! x = i
                                return f x }

type OptionMonad() =
    member __.F(i, f) = 
        Option.map f i

let inline fmap f (x: ^T) (M: ^A) = 
    (^A: (member F: ^T -> (^R -> ^Z) -> ^Y) (M, x, f))

let foo() =
    let m = AsyncMonad()
    let a = async.Return 0
    let f (x: int) = string x
    let b = fmap f a m

    let om = OptionMonad()
    let oi = Some 0
    let ob = fmap f oi om

    ()
