module Effects.ReactNative

let mutable private globalResponse : Option<obj -> unit> = None

let runTest (f : 'eff -> unit) =
    globalResponse <-
        Some(fun eff ->
                let x = eff :?> 'eff
                f x)

let wrap (fx : ('a -> unit) -> 'eff) (a : 'a Async) : 'a Async =
    async {
        if Option.isSome globalResponse
            then
                let testFunc = globalResponse.Value

                let mutable result : obj Option = None

                let mutable effOpt : 'eff Option = None
                effOpt <-
                    fx (fun x -> result <- Some <| box x)
                    |> Some

                let eff = Option.get effOpt
                testFunc eff

                globalResponse <- None

                return (Option.get result) :?> 'a
            else return! a
    }
