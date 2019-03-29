module Effects.Core

let private globalResponse = new System.Threading.ThreadLocal<Option<obj -> unit>>(fun _ -> None)

let runTest (f : 'eff -> unit) =
    globalResponse.Value <-
        Some(fun eff ->
                let x = eff :?> 'eff
                f x)

let wrap (fx : ('a -> unit) -> 'eff) (a : 'a Async) : 'a Async =
    async {
        if Option.isSome globalResponse.Value
            then
                let testFunc = globalResponse.Value.Value

                let mutable result : obj Option = None

                let mutable effOpt : 'eff Option = None
                effOpt <-
                    fx (fun x -> result <- Some <| box x)
                    |> Some

                let eff = Option.get effOpt
                testFunc eff

                globalResponse.Value <- None

                return (Option.get result) :?> 'a
            else return! a
    }
