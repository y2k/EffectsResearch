module SimpleFree2

open System

type User =
    { name: string
      age: int
      id: int }

type City =
    { name: string
      id: int }

type Replay<'x> = 'x -> unit

type ApiRequest =
    | UserById of int * Replay<User>
    | CityByName of string * Replay<City>

type FreeInstruction<'a> =
    | JsonRequest of Uri * (string -> 'a)
    | ApiRequest of (Replay<obj> -> ApiRequest) * (obj -> 'a)

type FaceProgram<'a> =
    | Free of FreeInstruction<FaceProgram<'a>>
    | Pure of 'a

let jsonRequest r = Free(JsonRequest(r, Pure))

let apiRequest (f: Replay<'x> -> ApiRequest): FaceProgram<'x> =
    let r = fun (replayObj: Replay<obj>) -> f (fun x -> replayObj x)
    let a = ApiRequest(r, fun x -> x :?> 'x |> Pure)
    Free(a)

let rec bind f =
    let mapI f =
        function 
        | JsonRequest(x, next) -> JsonRequest(x, next >> f)
        | ApiRequest(x, next) -> ApiRequest(x, next >> f)
    function 
    | Free x -> Free(mapI (bind f) x)
    | Pure x -> f x

type FreeBuilder() =
    member __.Bind(x, f) = bind f x
    member __.Return x = Pure x
    member __.ReturnFrom x = x
    member __.Zero() = Pure()

let free = FreeBuilder()

module WepApi =
    let handle (f: Replay<'x> -> ApiRequest): Async<'x> =
        let mutable result: 'x option = None
        let a = f (fun x -> result <- Some x)
        match a with
        | UserById(id, g) -> 
            g { name = "Oleg"
                age = 18
                id = id }
        | CityByName(name, g) -> 
            g { name = name
                id = 0 }
        result.Value |> async.Return

let rec run x: Async<'a> =
    async { 
        match x with
        | Pure x -> return x
        | Free(JsonRequest(uri, next)) -> 
            do! Async.Sleep 100
            let r = uri.Host
            return! (next >> run) r
        | Free(ApiRequest(r, next)) -> 
            do! Async.Sleep 100
            let! result = WepApi.handle r
            return! (next >> run) result
    }

let example = free { let! json = jsonRequest <| Uri "http://goo.gl/"
                     let! user = apiRequest (fun x -> UserById(0, x))
                     let! city = apiRequest (fun x -> CityByName("Moscow", x))
                     return sprintf "%O|%O|%O" user.name (json.ToUpper()) city }
let test() = async { let! result = run example
                     printfn "RESULT :: %O" result } |> Async.RunSynchronously
