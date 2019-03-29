module SimpleFree

open System

type User =
    { name: string
      age: int
      id: int }

type City =
    { name: string
      id: int }

type ApiRequest =
    | UserById of int * User option
    | CityByName of string * City option

type FreeInstruction<'a> =
    | JsonRequest of Uri * (string -> 'a)
    | ApiRequest of ApiRequest * (obj -> 'a)

type FaceProgram<'a> =
    | Free of FreeInstruction<FaceProgram<'a>>
    | Pure of 'a

let jsonRequest r = Free(JsonRequest(r, Pure))

let apiRequest (f: 'x option -> ApiRequest): FaceProgram<'x> =
    let r = f None
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
    type ApiRequest' =
        | UserById' of int * (User -> unit)
        | CityByName' of string * (City -> unit)
    
    let handle'' (f: ('x -> unit) -> ApiRequest'): 'x =
        let mutable result: 'x option = None
        let a = f (fun x -> result <- Some x)
        match a with
        | UserById'(id, g) -> 
            g { name = "Oleg"
                age = 18
                id = id }
        | CityByName'(name, g) -> 
            g { name = name
                id = 0 }
        result.Value
    
    let handle' (f: 'x option -> ApiRequest): 'x =
        match f None with
        | UserById(id, _) -> 
            box { name = "Oleg"
                  age = 18
                  id = id } :?> 'x
        | CityByName(name, _) -> 
            box { name = name
                  id = 0 } :?> 'x
    
    let private test() =
        let x3 = handle'' (fun x -> UserById'(0, x))
        let x4 = handle'' (fun x -> CityByName'("", x))
        let x1 = handle' (fun x -> UserById(0, x))
        let x2 = handle' (fun x -> CityByName("", x))
        ()
    
    let handle =
        function 
        | UserById(id, _) -> 
            async.Return({ name = "Oleg"
                           age = 18
                           id = id } :> obj)
        | CityByName(name, _) -> 
            async.Return({ name = name
                           id = 0 } :> obj)

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
