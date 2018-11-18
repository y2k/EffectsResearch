module SimpleFreeAndHttp

// open System
// open System.Net
// open System.Net.Http

// type City = { name : string; country : string }
// type User = { id : int; name : string; city : City; age: int }

// type Requests =
//     | UserReq of int * (City list -> unit)
//     | CityReq of string * (User list -> unit)

// type FaceInstruction<'a, 'b> = 
//     | RequestInst of (obj -> Requests) * (obj -> 'a)

// type FaceProgram<'a, 'b> =
//     | Free of FaceInstruction<FaceProgram<'a, 'b>, 'b>
//     | Pure of 'a

// let private mapI f = function 
//     | RequestInst(x, next) -> RequestInst(x, next >> f)

// let rec bind f =
//     function 
//     | Free x -> Free(mapI (bind f) x)
//     | Pure x -> f x

// type FaceBuilder() =
//     member __.Bind(x, f) = bind f x
//     member __.Return x = Pure x
//     member __.ReturnFrom x = x
//     member __.Zero() = Pure()
// let face = FaceBuilder()

// let rec interpret x : Async<'a> =
//     async {
//         match x with
//         | Pure x -> return async.Return x
//         | Free(RequestInst(x, next)) -> 
//             return 
//                 x
//                 |> (fun _ -> failwith "TODO")
//                 |> next
//                 |> interpret
//     }

// let foo () =
//     face {
        
//         let a = fun x -> CityReq ("moscow", x)

//         let c = fun _ -> ()

//         // let b = AsyncReplyChannel<User list>(fun _ -> ())
//         // let c = fun (x : AsyncReplyChannel<obj>) -> a (x :?> AsyncReplyChannel<User list>)
//         let d = Free <| RequestInst (c, Pure)

//         ()
//     }