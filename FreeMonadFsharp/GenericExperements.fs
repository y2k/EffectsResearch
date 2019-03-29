module GenericExperements

open System
open System.Net
open System.Net.Http

module M = MailBoxTest

let fromJson<'t> (_ : string) : 't = failwith "TODO"

type City = { name : string; country : string }
type User = { id : int; name : string; city : City; age: int }

type Request =
    | UserReq of int
    | CityReq of string

type FaceInstruction<'a, 'b> = 
    // | RequestInst of Request * (obj -> 'a)
    | Request2 of (AsyncReplyChannel<'b> -> M.Requests) * ('b -> 'a)

type FaceProgram<'a, 'b> =
    | Free of FaceInstruction<FaceProgram<'a, 'b>, 'b>
    | Pure of 'a

// let httpClientCall r = Free(RequestInst(r, Pure))
let private mapI f = function 
    // | RequestInst(x, next) -> RequestInst(x, next >> f)
    | Request2(x, next) -> Request2(x, next >> f)

let rec bind f =
    function 
    | Free x -> Free(mapI (bind f) x)
    | Pure x -> f x

type FaceBuilder() =
    member __.Bind(x, f) = bind f x
    member __.Return x = Pure x
    member __.ReturnFrom x = x
    member __.Zero() = Pure()
let face = FaceBuilder()

let id' x = x
let foo' (f : AsyncReplyChannel<'a> -> M.Requests) : 'b = failwith "TODO"

// let foo' () =
//     let x : string = foo' (fun x -> M.CityReq ("", x))
//     let y : int = foo' (fun x -> M.Requests.AllUsers x)
//     ()

let request (x : AsyncReplyChannel<'b> -> M.Requests) : FaceProgram<'a, 'b> = 
    // let a = Request2 (x, Pure)
    failwith "TODO"

// let inline request'' (x : AsyncReplyChannel<^a> -> M.Requests) : FaceProgram<'a, 'b> = 
//     let a = Request2 (x, Pure)
//     failwith "TODO"

let bar _ =
    face {

        // let x = Free(Request2( (fun c -> M.CityReq ("Moscow", c)) , Pure))
        // let! y = x
        // let x = Free(Request2( (fun c -> M.AllUsers c) , Pure))
        // let! y = x

        let! (x : M.City list) = request (fun c -> M.CityReq ("Moscow", c))
        // let! (x : M.User list) = request (fun c -> M.AllUsers c)
        // let! (x : M.User list) = request M.AllUsers

        // let! x = request' (fun c -> M.UserOlderReq (18, c))

        ()
    }

// let bar<'a> () (i : M.Requests) : FaceProgram<FaceInstruction<'a>> =
//     Pure (failwith "TODO")

    // let x : FaceInstruction<FaceProgram<'a>> = Request2 (M.AllUsers, Free)
    // let x : FaceProgram<FaceInstruction<'a>> = Free x
    // failwith "TODO"

// let foo () : FaceProgram<int, ()> =
//     let r = UserReq 42
//     let fi = Free <| RequestInst (r, Pure)
//     let i = bind (fun (x : obj) -> (x :?> User).age |> Pure) fi
//     i
