module JrExample

open System
open Types

type Replay<'x> = 'x -> unit

type ApiRequest =
    | TagListRequest of string * (Tag list -> unit)
    | ProfileRequest of string * (Profile -> unit)
    | PostRequest of string * (Post -> unit)
    | PostsRequest of string * (PostResponse -> unit)

type FreeInstruction<'a> =
    | HtmlRequest of Uri * (string -> 'a)
    | ApiRequest of (Replay<obj> -> ApiRequest) * (obj -> 'a)

type FaceProgram<'a> =
    | Free of FreeInstruction<FaceProgram<'a>>
    | Pure of 'a

module Free =
    let rec bind f =
        let mapI f =
            function 
            | HtmlRequest(x, next) -> HtmlRequest(x, next >> f)
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

let apiRequest (f: Replay<'x> -> ApiRequest): FaceProgram<'x> = failwith "TODO"

let common url f =
    Free.free { 
        let uri = Uri url
        let! html = Free(HtmlRequest(uri, Pure))
        let! profile = apiRequest (fun x -> f (html, x))
        printfn "Profile = %O" profile
        return profile
    }

let test =
    Free.free { 
        let uri = Uri "http://joyreactor.cc/user/_y2k"
        let! html = Free(HtmlRequest(uri, Pure))
        let! profile = apiRequest (fun x -> ProfileRequest(html, x))
        printfn "Profile = %O" profile
        return profile
    }
let test' =
    Free.free { 
        let uri = Uri "http://joyreactor.cc/"
        let! html = Free(HtmlRequest(uri, Pure))
        let! tags = apiRequest (fun x -> TagListRequest(html, x))
        printfn "Tags = %O" tags
        return tags
    }
let test'' =
    Free.free { 
        let! x = common "http://joyreactor.cc/user/_y2k" ProfileRequest
        return x
    }
let test''' =
    Free.free { 
        let! x = common "http://joyreactor.cc/" TagListRequest
        return x
    }
