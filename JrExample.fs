module JrExample

open System
open Types

type Replay<'x> = 'x -> unit

type Html = string

type ApiRequest =
    | TagListRequest of Html * Replay<Tag list>
    | ProfileRequest of Html * Replay<Profile>
    | PostRequest of Html * Replay<Post>
    | PostsRequest of Html * Replay<PostResponse>

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

let requestWithParse siteUrl f =
    Free.free {
        let uri = Uri siteUrl
        let! html = Free(HtmlRequest(uri, Pure))
        let! response = apiRequest (fun x -> f (html, x))
        printfn "Response = %O" response
        return response
    }

let requestWithParse' _ f =
    let fr = f ("", ignore)

    let url =
        match fr with
        | TagListRequest _ -> "TODO"
        | _ -> "TODO"
    requestWithParse url f

let test =
    Free.free {
        let! x1 = requestWithParse "http://joyreactor.cc/user/_y2k" ProfileRequest
        printfn "Profile %O %O" x1.userImage x1.rating
        let! x2 = requestWithParse "http://joyreactor.cc/" TagListRequest
        printfn "Tags %O" x2.Length
        return ()
    }
