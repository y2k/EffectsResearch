module rec MailBoxTest

let curry f a b = f (a, b)
let uncurry f (a, b) = f a b

type Requests =
    | UserReq of int * AsyncReplyChannel<User>
    | CityReq of string * AsyncReplyChannel<City>
    | UserOlderReq of age: int * AsyncReplyChannel<User list>
    | AllUsers of AsyncReplyChannel<User list>

type User =
    { id: int
      name: string
      city: City }

type City =
    { name: string
      count: string }

let request f = mail.PostAndAsyncReply(fun r -> f r)
let request' f x = mail.PostAndAsyncReply(fun r -> f (x, r))
let request'' f = mail.PostAndAsyncReply(fun r -> f r)

let mail: MailboxProcessor<Requests> =
    MailboxProcessor.Start(fun m -> 
        async { 
            match! m.Receive() with
            | UserReq(a, c) -> ()
            | CityReq(a, c) -> ()
            | UserOlderReq _ -> ()
            | AllUsers _ -> ()
        })

let test() =
    async { 
        let! x = mail.PostAndAsyncReply(fun r -> UserReq(42, r))
        let! x = mail.PostAndAsyncReply(fun r -> CityReq("42", r))
        let! x = request (fun c -> UserOlderReq(18, c))
        let f = curry UserOlderReq 18
        let! x = request <| curry UserOlderReq 18
        let! x = request' UserOlderReq 18
        let! x = request'' AllUsers
        ()
    }
