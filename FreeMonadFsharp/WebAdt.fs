module WebAdt

type User = User
type City = City

type UserRequest = int
type CiyRequest = string
type Request = | UserRequest of UserRequest | CiyRequest of CiyRequest

let doUserRequest (r : UserRequest) : Async<User> = failwith "TODO"
let doCityRequest (r : CiyRequest) : Async<City> = failwith "TODO"

// let doRequest (r : Request) =
//     match r with
//     | UserRequest x -> doUserRequest x
//     | CiyRequest x -> doCityRequest x

// let test =
//     async {

//         let! x = doRequest <| UserRequest 0

//         return ()
//     }
