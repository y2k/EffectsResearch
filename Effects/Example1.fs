module Example1 

let (^) f x = f x

module Foo' =
    type RawMessages = RawMessages
    type Model = Model
    type TLUser = TLUser
    type 't IEffect =
        abstract member invoke: Async<'t>
    
    type 't ResultEffect =
        { result : 't }
        interface 't IEffect with 
            member this.invoke : Async<'t> = this.result |> async.Return
    type 't MappedEffect =
        { origin : obj
          f : 't Async }
        interface 't IEffect with
            member this.invoke : Async<'t> = this.f
    module Effect =
        let lift x = { result = x } :> IEffect<_>
        let bind (f : 'a -> _ :> 'b IEffect) (origin : 'a IEffect) : 'b IEffect =
            let e =
                { MappedEffect.origin = origin
                  f = async {
                      let! a = origin.invoke
                      let b = f a
                      return! b.invoke
                  } 
                }
            e :> 'b IEffect
        let map f x = bind (f >> lift) x

    type ResolveName = 
        { name : string }
        interface TLUser IEffect with 
            member __.invoke : Async<TLUser> = failwith "???"
    type LoadHistory = 
        { user : TLUser; limit : int }
        interface RawMessages IEffect with 
            member __.invoke : Async<RawMessages> = failwith "???"

    let rawMessagesToResult (_ : RawMessages) : string = failwith "???"
    let tryFindUserName _ : Result<_,_> = failwith "???"

    let onChatHistoryRequested chat =
        match tryFindUserName chat with
        | Error e -> Effect.lift e
        | Ok chat ->
            { ResolveName.name = chat } 
            |> Effect.bind ^ fun user -> { LoadHistory.user = user; limit = 50 }
            |> Effect.map rawMessagesToResult

    let onChatHistoryRequested'' chat : IEffect<string> =
        let onChatResolved (user : TLUser) : IEffect<string> = 
            let ofHistoryLoaded (message : RawMessages) : IEffect<string> = 
                Effect.lift (string message)
        
            { LoadHistory.user = user; limit = 50 }
            |> Effect.bind ofHistoryLoaded
    
        { ResolveName.name = chat } 
        |> Effect.bind onChatResolved

module Foo =
    type Model = Model
    type TLUser = TLUser
    type IEffect =
        abstract member invoke: Async<unit>
    
    type ResolveName = 
        { user : string; next : TLUser -> Model -> Model * IEffect }
        interface IEffect with 
            member __.invoke : Async<unit> = failwith "???"
    type LoadHistory = 
        { user : TLUser; next : unit list -> Model -> Model * IEffect }
        interface IEffect with 
            member __.invoke : Async<unit> = failwith "???"

    let doSomethingWithModel3 x = x
    let onChatResolved (user : TLUser) (model : Model) : Model * IEffect = 
        failwith "???"
    let onChatHistoryRequested chat model : Model * IEffect =
        doSomethingWithModel3 model, 
        { user = chat; ResolveName.next = onChatResolved } :> IEffect
