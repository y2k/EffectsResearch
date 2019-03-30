module Effects.Tests

open Effects.Core

[<AutoOpen>]
module Utils =
    open Effects.Core
    open Fabulous.Core

    let getBlocked f =
        let mutable actual = None
        f (fun newMsg ->
                actual <- Some newMsg)
        Option.get actual

    let runBlocked cmd f =
        Eff.runTest f
        getBlocked (fun f -> Cmd.dispatch f cmd)

open Effects.Core
open Effects.EffectsTestPage
open Fabulous.Core
open System
open Xunit

[<Fact(Timeout = 1000)>]
let ``Test page init``() =
    let (model, cmd) = Page.init()
    let actual = runBlocked
                    cmd
                    (fun (Effects.DownloadString(arg, f)) ->
                        Assert.Equal(Uri "https://google.com/#", arg)
                        f "[{\"Code\":\"Code\"}]")

    Assert.Equal(Page.initModel, model)
    Assert.Equal(Page.Msg.CountriesLoaded <| Ok [| { Code = "Code" } |], actual)
