module Effects.EffectsTestPage

open Fabulous.Core
open System
open System.Net

module Effect = Effects.Core

[<AutoOpen>]
module Utils =
    let inline curry f a b = f (a, b)
    let (>>-) a f =
        async {
            let! x = a
            return f x
        }
    let (>>-!) a f =
        async {
            let! x = a
            return Result.map f x
        }

module Cmd =
    let ofAsync a f =
        async {
            let! b = a |> Async.Catch
            return match b with | Choice1Of2 x -> f <| Ok x | Choice2Of2 e -> f <| Error e
        } |> Cmd.ofAsyncMsg
    let ofAsync0 a f =
        async {
            let! b = a
            return f b
        } |> Cmd.ofAsyncMsg

module Effects =
    type DownloadString = DownloadString of Uri * (string -> unit)
    let downloadFromWeb (uri : Uri) =
        async { return! (new WebClient()).DownloadStringTaskAsync uri |> Async.AwaitTask }
        |> Effect.wrap (curry DownloadString uri)

module Services =
    open FSharp.Data
    open Newtonsoft.Json

    [<CLIMutable>] type CountryProvider = { Code : string }
    [<CLIMutable>] type StateProvider = { Region : string }
    [<CLIMutable>] type CityProvider = { Name : string }

    let private key = ""

    let loadCountries : CountryProvider array Async =
        sprintf "https://google.com/#%s" key |> Uri
        |> Effects.downloadFromWeb
        >>- JsonConvert.DeserializeObject<CountryProvider array>
    let loadStates country : StateProvider array Async =
        sprintf "https://.google.com/#%s-%s" country key |> Uri
        |> Effects.downloadFromWeb
        >>- JsonConvert.DeserializeObject<StateProvider array>
    let loadCities country state : CityProvider array Async =
        sprintf "https://google.com/#%s-%s-%s" country state key |> Uri
        |> Effects.downloadFromWeb
        >>- JsonConvert.DeserializeObject<CityProvider array>

module Page =
    type Model =
        { countries : Services.CountryProvider array
          states : Services.StateProvider array
          cities : Services.CityProvider array
          isLoading : bool
          selectedCountry : int option
          selectedState : int option
          selectedCity : int option }

    type Target = | Country | State | City

    type Msg =
        | CountriesLoaded of Result<Services.CountryProvider array, exn>
        | StatesLoaded of Result<Services.StateProvider array, exn>
        | CitiesLoaded of Result<Services.CityProvider array, exn>
        | ItemSelected of Target * int

    let initModel =
        { countries = [||]; states = [||]; cities = [||]
          selectedCountry = None; selectedState = None; selectedCity = None
          isLoading = true }

    let init() = initModel, Cmd.ofAsync Services.loadCountries CountriesLoaded

    let update msg model =
        match msg with
        | CountriesLoaded(Ok xs) -> { model with countries = xs; isLoading = false }, Cmd.none
        | StatesLoaded(Ok xs) -> { model with states = xs; isLoading = false }, Cmd.none
        | CitiesLoaded(Ok xs) -> { model with cities = xs; isLoading = false }, Cmd.none
        | ItemSelected(target, id) ->
            match target with
            | Country ->
                { model with selectedCountry = Some id
                             selectedState = None
                             selectedCity = None
                             isLoading = true
                             states = [||]
                             cities = [||] },
                Cmd.ofAsync (Services.loadStates (model.countries.[id].Code)) StatesLoaded
            | State ->
                { model with selectedState = Some id
                             selectedCity = None
                             isLoading = true
                             cities = [||] },
                Cmd.ofAsync
                    (Services.loadCities (model.countries.[model.selectedCountry.Value].Code) model.states.[id].Region)
                    CitiesLoaded
            | City -> { model with selectedCity = Some id }, Cmd.none
        | _ -> failwith "TODO"
