module MarketData

open System
open FSharp.Data

open NUnit.Framework
open FsUnit

//Data type definition
type MarketDataProvider = JsonProvider<""" 
  [ { "timeStamp":"17/11/2016 8:15:19", "spot":100.00 }, 
    { "timeStamp":"17/11/2016 8:15:19", "sigma":0.1 },
    { "timeStamp":"17/11/2016 8:15:19", "rate":0.01 } ] """>

//Wrapper for the data types
type Spot = { TimeStamp:DateTime; Value:float }
type FlatRate = { TimeStamp:DateTime; Rate:float }
type FlatVolSurface = { TimeStamp:DateTime; Sigma:double }
type VolSurface = { TimeStamp:DateTime; Sigma:float list }

type MarketData =
    | Spot of Spot
    | FlatRate of FlatRate
    | FlatVol of FlatVolSurface
    | VolSurface of VolSurface
    member self.Key = 
        match self with
        | Spot _ -> "Spot"
        | FlatVol _ -> "FlatVol"
        | FlatRate _ -> "FlatRate"
        | VolSurface _ -> "Vol"

let MarketDataBuilder (marketData:MarketDataProvider.Root) =
    match (marketData.Sigma, marketData.Spot, marketData.Rate) with
    | (sigma, None, None) -> FlatVol({TimeStamp=DateTime.Parse(marketData.TimeStamp) ; Sigma=float(sigma.Value)})
    | (None, spot, None) -> Spot({TimeStamp=DateTime.Parse(marketData.TimeStamp); Value=float(spot.Value)})
    | (None, None, rate) -> FlatRate({TimeStamp=DateTime.Parse(marketData.TimeStamp); Rate=float(rate.Value)})
    | _ -> failwith "Unknown market data in MarketDataProvider"

type MarketContainer = 
    {TimeStamp:DateTime; Data:MarketData list} with
    static member Create (t:DateTime) (d:MarketData list) = {TimeStamp=t;Data=d}
    static member FromJSon (marketData:MarketDataProvider.Root[]) =
        MarketContainer.Create 
            (System.DateTime.UtcNow) 
            [for data in marketData -> (MarketDataBuilder data)]
    member this.GetMarketData(data:MarketData) = 
        match data with 
            | Spot s -> s.Value
            | FlatVol f -> f.Sigma       
            | FlatRate r -> r.Rate
            | _ -> failwith (data.Key + " not found in MarketContainer Flat market data")
    member this.GetMarketData(data:MarketData,t:int) = 
        match data with 
            | Spot _ | FlatVol _ | FlatRate _ -> this.GetMarketData data
            | VolSurface surface ->  surface.Sigma.[t]
    member this.GetMarketData(key:string,t:int) = 
        List.tryFind (fun (elem:MarketData) -> elem.Key = key) this.Data
        |> fun data ->
        match data with 
        | Some x ->  this.GetMarketData(x, t)
        | None -> failwith (key + " not found in MarketContainer")
    member this.GetMarketData(key:string) = 
        List.tryFind (fun (elem:MarketData) -> elem.Key = key) this.Data
        |> fun data -> 
        match data with 
        | Some x ->  this.GetMarketData(x, 0)
        | None -> failwith (key + " not found in MarketContainer")

[<Test>]
let ``MarketContainer.GetMarketData from JSon data source``() =
    let marketData = MarketContainer.FromJSon(MarketDataProvider.GetSamples())
    marketData.GetMarketData "Spot" |> should equal 100.0
    marketData.GetMarketData "FlatVol" |> should equal 0.1
    marketData.GetMarketData "FlatRate" |> should equal 0.01

[<Test>]
let ``MarketContainer.GetMarketData throws on unknown``() =
    let marketData = MarketContainer.FromJSon(MarketDataProvider.GetSamples())
    (fun () -> marketData.GetMarketData "UnknownMarketData" |> ignore)
    |> should (throwWithMessage "UnknownMarketData not found in MarketContainer") typeof<System.Exception>    
