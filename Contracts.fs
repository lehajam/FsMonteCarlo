module Contracts
open FSharp.Data

open NUnit.Framework
open FsUnit

type ContractProvider = JsonProvider<""" 
    [   { "strike":100.0, "expiry":1. }, 
        { "strike":100.0, "expiry":1., "barrier":90.0 } ] """>

type Contract =
    | European of float * float
    | Barrier of float * float * float
 
type Product = {
    Schedule:float list
    Model:int->float->float->float
    Payoff:int->int->float[]->float
}

let ScheduleBuilder contract = 
    match contract with 
    | European(expiry,_) -> [expiry]
    | Barrier(expiry,_,_) ->  [1. .. expiry]

let ContractBuilder (x:ContractProvider.Root) =
    match (x.Expiry, x.Strike, x.Barrier) with
    | (expiry, strike, None) -> European(float(expiry), float(strike))
    | (expiry, strike, barrier) -> Barrier(float(expiry), float(strike), float(barrier.Value))

let ProductBuilder marketData contract = 
    let (+) model payoff = (model, payoff)
    let BlackScholesAdapter = (Diffusion.ModelAdapter "BlackScholes" marketData)
    match contract with
    | European(expiry,strike) -> BlackScholesAdapter + (Payoffs.EuropeanCall strike)
    | Barrier(expiry,strike,barrier) ->  BlackScholesAdapter + (Payoffs.BarrierKnockIn strike barrier)
    |> fun (model, payoff) -> {Schedule=(ScheduleBuilder contract);Model=model;Payoff=payoff}

let JSonContractLoader (contracts:ContractProvider.Root[]) = 
    [for contract in contracts -> (ContractBuilder contract)]

let JSonProductLoader marketData (contracts:ContractProvider.Root[]) = 
    [for contract in contracts -> (ContractBuilder contract) |> ProductBuilder marketData]

[<Test>]
let ``Loading contracts from JSon``() =
    let contracts = JSonContractLoader(ContractProvider.GetSamples())
    for contract in contracts do 
        match contract with 
        | European(expiry, strike) -> 
            expiry |> should equal 1.0
            strike |> should equal 100.0
        | Barrier(expiry, strike, barrier) -> 
            expiry |> should equal 1.0
            strike |> should equal 100.0
            barrier |> should equal 90.0

[<Test>]
let ``Creating products from contract loaded from JSon``() =
    //Values to use in asserts
    let (simIndex, spotLevel, samplePath) = (0, 100., [|120.|])
    let marketData = MarketData.MarketContainer.FromJSon(MarketData.MarketDataProvider.GetSamples())

    //Methods to use in asserts
    let BlackScholeModel expiry = Diffusion.ModelAdapter "BlackScholes" marketData simIndex expiry spotLevel
    let EuropeanPayoff strike expiry = Payoffs.EuropeanCall strike simIndex (int(expiry)) samplePath
    let BarrierPayoff strike expiry barrier = Payoffs.BarrierKnockIn strike barrier simIndex (int(expiry)) samplePath

    let contracts = JSonContractLoader(ContractProvider.GetSamples())
    for contract in contracts do 
        match contract with 
        | European(expiry, strike) -> 
            ProductBuilder marketData contract
            |> fun p -> 
                p.Schedule |> should equal [expiry]
                p.Model simIndex expiry spotLevel |> should equal (BlackScholeModel expiry)
                p.Payoff simIndex (int(expiry)) samplePath |> should equal (EuropeanPayoff strike expiry)
        | Barrier(expiry, strike, barrier) -> 
            ProductBuilder marketData contract
            |> fun p -> 
                p.Schedule |> should equal [1. .. expiry]
                p.Model simIndex expiry spotLevel |> should equal (BlackScholeModel expiry)
                p.Payoff simIndex (int(expiry)) samplePath |> should equal (BarrierPayoff strike expiry barrier)
