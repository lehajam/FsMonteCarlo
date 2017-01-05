module MonteCarlo

open System
open Contracts
open MathNet.Numerics
open MathNet.Numerics.Statistics
open FSharp.Charting
//open XPlot.GoogleCharts

let NormInv (x:int) (t:float list) = 
    let dim = x * t.Length
    let sample = Array.zeroCreate<float>(dim)
    Distributions.Normal.Samples(sample, 0.,1.)
    sample

let Diffuse(x:int) (t:int) (timeStep:float list) (model:int->float->float->float) (rand:float[]) = 
    let paths= Array.zeroCreate<float>(rand.Length)
    for i = 0 to x-1 do
        for j = 0 to t-1 do
            let index = i*t+j
            paths.[index] <- model i timeStep.[j] rand.[index]
    paths

let EvaluatePayoff(x:int) (t:int) (payoff:int->int->float[]->float) (rand:float[]) = 
    let results = Array.zeroCreate<float>(x) 
    for i = 0 to x-1 do
        results.[i] <- payoff i t rand
    results

let MonteCarloSimulation nbSimulation (product:Product) =
    NormInv nbSimulation product.Schedule
    |> Diffuse nbSimulation product.Schedule.Length product.Schedule product.Model
    |> EvaluatePayoff nbSimulation product.Schedule.Length product.Payoff

let Price marketData products = 
    for product in products do
        let NumTrials = pown 2 13
        MonteCarloSimulation NumTrials product 
        |> Array.average 
        |> printfn "%f"

let Convergence marketData products = 
    let confidenceInterval99 mean sd sampleSize = 
        Distributions.StudentT.InvCDF(0., 1., sampleSize-1., (0.99 + 1.)/2.) * (sd / sqrt(sampleSize))

    [for product in products ->
        [for i in [5..20] ->
            let NumTrials = pown 2 i
            let stopWatch = System.Diagnostics.Stopwatch.StartNew()
            let prices = MonteCarloSimulation NumTrials product 
            stopWatch.Stop()
            (Statistics.Mean(prices), Statistics.PopulationStandardDeviation(prices), float(NumTrials), stopWatch.Elapsed.TotalMilliseconds)]
        |> fun data -> (( "Prices", [for (price, _, _, _) in data -> price]),
                        ("LowerBound", [ for (price, stDev, trials, _) in data -> price - (confidenceInterval99 price stDev trials)]),
                        ("UpperUpper", [ for (price, stDev, trials, _) in data -> price + (confidenceInterval99 price stDev trials)]))]

