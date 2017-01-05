module Payoffs

open NUnit.Framework
open FsUnit

let t0 x t =x*t
let T x t =(t0 x t)+(t-1)

let European putCall strike x t (rand:float[]) : float = 
    let T = T x t
    match putCall with 
    | "Call" -> System.Math.Max(rand.[T]-strike, 0.)
    | "Put" -> System.Math.Max(strike-rand.[T], 0.)
    | _ -> failwith "European PutCall must be set to Put or Call"

let EuropeanPut = European "Put"
let EuropeanCall = European "Call"

let Barrier (barrierType:string) (K:float) (barrier:float) (x:int) (t:int) (rand:float[]) : float = 
    let t0 = t0 x t
    let T = T x t    
    let event = (Array.exists (fun x -> x > barrier) (rand.[t0..T]))

    match barrierType with
    | "Out" -> if event then 0.0 else EuropeanCall K x t rand
    | "In" ->  if event then EuropeanCall K x t rand else 0.0
    | _ -> failwith "BarrierType must be set to In or Out"

let BarrierKnockIn = Barrier "In"
let BarrierKnockOut = Barrier "Out"

[<Test>]
let ``Test European Payoff``() =
    EuropeanCall 100. 0 1 ([|120.|]) |> should equal 20.
    EuropeanCall 100. 0 1 ([|90.|]) |> should equal 0.


[<Test>]
let ``Test Barrier Payoff``() =
    let (sim0, strike, expiry, samplePath) = (0, 100., 3, [|110.;120.;130.|])
    BarrierKnockIn strike 0. sim0 expiry samplePath |> should equal (EuropeanCall strike sim0 expiry samplePath)
    BarrierKnockIn strike 140. sim0 expiry samplePath |> should equal 0.
    BarrierKnockIn strike 115. sim0 expiry samplePath |> should equal (EuropeanCall strike sim0 expiry samplePath)