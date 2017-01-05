module Diffusion

let BlackScholes(s0:float) (r:float) (sigma:float) (x:int) (t:float) (rand:float) = 
    let variance = sigma*sigma
    s0*exp((r-0.5*variance)*t + sigma*rand*sqrt(t))

let ModelAdapter model (marketData:MarketData.MarketContainer) = 
    match model with
    | "BlackScholes" -> BlackScholes (marketData.GetMarketData "Spot") (marketData.GetMarketData "FlatRate") (marketData.GetMarketData "FlatVol")
    | _ -> failwith (model + " not found in ModelAdapter")