module fable

open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open game.Game
open game

let state2 x p = { Blocks = Array2D.ofList x; Points = p }
let state x = state2 x 0

let n = empty
let a = true, Some 1
let b = false, Some 1
let colors = [| "rgb(200,0,0)"; "rgba(0, 0, 200, 0.5)" |]

let draw state = 
    let canvas = Browser.document.getElementsByTagName_canvas().[0]
    canvas.width <- 1000.
    canvas.height <- 800.
    let ctx = canvas.getContext_2d()
    
    let rectH = int canvas.height/BoardSize.height
    let rectW = int canvas.width/BoardSize.width
    for i = 0 to state.Blocks.Length1 - 1 do
        for j = 0 to state.Blocks.Length2 - 1 do
            let (_, color) = state.Blocks.[i,j]
            if Option.isSome color then
                ctx.fillStyle <- !^colors.[color.Value % colors.Length]
                ctx.fillRect (float <| i*rectW, float <| j*rectH, float <| rectW - rectW / 10 , float <| rectH - rectH / 10)
                



let init() =
    state [ [ n; a; n ]
            [ n; b; n ] 
            [ n; n; n ] ] 
    |> draw

init()