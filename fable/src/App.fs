module fable

open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open game.Game
open game
open System

let state2 x p = { Blocks = Array2D.ofList x; Points = p }
let state x = state2 x 0

let n = empty
let a = true, Some 1
let b = false, Some 1
let colors = [| "rgb(200,0,0)"; "rgba(0, 0, 200, 0.5)" |]

let draw state = 
    let canvas = Browser.document.getElementsByTagName_canvas().[0]
    canvas.width <- 400.
    canvas.height <- 600.
    let ctx = canvas.getContext_2d()
    ctx.fillStyle <- !^"black"
    ctx.fillRect (0., 0., canvas.width, canvas.height)


    let rectH = int canvas.height/BoardSize.height
    let rectW = int canvas.width/BoardSize.width
    for i = 0 to state.Blocks.Length1 - 1 do
        for j = 0 to state.Blocks.Length2 - 1 do
            let (_, color) = state.Blocks.[i,j]
            if Option.isSome color then
                ctx.fillStyle <- !^colors.[color.Value % colors.Length]
                ctx.fillRect (float <| i*rectW, float <| j*rectH, float <| rectW - rectW / 10 , float <| rectH - rectH / 10)
                

let rnd = Random()

let rec init (state:State) = async {
        
        let state = state
                    |> Game.move
                    |> Game.applyTransition rnd
                    |> Game.score
        draw state
        do! Async.Sleep 500
        return! init state
}

init Game.initState |> Async.StartImmediate