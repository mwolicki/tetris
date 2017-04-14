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
let colors = [| "rgb(200,0,0)"; "rgb(0,200,0)"; "rgb(200,200,0)"; "rgb(200,200,200)"; "rgb(0, 0, 200)" |]

let draw state = 
    let span = Browser.document.getElementsByTagName_span().[0]
    span.innerText <- state.Points.ToString()
    

    let canvas = Browser.document.getElementsByTagName_canvas().[0]
    
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
            else
                ctx.fillStyle <- !^"rgb(20,20,20)"            
            ctx.fillRect (float <| i*rectW, float <| j*rectH, float <| rectW - rectW / 10 , float <| rectH - rectH / 10)
                


type AgentEvent =
| Time
| KeyPressed of Game.KeyPressed


let mutable score = 0

let game = MailboxProcessor.Start (fun inbox->
    let rnd = Random()
    let rec loop state = async {
        try
            let! msg = inbox.Receive()
            let state =
                match msg with
                | Time -> 
                    state
                    |> Game.move
                    |> Game.applyTransition rnd
                    |> Game.score
                | KeyPressed keyPressed -> 
                    state
                    |> Game.applyKeyboardTransition keyPressed
                    |> Game.score
            score <- state.Points
            draw state
            return! loop state
        with e -> 
            Browser.console.error e 
            return! loop state }
    loop Game.initState )


Browser.window.addEventListener_keypress(fun x-> 
    match x.charCode |> int |> char with
    | 'w' | 'W' -> { Game.KeyPressed.Default with Up = true } |> Some
    | 'a' | 'A' -> { Game.KeyPressed.Default with Left = true }  |> Some
    | 'd' | 'D' -> { Game.KeyPressed.Default with Right = true } |> Some
    | 's' | 'S' -> { Game.KeyPressed.Default with Down = true } |> Some
    | _ -> None
    |> Option.bind (KeyPressed >> game.Post >> Some)
    |> ignore
    box null)




Browser.window.addEventListener_keydown(fun x-> 
    match x.which |> int  with
    | 38 -> { Game.KeyPressed.Default with Up = true } |> Some
    | 37 -> { Game.KeyPressed.Default with Left = true }  |> Some
    | 39 -> { Game.KeyPressed.Default with Right = true } |> Some
    | 40 -> { Game.KeyPressed.Default with Down = true } |> Some
    | _ -> None
    |> Option.bind (KeyPressed >> game.Post >> Some)
    |> ignore
    box null)


let rec touchEvents startXY endXY = 
    let body = Browser.document.body
    
    let(|Left|Right|Top|Down|Nothing|) ((startX, startY), (endX, endY))  =
        let minHeightDiff  = body.clientHeight * 0.1
        let minWidthDiff = body.clientWidth * 0.1
        match endX-startX, endY - startY with
        | x, _ when x > minWidthDiff -> Right
        | x, _ when x < -minWidthDiff -> Left
        | _, y when y > minHeightDiff -> Down
        | _, y when y < -minHeightDiff -> Top
        | _ -> Nothing


    match (startXY, endXY) with
    | Left -> { Game.KeyPressed.Default with Left = true } |> Some
    | Right -> { Game.KeyPressed.Default with Right = true } |> Some
    | Top -> { Game.KeyPressed.Default with Up = true } |> Some
    | Down -> { Game.KeyPressed.Default with Down = true } |> Some
    | Nothing -> None
    |> Option.bind (KeyPressed >> game.Post >> Some)
    |> ignore


let setUpTouchEvents()  =
    let mutable startXY = 0., 0.

    let body = Browser.document.body
    body.addEventListener_touchstart (fun x->
        let touches = x.changedTouches.[0]
        startXY <- touches.clientX, touches.clientY
        box null)

    body.addEventListener_touchend (fun x->
        let touches = x.changedTouches.[0]
        printf "%A - %A" startXY (touches.clientX, touches.clientY)
        touchEvents startXY (touches.clientX, touches.clientY)
        box null)

setUpTouchEvents()

let rec init () :Async<unit> = async {
        game.Post Time
        do! Async.Sleep (500 - min 250 (score*10))
        return! init () }

init () |> Async.StartImmediate

