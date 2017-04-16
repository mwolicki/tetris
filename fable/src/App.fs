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
let colors = [| "rgb(200,0,0)"; "rgb(0,200,0)"; "rgb(200,200,0)"; "rgb(255,255,255)"; "rgb(255,140,0)"; "rgb(68, 211, 255)" |]

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

let setUpTouchEvents()  =

    let mutable startXY = 0., 0.
    let mutable startXYTime =  System.DateTime.UtcNow

    let body = Browser.document.body
        
    let setStart x y =
        startXY <- x, y
        startXYTime <- System.DateTime.UtcNow

    let(|LeftTouch|RightTouch|TopTouch|DownTouch|Nothing|) ((startX, startY), (endX, endY)) =
        let minHeightDiff  = body.clientHeight * 0.1
        let minWidthDiff = body.clientWidth * 0.1
        match endX-startX, endY - startY with
        | x, _ when x > minWidthDiff -> RightTouch
        | x, _ when x < -minWidthDiff -> LeftTouch
        | _, y when y > minHeightDiff -> DownTouch
        | _, y when y < -minHeightDiff -> TopTouch
        | _ -> Nothing

    body.addEventListener_touchstart (fun x->
        let touches = x.changedTouches.[0]
        setStart touches.clientX touches.clientY
        box null)

    body.addEventListener_touchmove (fun x->
        if System.DateTime.UtcNow - startXYTime > TimeSpan.FromMilliseconds 50. then
            let touches = x.changedTouches.[0]
            
            match (startXY, (touches.clientX, touches.clientY)) with
            | LeftTouch -> { Game.KeyPressed.Default with Left = true } |> Some
            | RightTouch -> { Game.KeyPressed.Default with Right = true } |> Some
            | TopTouch -> { Game.KeyPressed.Default with Up = true } |> Some
            | DownTouch -> { Game.KeyPressed.Default with Down = true } |> Some
            | Nothing -> None
            |> Option.map (fun key ->
                setStart touches.clientX touches.clientY
                KeyPressed key |> game.Post)
            |> ignore
        box null)

setUpTouchEvents()

let rec init () :Async<unit> = async {
        game.Post Time
        do! Async.Sleep (1000 - min 600 (score * 6))
        return! init () }

init () |> Async.StartImmediate

