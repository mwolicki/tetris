#r "/Users/kevin/.nuget/packages/Fable.Core/1.0.0-narumi-908/lib/netstandard1.6/Fable.Core.dll"

open Fable.Core
open Fable.Core.JsInterop
open Fable.Import

type AgentEvent =
| Time
| KeyPressed of string

let game = MailboxProcessor.Start (fun inbox->
    let rec loop x = async {
        let! msg = inbox.Receive()
        match msg with
        | Time -> ()
        | KeyPressed _ -> ()
        return! loop x
    }
    loop "")

Browser.window.addEventListener_keypress(fun x-> 
    match x.key with
    Browser.console.log x; box null)
