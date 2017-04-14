#r "/Users/kevin/.nuget/packages/Fable.Core/1.0.0-narumi-908/lib/netstandard1.6/Fable.Core.dll"


open Fable.Core
open Fable.Core.JsInterop
open Fable.Import.Browser


let toucheEvents  =
    let touchesStart = FSharp.Control.Event<_>()
    let touchesEnd = FSharp.Control.Event<_>()

    document.body.addEventListener_touchstart (fun x->
        let touches = x.changedTouches.[0]
        touchesStart.Trigger (touches.clientX, touches.clientY)
        box null)

    document.body.addEventListener_touchend (fun x->
        let touches = x.changedTouches.[0]
        touchesEnd.Trigger (touches.clientX, touches.clientY)
        box null)

    touchesStart.Publish, touchesEnd.Publish

