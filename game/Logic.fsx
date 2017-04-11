#load "Logic.fs"
open game
open game.Game

let state x = { Blocks = Array2D.ofList x }

let n = empty
let a = true, opt 1
let b = false, opt 1

let inline (==) a b = if a <> b then failwithf "%A != %A" a b

//move
state [ [ n; a; n ]
        [ n; n; n ] ] 
|> move == state [
        [ n; n; n ]    
        [ n; a; n ] ]

state [ [ n; a; n ]
        [ n; b; n ] 
        [ n; n; n ] ] 
|> move == state [
        [ n; b; n ]
        [ n; b; n ] 
        [ n; n; n ] ] 

//key
let KeyPressed = { Up = false
                   Down = false
                   Left = false
                   Right = false }


state [ [ n; a; n ]
        [ n; n; n ] ] 
|> applyKeyboardTransition { KeyPressed with Left = true } == state [
        [ a; n; n ]    
        [ n; n; n ] ]

state [ [ n; a; n ]
        [ n; b; n ] 
        [ n; n; n ] ] 
|> applyKeyboardTransition { KeyPressed with Down = true } == state [
        [ n; a; n ]
        [ n; b; n ] 
        [ n; n; n ] ] 
