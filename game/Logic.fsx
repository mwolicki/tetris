#load "Logic.fs"
open game
open game.Game

let state2 x p = { Blocks = Array2D.ofList x; Points = p }
let state x = state2 x 0

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

state [ [ n; a; n ]
        [ b; b; b ] 
        [ b; b; b ] ] 
|> score == state2 [
        [ n; n; n ]
        [ n; n; n ] 
        [ n; a; n ] ] 2



state [ [ b; a; n; n ]
        [ b; a; a; a ] 
        [ b; a; n; n ]
        [ b; b; b; b ] ] 
|> canRotate == Some (2,1,3)

state [ [ b; a; b; n ]
        [ b; a; a; a ] 
        [ b; a; n; n ]
        [ b; b; b; b ] ] 
|> canRotate == None



state [ [ b; a; n; n ]
        [ b; a; a; a ] 
        [ b; a; n; n ]
        [ b; b; b; b ] ] 
|> rotate == state [
        [ b; n; a; n ]
        [ b; n; a; n ] 
        [ b; a; a; a ]
        [ b; b; b; b ] ] 