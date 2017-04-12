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

let score (state:State) : State =
    let width = state.Blocks |> Array2D.length1
    let height = state.Blocks |> Array2D.length2
    let result = Array2D.init width height (fun _ _ -> false, opt ())
    let mutable moveUp = 0
    for x = width - 1 downto 0 do
        let fullRow = 
            seq { for y = height - 1 downto 0 do
                    let v, c = state.Blocks.[x,y]
                    yield (not v) && c.HasValue } |> Seq.forall id
        if fullRow then moveUp <- moveUp + 1
        else
            let x' = x + moveUp
            if x' >= 0 then
                for y = height - 1 downto 0 do
                    Array2D.set result x y (state.Blocks.[x', y]) 
    { state with Blocks = result; Points = state.Points + moveUp }
            


state [ [ n; a; n ]
        [ n; b; n ] 
        [ n; n; n ] ] 
|> score { KeyPressed with Down = true } == state [
        [ n; a; n ]
        [ n; b; n ] 
        [ n; n; n ] ] 
