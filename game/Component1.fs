namespace game

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input
open System 

module Array2D =
    let inline exists f arr = 
        let iLength = Array2D.length1 arr
        let jLength = Array2D.length2 arr
        let rec exists' i j =
            match arr.[i,j] |> f with
            | true -> true
            | _ when j < jLength - 1 -> exists' i (j+1)
            | _ when i < iLength - 1 -> exists' (i+1) 0
            | _ -> false
        exists' 0 0

module Game =

    type opt<'a when 'a : (new : unit -> 'a) and 'a : struct and 'a :> ValueType> = Nullable<'a>

    [<RequireQualifiedAccess>]
    module BoardSize = 
        [<Literal>]
        let width = 10
        [<Literal>]
        let height = 15

    type BlockType = L = 0 | T = 1 | Line = 2 | Squere = 3

    [<AutoOpen>]
    module Extensions =
        type BlockType with
            member bt.GetStringRepresentation() =
                match bt with
                | BlockType.L -> [| "X"
                                    "X"
                                    "XX"|]

                | BlockType.T -> [| " X "
                                    "XXX"|]

                | BlockType.Line -> [| "XxxX"|]

                | BlockType.Squere -> [|"XX"
                                        "XX"|]
                 | _ -> [|"X X"
                          " X "
                          "X X"|]
              
            member bt.GetArray (c:Color) =
                let str = bt.GetStringRepresentation()
                Array2D.init 3 4 (fun x y-> if str.Length > x && str.[x].Length > y && str.[x].[y] <> ' ' then opt c else opt())

    [<Struct>]
    type Block = { Color:Color }

    [<Struct>]
    type State = { Blocks : (struct (bool * Color opt)) [,] }

    [<Struct>]
    type KeyPressed = { Up : bool; Down : bool; Left : bool; Right : bool }
        with member k.Any () = k.Up || k.Down || k.Left || k.Right

    let colors = [| Color.Red; Color.Gold; Color.Green; Color.Yellow; Color.Blue; Color.Violet; Color.WhiteSmoke; 
                    Color.Brown; Color.Orange; Color.Aqua; Color.Chocolate |]

    let empty = struct (false, opt())

    let initState = { Blocks = Array2D.create BoardSize.width BoardSize.height empty }

    let applyKeyboardTransition (keyPressed:KeyPressed) (state:State) =
        if keyPressed.Any () then
            { state with 
                Blocks =
                    Array2D.init BoardSize.width BoardSize.height
                        (fun x y ->
                            let x' = if keyPressed.Left then x + 1 
                                     elif keyPressed.Right then x - 1 
                                     else x 
                                     |> max 0 |> min (BoardSize.width - 1)
                            let y' = if keyPressed.Down then y - 1 else y
                                     |> max 0
                            let (struct (v,c)) = state.Blocks.[x',y']

                            if v then 
                                if keyPressed.Left && x = BoardSize.width - 1 then empty
                                elif keyPressed.Right && x = 0 then empty
                                elif keyPressed.Down && y = 0 then empty
                                else (struct (v,c)) 
                            else empty) }
        else state

    let move (state:State) =
        { state with 
            Blocks =
                Array2D.init BoardSize.width BoardSize.height
                    (fun x y ->
                        if y = 0 then empty
                        else state.Blocks.[x,y-1]) }


    let applyTransition (rnd:Random) (state:State) =
        let blocks = state.Blocks

        if blocks |> Array2D.exists (fun (struct (x,_))-> x) |> not then
            let nextBlock = enum<BlockType> (rnd.Next() % 4)
            let middle = (blocks |> Array2D.length1) / 2
            let color = colors.[rnd.Next colors.Length]

            Array2D.iteri (fun x y (v:Color opt) -> 
                if v.HasValue then Array2D.set blocks (x+middle-1) y (struct (true, v))) (nextBlock.GetArray color)
        { state with Blocks = blocks}

    let draw width height state = 
        let rectH = height/BoardSize.height
        let rectW = width/BoardSize.width
        seq { for i = 0 to Array2D.length1 state.Blocks - 1 do
                for j = 0 to Array2D.length2 state.Blocks - 1 do
                    let struct(_, color) = state.Blocks.[i,j]
                    if color.HasValue then
                        yield struct (Rectangle(i*rectW, j*rectH, rectW, rectH), color.Value) }

type GameClass () as game =
    inherit Game()
    let graphics = new GraphicsDeviceManager (game)
    let mutable spriteBatch : SpriteBatch = null
    let mutable whiteRectangle : Texture2D = null
    let mutable time = 1000.
    let mutable state = Game.initState
    let rnd = Random()

    do
        game.Content.RootDirectory <- "Content"
        graphics.PreferredBackBufferWidth <- 750
        graphics.PreferredBackBufferHeight <- 1250
    override game.LoadContent () =
        base.LoadContent()
        spriteBatch <- new SpriteBatch(game.GraphicsDevice)
        whiteRectangle <- new Texture2D(game.GraphicsDevice, 1, 1)
        whiteRectangle.SetData([| Color.White |]);
    override game.Update (gameTime: GameTime) =


        if time % 100. < 0.1 || time > 500. then
            let keyboard = Keyboard.GetState PlayerIndex.One
            let keyPressed : Game.KeyPressed = 
                {   Up = keyboard.IsKeyDown Keys.Up
                    Down = keyboard.IsKeyDown Keys.Down
                    Left = keyboard.IsKeyDown Keys.Left
                    Right = keyboard.IsKeyDown Keys.Right }
            state <- state |> Game.applyKeyboardTransition keyPressed
            
            if time > 500. then
                time <- 0.
                state <- state 
                    |> Game.move
                    |> Game.applyTransition rnd
        else
            game.SuppressDraw()

        time <- time + gameTime.ElapsedGameTime.TotalMilliseconds

        System.GC.Collect 0        
        base.Update gameTime
    
    override game.Draw (gameTime: GameTime) =
        graphics.GraphicsDevice.Clear (Color.Black);

        spriteBatch.Begin();
        let draw = Game.draw graphics.PreferredBackBufferWidth graphics.PreferredBackBufferHeight state
        draw |> Seq.iter(fun struct(rect, color) -> spriteBatch.Draw(whiteRectangle, rect, color))
        spriteBatch.End()

        base.Draw gameTime