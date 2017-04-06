namespace game

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input
open System 

module Game =

    type opt<'a when 'a : (new : unit -> 'a) and 'a : struct and 'a :> ValueType> = Nullable<'a>

    [<RequireQualifiedAccess>]
    module BoardSize = 
        [<Literal>]
        let width = 10
        [<Literal>]
        let height = 15

    type BlockType = L = 0| T = 1 | Line = 2 | Squere = 3

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
    type State = {
        Blocks : (Color opt) [,]
        CurrentBlock : Point opt
    }

    let colors = [| Color.Red; Color.Gold; Color.Green; Color.Yellow; Color.Blue; Color.Violet; Color.WhiteSmoke; 
                    Color.Brown; Color.Orange; Color.Aqua; Color.Chocolate |]

    //let initState = { CurrentBlock = Point(0,0); Blocks = Array2D.init BoardSize.width BoardSize.height (fun w h -> if w * h % 4 = 0 then opt() else opt (Color(w*10%255,h*10%255, 100+w+h%255)))}
    let initState = { CurrentBlock = opt(); Blocks = Array2D.create BoardSize.width BoardSize.height (opt()) }

    let applyTransition (rnd:Random) (state:State) =
        let blocks = state.Blocks
        
        let maxY = blocks |> Array2D.length1

        for i = (blocks |> Array2D.length2) - 1 downto 0 do

            for j = 0 to maxY - 1 do
                if i = 0 then
                    Array2D.set blocks j 0 (opt())
                else
                    Array2D.set blocks j i blocks.[j,i-1]


        if state.CurrentBlock.HasValue |> not || state.CurrentBlock.Value.X = 3 then
            let nextBlock = enum<BlockType> (rnd.Next() % 4)
            let middle = (blocks |> Array2D.length1) / 2
            let color = colors.[rnd.Next colors.Length]


            Array2D.iteri (fun x y (v:Color opt) -> if v.HasValue then Array2D.set blocks (x+middle-1) y v) (nextBlock.GetArray color)



            { state with CurrentBlock = opt (Point(0,0)) }

         else { state with CurrentBlock = opt <| Point(state.CurrentBlock.Value.X + 1, 0) }


    let draw width height state = 
        let rectH = height/BoardSize.height
        let rectW = width/BoardSize.width
        seq { for i = 0 to Array2D.length1 state.Blocks - 1 do
                for j = 0 to Array2D.length2 state.Blocks - 1 do
                    if state.Blocks.[i,j].HasValue then
                        yield struct (Rectangle(i*rectW, j*rectH, rectW, rectH), state.Blocks.[i,j].Value) }

type GameClass () as game =
    inherit Game()
    let graphics = new GraphicsDeviceManager (game)
    

    let mutable spriteBatch : SpriteBatch = null
    let mutable whiteRectangle : Texture2D = null
    let mutable time = 9999.

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
        if time > 500. then
            time <- 0.
            state <- state |> Game.applyTransition rnd
        else
            game.SuppressDraw()
        time <- time + gameTime.ElapsedGameTime.TotalMilliseconds
        
        base.Update gameTime
    
    override game.Draw (gameTime: GameTime) =

        graphics.GraphicsDevice.Clear (Color.Black);

        //let x =if Keyboard.GetState(PlayerIndex.One).IsKeyDown(Keys.Down) then 100 else 10
        spriteBatch.Begin();
 

        let draw = Game.draw graphics.PreferredBackBufferWidth graphics.PreferredBackBufferHeight state

        draw |> Seq.iter(fun struct(rect, color) -> spriteBatch.Draw(whiteRectangle, rect, color))

        spriteBatch.End()

        System.GC.Collect 0
        base.Draw gameTime