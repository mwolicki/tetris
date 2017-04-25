namespace game
open Game
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input
open System 


module GameDraw = 
    let colors = [| Color.Red; Color.Gold; Color.Green; Color.Yellow; Color.Blue; Color.Violet; Color.WhiteSmoke; 
                    Color.Brown; Color.Orange; Color.Aqua; Color.Chocolate |]
    
    let draw width height state = 
         let rectH = height/BoardSize.height
         let rectW = width/BoardSize.width
         seq { for i = 0 to state.Blocks.Length1 - 1 do
                 for j = 0 to state.Blocks.Length2 - 1 do
                     let color = state.Blocks.[i,j] |> int |> abs
                     let color = if color > 0 then colors.[color % colors.Length] else Color(0.1f, 0.1f, 0.1f) 
                     yield  (Rectangle(i*rectW + rectW / 20, j*rectH + rectH / 20, rectW - rectW / 20 , rectH - rectH / 20), color) }


type GameClass () as game =
    inherit Game()
    let graphics = new GraphicsDeviceManager (game)
    let mutable spriteBatch : SpriteBatch = null
    let mutable whiteRectangle : Texture2D = null
    let mutable time = 10000.
    let mutable time2 = 10000.
    let mutable state = Game.initState
    let rnd = Random()
    do
        game.Content.RootDirectory <- "Content"

    override game.LoadContent () =
        base.LoadContent()
        spriteBatch <- new SpriteBatch(game.GraphicsDevice)
        whiteRectangle <- new Texture2D(game.GraphicsDevice, 1, 1)
        whiteRectangle.SetData([| Color.White |]);
        Touch.TouchPanel.EnabledGestures <- Touch.GestureType.VerticalDrag ||| Touch.GestureType.HorizontalDrag

    member __.Geastures () =
        seq { while Touch.TouchPanel.IsGestureAvailable do yield Touch.TouchPanel.ReadGesture() }

    override game.Update (gameTime: GameTime) =

        let speed = 1000. - float (min (state.Points * 10) 600)
        if time2 > 100. || time > speed then
            if time2 > 100. then
                time2 <- 0.

                match game.Geastures () |> Seq.tryLast with
                | Some geasture ->

                    let keyPressed : Game.KeyPressed = 
                        {   Up = geasture.GestureType = Touch.GestureType.VerticalDrag && geasture.Delta.Y > 0.f
                            Down = geasture.GestureType = Touch.GestureType.VerticalDrag && geasture.Delta.Y < 0.f
                            Left = geasture.GestureType = Touch.GestureType.HorizontalDrag && geasture.Delta.X > 0.f
                            Right = geasture.GestureType = Touch.GestureType.HorizontalDrag && geasture.Delta.X < 0.f }
                    state <- state |> Game.applyKeyboardTransition keyPressed
                                   |> Game.score
                | None -> ()
            if time > speed then
                time <- 0.
                state <- state 
                    |> Game.move
                    |> Game.applyTransition rnd
                    |> Game.score
                    
        else
            //game.SuppressDraw()
            System.GC.Collect 0
            

        time <- time + gameTime.ElapsedGameTime.TotalMilliseconds
        time2 <- time2 + gameTime.ElapsedGameTime.TotalMilliseconds

        System.GC.Collect 0        
        base.Update gameTime
    
    override game.Draw (gameTime: GameTime) =
        graphics.GraphicsDevice.Clear (Color.Black);

        spriteBatch.Begin();
        let draw = GameDraw.draw graphics.GraphicsDevice.Viewport.Width graphics.GraphicsDevice.Viewport.Height state
        draw |> Seq.iter(fun (rect, color) -> spriteBatch.Draw(whiteRectangle, rect, color))
        spriteBatch.End()

        base.Draw gameTime