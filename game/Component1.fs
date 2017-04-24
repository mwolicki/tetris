﻿namespace game

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input
open System 


module GameDraw = 
    open Game
    let colors = [| Color.Red; Color.Gold; Color.Green; Color.Yellow; Color.Blue; Color.Violet; Color.WhiteSmoke; 
                    Color.Brown; Color.Orange; Color.Aqua; Color.Chocolate |]
    
    let draw width height state = 
         let rectH = height/BoardSize.height
         let rectW = width/BoardSize.width
         seq { for i = 0 to state.Blocks.Length1 - 1 do
                 for j = 0 to state.Blocks.Length2 - 1 do
                     let color = state.Blocks.[i,j] |> int |> abs
                     if color > 0 then
                         yield  (Rectangle(i*rectW, j*rectH, rectW - rectW / 10 , rectH - rectH / 10), colors.[color % colors.Length]) }


type GameClass () as game =
    inherit Game()
    let graphics = new GraphicsDeviceManager (game)
    let mutable spriteBatch : SpriteBatch = null
    let mutable whiteRectangle : Texture2D = null
    let mutable time = 1000.
    let mutable time2 = 1000.
    let mutable state = Game.initState
    let rnd = Random()
    do
        game.Content.RootDirectory <- "Content"
        graphics.PreferredBackBufferWidth <- 750
        graphics.PreferredBackBufferHeight <- 1250

    static member IsKeyDown = Keyboard.GetState().IsKeyDown
    override game.LoadContent () =
        base.LoadContent()
        spriteBatch <- new SpriteBatch(game.GraphicsDevice)
        whiteRectangle <- new Texture2D(game.GraphicsDevice, 1, 1)
        whiteRectangle.SetData([| Color.White |]);
    override game.Update (gameTime: GameTime) =

        if GameClass.IsKeyDown Keys.R then
            state <- Game.initState

        let speed = 600. - float (min (state.Points * 10) 350)
        if time2 > 120. || time > speed then
            if time2 > 120. then
                time2 <- 0.
                let keyPressed : Game.KeyPressed = 
                    {   Up = GameClass.IsKeyDown Keys.Up
                        Down = GameClass.IsKeyDown Keys.Down
                        Left = GameClass.IsKeyDown Keys.Left
                        Right = GameClass.IsKeyDown Keys.Right }
                state <- state |> Game.applyKeyboardTransition keyPressed
                               |> Game.score
                
            if time > speed then
                time <- 0.
                state <- state 
                    |> Game.move
                    |> Game.applyTransition rnd
                    |> Game.score
                    
        else
            game.SuppressDraw()
            System.GC.Collect 1
            

        time <- time + gameTime.ElapsedGameTime.TotalMilliseconds
        time2 <- time2 + gameTime.ElapsedGameTime.TotalMilliseconds

        System.GC.Collect 0        
        base.Update gameTime
    
    override game.Draw (gameTime: GameTime) =
        graphics.GraphicsDevice.Clear (Color.Black);

        spriteBatch.Begin();
        let draw = GameDraw.draw graphics.PreferredBackBufferWidth graphics.PreferredBackBufferHeight state
        draw |> Seq.iter(fun (rect, color) -> spriteBatch.Draw(whiteRectangle, rect, color))
        spriteBatch.End()

        base.Draw gameTime