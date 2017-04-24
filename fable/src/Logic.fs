namespace game

open System 

module Array2D =
    type Array2D<'a> = private { 
        base1 : int
        base2 : int
        arr: 'a array }
    with 
        member a.Item 
            with get(x, y) = a.arr.[a.base2 * x + y]
            and set (x, y) (value:'a) = a.arr.[a.base2 * x + y] <- value
        member x.Length1 = x.base1
        member x.Length2 = x.base2
        member x.ToSeq () = seq { for i = 0 to x.arr.Length - 1 do yield i/x.base2, i % x.base2, x.arr.[i] }
        //override x.ToString () = sprintf "%A" (x.arr |> Array.chunkBySize x.base2)
        interface Collections.Generic.IEnumerable<int*int*'a> with
            override x.GetEnumerator () = x.ToSeq().GetEnumerator()
            override x.GetEnumerator () = x.ToSeq().GetEnumerator() :> Collections.IEnumerator

    let inline exists f arr = arr.arr |> Array.exists f

    let inline set (arr:Array2D<_>) a b v = arr.[a,b] <- v

    let inline init x y f =
        { base1 = x
          base2 = y
          arr = [| for i = 0 to x - 1 do 
                   for j = 0 to y - 1 do yield f i j |] }
    let inline create a b x = init a b (fun _ _ -> x)
    let inline mapi f arr = init arr.base1 arr.base2 (fun i j -> f i j arr.[i,j])

    let inline map f = mapi (fun _ _ x -> f x)

    let inline iteri f arr = arr |> Seq.iter f

    let ofList (l:'a list list) =
        let x = l.Length
        let y = l.Item 0 |> List.length
        init y x (fun x y -> (l.Item y).Item x)

module Game =
    type Color = int8
    type System.SByte with
        member s.IsActive = s < 0y
        member s.IsNone = s = 0y
        member s.IsPassive = s > 0y
        member s.IsNotPassive = s <= 0y

    [<RequireQualifiedAccess>]
    module BoardSize = 
        [<Literal>]
        let width = 10
        [<Literal>]
        let height = 15

    [<Struct>]
    type State = { Blocks : Color Array2D.Array2D; Points : int }

    [<Struct>]
    type KeyPressed = { Up : bool; Down : bool; Left : bool; Right : bool }
        with 
            member k.Any () = k.Up || k.Down || k.Left || k.Right
            static member Default = { Up = false; Down = false; Left = false; Right = false }
    let (|IsLeftPressed|IsRightPressed|IsUpPressed|IsDownPressed|Nothing|) (kp:KeyPressed) =
        if kp.Up then IsUpPressed
        elif kp.Down then IsDownPressed
        elif kp.Left then IsLeftPressed
        elif kp.Right then IsRightPressed
        else Nothing

    let empty = 0y

    let initState = { Blocks = Array2D.create BoardSize.width BoardSize.height empty; Points = 0 }

    let score (state:State) : State =
        let width = state.Blocks.Length1
        let height = state.Blocks.Length2
        let result = Array2D.create width height empty
        let mutable moveUp = 0
        for y = height - 1 downto 0 do
            let fullRow = 
                seq { for x = width - 1 downto 0 do
                        yield state.Blocks.[x,y].IsPassive } |> Seq.forall id
            if fullRow then moveUp <- moveUp + 1
            if not fullRow then
                let y' = y + moveUp
                if y' >= 0 then
                    for x = width - 1 downto 0 do
                        Array2D.set result x y' (state.Blocks.[x, y]) 
        let points = Seq.init (moveUp+1) id |> Seq.sum
        { state with Blocks = result; Points = state.Points + points }

    let canApplyKeyboardTransition (keyPressed:KeyPressed) (state:State) =
        let width = state.Blocks.Length1
        let height = state.Blocks.Length2
        if not <| keyPressed.Any () then true
        else
            let isValidMove (x, y, _) = 
                match keyPressed with
                | IsLeftPressed -> x > 0 && state.Blocks.[x-1,y].IsNotPassive
                | IsRightPressed -> x < width - 1 && state.Blocks.[x+1,y].IsNotPassive
                | IsDownPressed -> y < height - 1 && state.Blocks.[x,y+1].IsNotPassive
                | _ -> true
            state.Blocks
            |> Seq.filter (fun (_,_,v) -> v.IsActive)
            |> Seq.forall isValidMove

    let activeBlockSpace (state:State) =
        let min a b = (match a with None -> b | Some a -> min a b) |> Some
        let max a b = (match a with None -> b | Some a -> max a b) |> Some
        state.Blocks 
        |> Seq.filter (fun (_,_, v) -> v.IsActive)
        |> Seq.fold (fun (minX, maxX, minY, maxY) (x, y, _) -> (min minX x), (max maxX x), (min minY y), (max maxY y)) (None, None, None, None)
        |> function 
        | Some minX, Some maxX, Some minY, Some maxY -> ((minX, maxX), (minY, maxY)) |> Some
        | _ -> None

    let canRotate (state:State) =
        let width = state.Blocks.Length1
        let height = state.Blocks.Length2
        let p = activeBlockSpace state 
        let (Some ((minX, maxX), (minY, maxY))) = p
        let distance = max (maxX - minX) (maxY - minY) + 1
        let centralX = (minX + maxX)/2
        let centralY = (minY + maxY)/2
        let (minX', maxX', minY', maxY') = (centralX - distance / 2), (centralX + distance / 2), (centralY - distance / 2), (centralY + distance / 2)

        state.Blocks 
        |> Seq.exists (fun (x,y, v) -> x >= minX' && x <= maxX' && y >= minY' && y <= maxY' && v.IsPassive)
        |> function
        | false when minX > 0 && minY > 0 && maxX < width && maxY < height -> Some (centralX, centralY, distance)
        | _  -> None
        
    let rotate (state:State) =
        match canRotate state with
        | None -> state
        | Some (centralX, centralY, distance) ->
            let (minX, maxX, minY, maxY) = (centralX - distance / 2), (centralX + distance / 2), (centralY - distance / 2), (centralY + distance / 2)
            
            let blocks = 
                state.Blocks 
                |> Array2D.mapi (fun x y v ->
                    if x >= minX && x <= maxX && y >= minY && y <= maxY then
                        let x' = centralX - (y - centralY)
                        let y' = centralY + (x - centralX)
                        state.Blocks.[x', y']
                    else v)
            { state with Blocks = blocks }

    let applyKeyboardTransition (keyPressed:KeyPressed) (state:State) =
        let width = state.Blocks.Length1
        let height = state.Blocks.Length2
        if keyPressed.Any () && canApplyKeyboardTransition keyPressed state then
            let blocks =
                    state.Blocks |> Array2D.mapi
                        (fun x y _ ->
                            let x' = match keyPressed with
                                     | IsLeftPressed -> x + 1 
                                     | IsRightPressed -> x - 1 
                                     | _ -> x 
                                     |> max 0 |> min (width - 1)
                            let y' = match keyPressed with
                                     | IsDownPressed -> y - 1
                                     | _ -> y
                                     |> max 0
                            let v = state.Blocks.[x',y']

                            if v.IsActive then 
                                match keyPressed with
                                | IsLeftPressed when x = width - 1 -> empty
                                | IsRightPressed when x = 0 -> empty
                                | IsDownPressed when y = 0 -> empty
                                | _ -> v
                                
                            else 
                                let v = state.Blocks.[x,y]
                                if v.IsActive then empty
                                else v)
            let state = { state with Blocks = blocks }
            if keyPressed.Up then rotate state else state
        else state

    let canMove (state:State) =
        let height = state.Blocks.Length2
        let isValidMove (x, y, _) = 
            if y = height - 1 then false
            else state.Blocks.[x,y+1].IsNotPassive

        state.Blocks 
        |> Seq.filter (fun (_,_,v)->v.IsActive)
        |> Seq.forall isValidMove

    let move (state:State) =
        if canMove state then
            { state with Blocks = state.Blocks |> Array2D.mapi 
                                                    (fun x y v ->
                                                        if y = 0 then empty
                                                        else
                                                            let v' = state.Blocks.[x,y-1]
                                                            if v'.IsActive then v'
                                                            elif v.IsActive then empty
                                                            else v) }
        else { state with Blocks = state.Blocks |> Array2D.map abs }

    let blocks : string array array = 
                  [| [| "L"
                        "L"
                        "LL"|];

                     [| " J"
                        " J"
                        "JJ"|];

                    [|  " X ";
                        "XXX"|];

                    [|  " SS";
                        "SS"|];

                    [|  "ZZ ";
                        " ZZ"|];

                    [| "XXXX"|];

                    [|"CC"
                      "CC"|];
                      
                    [|"X"
                      "X" |] |]
              
    let getRandomBlock (rnd:Random) =
            let str = blocks.[rnd.Next blocks.Length]
            let c = int System.SByte.MaxValue |> rnd.Next
            let c = -c - 1 |> int8

            Array2D.init 3 4 (fun x y-> if str.Length > x && str.[x].Length > y && str.[x].[y] <> ' ' then c else 0y)

    let applyTransition (rnd:Random) (state:State) =
        let blocks = state.Blocks

        if blocks |> Array2D.exists (fun x -> x.IsActive) |> not then
            let nextBlock = getRandomBlock rnd
            let middle = (blocks.Length1) / 2
            let color = rnd.Next 255

            Array2D.iteri (fun (x, y, (v:int8)) -> 
                if v.IsActive then 
                    Array2D.set blocks (x+middle-1) y v) nextBlock
        { state with Blocks = blocks}
