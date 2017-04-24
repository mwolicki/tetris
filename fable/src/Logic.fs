namespace game

open System 

module Array2D =
    type Array2D<'a> = private { 
        base1 : int
        base2 : int
        arr: 'a array }
    with 
        member a.Item 
            with get(x, y) = 
                if x >= 0 && x < a.base1 && y >= 0 && y < a.base2 then
                    a.arr.[a.base2 * x + y]
                else raise (System.IndexOutOfRangeException ())
            and set (x, y) (value:'a) = 
                if x >= 0 && x < a.base1 && y >= 0 && y < a.base2 then            
                    a.arr.[a.base2 * x + y] <- value
                else raise (System.IndexOutOfRangeException ())
        member x.Length1 = x.base1
        member x.Length2 = x.base2
        override x.ToString () = sprintf "%A" x.arr

    let exists f arr = arr.arr |> Array.exists f

    let set (arr:Array2D<_>) a b v = arr.[a,b] <- v

    let init x y f =
        { base1 = x
          base2 = y
          arr = [| for i = 0 to x - 1 do
                        for j = 0 to y - 1 do
                                    yield f i j |] }
    let create a b x = init a b (fun _ _ -> x)
    let mapi f arr =
        init arr.base1 arr.base2 (fun i j -> f i j arr.[i,j])

    let map f = mapi (fun _ _ x -> f x)

    let toSeq arr = 
        let iLength = arr.base1
        let jLength = arr.base2
        seq { for i = 0 to iLength - 1 do
              for j = 0 to jLength - 1 do
                yield (i, j, arr.[i,j]) }

    let iteri f arr = arr |> toSeq |> Seq.iter f

    let ofList (l:'a list list) =
        let x = l.Length
        let y = l.Item 0 |> List.length
        init y x (fun x y -> (l.Item y).Item x)

module Game =
    type Color = int

    [<RequireQualifiedAccess>]
    module BoardSize = 
        [<Literal>]
        let width = 10
        [<Literal>]
        let height = 15

    [<Struct>]
    type Block = { Color:Color }

    [<Struct>]
    type State = { Blocks : int8 Array2D.Array2D; Points : int }

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

    type System.SByte with
        member s.IsActive = s < 0y
        member s.IsNone = s = 0y
        member s.IsSomething = s <> 0y
        member s.IsPassive = s > 0y
        member s.Color = abs s

    let score (state:State) : State =
        let width = state.Blocks.Length1
        let height = state.Blocks.Length2
        let result = Array2D.init width height (fun _ _ -> empty)
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
            let isValidMove x y = 
                match keyPressed with
                | IsLeftPressed -> x > 0 && (state.Blocks.[x-1,y].IsActive || state.Blocks.[x-1,y].IsNone)
                | IsRightPressed -> x < width - 1 && (state.Blocks.[x+1,y].IsActive || state.Blocks.[x+1,y].IsNone)
                | IsDownPressed -> y < height - 1 && (state.Blocks.[x,y+1].IsActive || state.Blocks.[x,y+1].IsNone)
                | _ -> true
            state.Blocks 
            |> Array2D.toSeq
            |> Seq.filter (fun (_,_,v)->v.IsActive)
            |> Seq.forall (fun (x,y,_) -> isValidMove x y)



    let activeBlockSpace (state:State) =
        let min a b = (match a with None -> b | Some a -> min a b) |> Some
        let max a b = (match a with None -> b | Some a -> max a b) |> Some
        state.Blocks 
        |> Array2D.toSeq
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
        |> Array2D.toSeq
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
        let width = state.Blocks.Length1
        let height = state.Blocks.Length2
        let isValidMove x y = 
            if y = height - 1 then 
                false
            else 
                let v = state.Blocks.[x,y+1]
                v.IsActive || v.IsNone

        state.Blocks 
        |> Array2D.toSeq
        |> Seq.filter (fun (_,_,v)->v.IsActive)
        |> Seq.forall (fun  (x,y,_) -> 
            isValidMove x y)

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
            let c = (int System.SByte.MaxValue) + 1 |> rnd.Next
            let c = -c |> int8

            Array2D.init 3 4 (fun x y-> if str.Length > x && str.[x].Length > y && str.[x].[y] <> ' ' then c else 0y)


    let applyTransition (rnd:Random) (state:State) =
        let blocks = state.Blocks

        if blocks |> Array2D.exists (fun x -> x.IsActive) |> not then
            let nextBlock = getRandomBlock rnd
            let middle = (blocks.Length1) / 2
            let color = rnd.Next 255

            Array2D.iteri (fun (x, y, (v:int8)) -> 
                printfn "%i %i %A" x y v
                if v.IsActive then 
                    Array2D.set blocks (x+middle-1) y v
                    printfn "##%i %i %A" (x+middle-1) y v) nextBlock
        { state with Blocks = blocks}

