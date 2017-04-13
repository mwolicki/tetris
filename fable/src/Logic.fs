namespace game

open System 

module Array2D =
    type Array2D<'a> = private { 
        base1 : int
        base2 : int
        arr: 'a array array
    }
    with 
        member x.Item 
            with get(i1, i2) = x.arr.[i1].[i2]
            and set (i1, i2) (value:'a) = x.arr.[i1].[i2] <- value
        member x.Length1 = x.base1
        member x.Length2 = x.base2

    let exists f arr = 
        let iLength = arr.base1
        let jLength = arr.base2
        let rec exists' i j =
            match arr.[i,j] |> f with
            | true -> true
            | _ when j < jLength - 1 -> exists' i (j+1)
            | _ when i < iLength - 1 -> exists' (i+1) 0
            | _ -> false
        exists' 0 0

    let set (arr:Array2D<_>) a b v = arr.[a,b]<-v

    let init a b f =
        { base1 = a 
          base2 = b
          arr = [| for i = 0 to a - 1 do
                        yield [| for j = 0 to b - 1 do
                                    yield f i j|] |] }
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

    let iteri f arr =
        for i = 0 to arr.base1 - 1 do
              for j = 0 to arr.base2 - 1 do
                f i j arr.[i,j]

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
    type State = { Blocks : (bool * Color option) Array2D.Array2D; Points : int }

    [<Struct>]
    type KeyPressed = { Up : bool; Down : bool; Left : bool; Right : bool }
        with member k.Any () = k.Up || k.Down || k.Left || k.Right
    let (|IsLeftPressed|IsRightPressed|IsUpPressed|IsDownPressed|Nothing|) (kp:KeyPressed) =
        if kp.Up then IsUpPressed
        elif kp.Down then IsDownPressed
        elif kp.Left then IsLeftPressed
        elif kp.Right then IsRightPressed
        else Nothing

    let empty = false, None

    let initState = { Blocks = Array2D.create BoardSize.width BoardSize.height empty; Points = 0 }


    let score (state:State) : State =
        let width = state.Blocks.Length1
        let height = state.Blocks.Length2
        let result = Array2D.init width height (fun _ _ -> false, None)
        let mutable moveUp = 0
        for y = height - 1 downto 0 do
            let fullRow = 
                seq { for x = width - 1 downto 0 do
                        let v, c = state.Blocks.[x,y]
                        yield (not v) && Option.isSome c } |> Seq.forall id
            if fullRow then moveUp <- moveUp + 1
            if not fullRow then
                let y' = y + moveUp
                printfn "%A" y'
                if y' >= 0 then
                    for x = width - 1 downto 0 do
                        Array2D.set result x y' (state.Blocks.[x, y]) 
        { state with Blocks = result; Points = state.Points + moveUp }

    let canApplyKeyboardTransition (keyPressed:KeyPressed) (state:State) =
        let width = state.Blocks.Length1
        let height = state.Blocks.Length2
        if not <| keyPressed.Any () then true
        else
            let isValidMove x y = 
                match keyPressed with
                | IsLeftPressed -> x > 0 && (fst state.Blocks.[x-1,y] || Option.isNone (snd state.Blocks.[x-1,y]))
                | IsRightPressed -> x < width - 1 && (fst state.Blocks.[x+1,y] || Option.isNone (snd state.Blocks.[x+1,y]))
                | IsDownPressed -> y < height - 1 && (fst state.Blocks.[x,y+1] || Option.isNone (snd state.Blocks.[x,y+1]))
                | _ -> true
            state.Blocks 
            |> Array2D.toSeq
            |> Seq.filter (fun (_,_,(v,_))->v)
            |> Seq.forall (fun (x,y,_) -> isValidMove x y)



    let activeBlockSpace (state:State) =
        let min a b = (match a with None -> b | Some a -> min a b) |> Some
        let max a b = (match a with None -> b | Some a -> max a b) |> Some
        state.Blocks 
        |> Array2D.toSeq
        |> Seq.filter (fun (_,_, (v,_)) -> v)
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
        |> Seq.exists (fun (x,y, (v,c)) -> x >= minX' && x <= maxX' && y >= minY' && y <= maxY' && (not v && Option.isSome c))
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
                            let ((v,c)) = state.Blocks.[x',y']

                            if v then 
                                match keyPressed with
                                | IsLeftPressed when x = width - 1 -> empty
                                | IsRightPressed when x = 0 -> empty
                                | IsDownPressed when y = 0 -> empty
                                | _ -> (v,c) 
                                
                            else 
                                let ( (v,c)) = state.Blocks.[x,y]
                                if v then empty
                                else (v,c))
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
                let ((v,c)) = state.Blocks.[x,y+1]
                v || (Option.isNone c && not v)

        state.Blocks 
        |> Array2D.toSeq
        |> Seq.filter (fun (_,_,(v,_))->v)
        |> Seq.forall (fun  (x,y,_) -> 
            isValidMove x y)

    let move (state:State) =
        if canMove state then
            { state with Blocks = state.Blocks |> Array2D.mapi 
                                                    (fun x y (v, c) ->
                                                        if y = 0 then empty
                                                        else
                                                            let (v', c') = state.Blocks.[x,y-1]
                                                            if v' then v', c'
                                                            elif v then empty
                                                            else v, c) }
        else { state with Blocks = state.Blocks |> Array2D.map (fun  (_, c)-> (false, c)) }

    let blocks : string array array = 
                  [| [| "X"
                        "X"
                        "XX"|];

                     [| "XX"
                        "X"
                        "X"|];


                    [|  " X ";
                        "XXX"|];

                    [| "XXXX"|];

                    [|"XX"
                      "XX"|];
                      
                    [|"  X"
                      " X" |] |]
              
    let getRandomBlock (rnd:Random) =
            let str = blocks.[rnd.Next blocks.Length]
            let c = rnd.Next 255
            Array2D.init 3 4 (fun x y-> if str.Length > x && str.[x].Length > y && str.[x].[y] <> ' ' then Some c else None)


    let applyTransition (rnd:Random) (state:State) =
        let blocks = state.Blocks

        if blocks |> Array2D.exists (fun ( (x,_))-> x) |> not then
            let nextBlock = getRandomBlock rnd
            let middle = (blocks.Length1) / 2
            let color = rnd.Next 255

            Array2D.iteri (fun x y (v:Color option) -> 
                if Option.isSome v then Array2D.set blocks (x+middle-1) y (true, v)) nextBlock
        { state with Blocks = blocks}

