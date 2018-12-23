module ProjectInterpreter
open System
open ProjectParser

// MapObject type to add in the Map
type MapObject =
| Player of int
| Wall
| Trap of int
| Goal
| Teleporter of int * int
| Dimensions of int * int

// Checks that the coordinate dimensions are within the Map dimensions. Fails if Dimensions are not specified in the code
let checkDim (map: Map<(int * int),MapObject>) (coord: int *int) =
    let dims = Map.find ((-1,-1)) (map)
    try
        match dims with
        | Dimensions (a,b) ->
            ((fst (coord) < a) && (fst (coord) >= 0) && (snd (coord) < b) && (snd (coord) >= 0))
        | _ -> failwith "!ERROR!: Dimensions not specified"
    with
    | Failure msg ->
                    printfn "%s" msg
                    false
    | _ -> false

// Returns true if the map contains an object in the coordinate
let isNotClear map coord =
    Map.containsKey coord map

// Function to be used in setStart function. Returns true if the value is a Player.
let isPlayer key value =
    match value with
    | Player(i) -> true
    | _ -> false
    
// Function to evaluate an expression. Takes in an expression and a map. Returns a map
let rec eval e map =

    // Recursively adds walls along a horizontal path in the map
    let rec longWallX (map: Map<(int * int),MapObject>) corr x =
        try
            if (isNotClear map corr) then  // If the coordinate already contains an object, print error message and do not add the wall
                failwith "!ERROR!: Long wall collides with some object"
            else   // If the coordinate is empty, add the wall
                if((fst corr) = x) then
                    Map.add (corr) (Wall) (map)
                else
                    match corr with
                    | (x1,y1) ->
                        let newMap = Map.add (x1,y1) (Wall) (map)
                        longWallX newMap ((x1+1),y1) x
        with
        | Failure msg ->
                        printfn "%s" msg
                        map
        | _ -> map
    //Recursively adds walls along a vertical path in the map        
    let rec longWallY (map: Map<(int * int), MapObject>) corr y =
        try
            if (isNotClear map corr) then
                failwith "!ERROR!: Long wall collides with some object"
            else
                if((snd corr) = y) then
                    Map.add (corr) (Wall) (map)
                else
                    match corr with
                    | (x1,y1) ->
                        let newMap = Map.add (x1,y1) (Wall) (map)
                        longWallY newMap (x1,(y1+1)) y
        with
        | Failure msg ->
                        printfn "%s" msg
                        map
        | _ -> map
        
    //Match what operation is used and add to the map accordingly, also checking for errors
    try
        match e with
        | SeqOp (e1,e2) ->
            let map1 = eval e1 map
            eval e2 map1
        | SetDim (r, c) ->
            if (r < 1 || c < 1) then
                failwith "!ERROR!: Invalid dimensions" //Do not set dimensions if they are already set or if either is less than 1
            elif (Map.containsKey (-1,-1) (map)) then
                printfn "!ERROR!: Dimensions already set"
                map
            else
                let map1 = Map.add (-1,-1) (Dimensions (r, c)) (map)
                let map_top = longWallX map1 (0, -1) (r-1)
                let map_bot = longWallX map_top (0, c) (r-1)
                let map_side1 = longWallY map_bot (-1, 0) (c-1)
                longWallY map_side1 (r, 0) (c-1)
        | SetGoal (x,y) ->
            if (Map.exists (fun key value -> value = Goal) map) then //Do not set goal if it is already set, if an object already exists in that space, or if given an invalid coordinate
                printfn "!ERROR!: Goal already set."
                map
            elif (isNotClear map (x,y)) then
                failwith "!ERROR!: Goal set over another object"
            elif (checkDim (map) (x,y)) then
                Map.add (x,y) (Goal) (map)
            else
                failwith "!ERROR!: Invalid Goal coordinate"
        | SetStart (x,y) ->
            if (Map.exists (isPlayer) (map)) then //Do not set start if it is already set, if there is already an object there, or if given an invalid coordinate
                failwith "!ERROR!: Start already set."
            elif (isNotClear map (x,y)) then
                failwith "!ERROR!: Start set over another object"
            elif (checkDim (map) (x, y)) then
                Map.add (x, y) (Player (100)) (map)
            else
                failwith "!ERROR!: Invalid Start dimensions"
        | SetWall (x,y) ->
            if (isNotClear map (x,y)) then
                failwith "!ERROR!: Wall set over another object" //Do not set wall if an object is there already or if given invalid coordinates
            elif (checkDim (map) (x, y)) then
                Map.add (x, y) (Wall) (map)
            else
                failwith "!ERROR!: Invalid Wall dimensions"
        | SetLongWall ((x1,y1),(x2,y2)) ->
            if ((not (checkDim map (x1,y1))) || (not (checkDim map (x2,y2)))) then
                failwith "!ERROR!: LongWall dimensions invalid"
            else
                if (x1 = x2) then
                    let ymin = min y1 y2
                    let ymax = max y1 y2
                    longWallY map (x1,ymin) ymax
                elif (y1 = y2) then
                    let xmin = min x1 x2
                    let xmax = max x1 x2
                    longWallX map (xmin,y1) xmax
                else
                    failwith "!ERROR!: Invalid Coordinates"
        | SetTrap (x,y,z) ->
            if (isNotClear map (x,y)) then
                failwith "!ERROR!: Trap placed over another object" //Do not place trap if an object already exists or if given invalid coordinates
            elif (not (checkDim (map) (x,y))) then
                failwith "!ERROR!: Trap coordinates invalid"
            else
                Map.add (x, y) (Trap (z)) (map)
        | SetTeleporter ((x1,y1),(x2,y2)) ->
            if ((not (checkDim map (x1,y1))) || (not (checkDim map (x2,y2)))) then
                failwith "!ERROR!: Teleporter coordinates not within dimensions of map." //Do not place teleporter if an object already exists there or if the coordinates are invalid
            elif (isNotClear map (x1,y1)) then
                failwith "!ERROR!: Teleporter cannot be placed on top of other object."
            else
                Map.add (x1, y1) (Teleporter (x2,y2)) (map)                   
    with
    |Failure msg ->
                   printfn "%s" msg
                   map
    | _ -> map
                   

        

