module LibraryMethods
open System
open ProjectInterpreter

//Converts the map data structure into a 2d array of chars for easier printing
let MapToArr map = 
    let getdim = Map.find ((-1,-1)) (map)
    let dim = 
        match getdim with
        | Dimensions (a,b) -> (a,b)
        | _ -> failwith "Dimensions not set"
    let arr = Array2D.create (snd dim) (fst dim) ('·')

    //Recursive helper function used to loop through all positions of the array being built
    let rec MapToArrHelp map arr coord dim =
        let rec nextcall map2 arr2 coord2 dim2 = 
            if (((fst (coord)) = (fst (dim)-1)) && ((snd (coord)) = (snd (dim)-1))) then
                arr
            elif ((fst (coord)) < (fst (dim)-1)) then
                let newx = (fst (coord)) + 1
                let newy = snd (coord)
                MapToArrHelp map arr (newx, newy) dim
            else
                MapToArrHelp map arr (0, (snd coord)+1) dim

        if (not (Map.containsKey coord map)) then
            nextcall map arr coord dim
        else
            let mapObj = Map.find coord map
            match mapObj with
            | Player(_) -> 
                          Array2D.set (arr) (snd coord) (fst coord) ('ö')
                          nextcall map arr coord dim
            | Wall -> 
                     Array2D.set (arr) (snd coord) (fst coord) ('█')
                     nextcall map arr coord dim
            | Goal -> 
                     Array2D.set (arr) (snd coord) (fst coord) ('G')
                     nextcall map arr coord dim
            | Trap(_) -> 
                        Array2D.set (arr) (snd coord) (fst coord) ('?')
                        nextcall map arr coord dim
            | Teleporter(_) ->
                        Array2D.set (arr) (snd coord) (fst coord) ('T')
                        nextcall map arr coord dim
            | _ -> failwith "Error with Map. Dimension found within map boundaries."
    MapToArrHelp map arr (0,0) dim

//Prints a nice looking map from the 2d array of chars
let arr_prettyprint (arr: char [,]) =
    let max_vert = Array2D.length1 arr
    let max_horiz = Array2D.length2 arr

    //Adds a nice looking border to the top of the array
    let rec add_topborder curr_pos max_horiz =
        let top_border = 
            if (curr_pos = 0) then
                "╔"
            elif (curr_pos = max_horiz+1) then
                "╗"
            else
                "═"
        if (curr_pos <= max_horiz) then
            top_border + add_topborder (curr_pos+1) (max_horiz)
        else
            top_border

    //Adds a nice looking border to the bottom of the array
    let rec add_botborder curr_pos max_horiz =
        let bot_border =
            if (curr_pos = 0) then
                "╚"
            elif (curr_pos = max_horiz+1) then
                "╝"
            else
                "═"
        if (curr_pos <= max_horiz) then
            bot_border + add_botborder (curr_pos+1) (max_horiz)
        else
            bot_border

    //Recursively loops through array adding contents to the string
    let rec helper_print (curr_dim:int) (max_dim:int) (arr: char[,]) =
        let stringrep = Array.fold (fun (str:string) (el:char) -> str + el.ToString()) "" arr.[curr_dim,0..]
        let with_borders = "║" + stringrep + "║" //Takes the contents of a row and adds borders to the sides
        if (curr_dim < max_dim-1) then
            with_borders + "\n" + helper_print (curr_dim+1) (max_dim) (arr)
        else
            with_borders
    (add_topborder 0 max_horiz) + "\n" + (helper_print 0 max_vert arr)  + "\n" + (add_botborder 0 max_horiz)

let match_item key value = 
    match value with
    | Player (x) -> true
    | _ -> false

let find_pos map =
    try
        Map.findKey match_item map
    with
    | _ ->
          printfn "!ERROR!: Player not found on the map"
          exit 1 //Ends the program if a starting position was not added to the map

let MapToString map = 
    let arr = MapToArr map
    arr_prettyprint arr

//Initiate playing the designed map
let rec PlayMap map current_pos : Map<(int * int),MapObject> =
    let input = Console.ReadLine()
    let player_obj : MapObject = Map.find current_pos map
    let curr_health =
        try
            match player_obj with
            | Player (x) -> x
            | _ -> failwith "Error cannot find player"
        with
        | Failure msg ->
                        printfn "%s" msg
                        0
    //Check to see what the player lands on and respond accordingly    
    let MatchMap map current_pos new_pos curr_health: Map<(int * int),MapObject> =
        let occup: MapObject = Map.find (new_pos) (map)
        try
            match occup with
            | Wall ->
                     let map_str = MapToString map
                     printfn "%s" map_str
                     printfn "Current health: %d" curr_health
                     PlayMap map current_pos
            | Goal ->
                     let map_1 = Map.remove new_pos map
                     let map_2 = Map.remove current_pos map_1
                     let map_fin = Map.add new_pos (Player(curr_health)) map_2
                     let map_str = MapToString map_fin
                     printfn "%s" map_str
                     printfn "Level Completed"
                     map_fin
            | Trap (dam) ->
                           let map_1 = Map.remove new_pos map
                           let map_2 = Map.remove current_pos map_1
                           if (curr_health - dam < 1) then
                               let map_fin = Map.add new_pos (Player (0)) map_2
                               let map_str = MapToString map_fin
                               printfn "%s" map_str
                               printfn "Level failed. You died"
                               map_fin
                           else
                               let new_health = curr_health - dam
                               let map_fin = Map.add new_pos (Player (new_health)) map_2
                               let map_str = MapToString map_fin
                               printfn "%s" map_str
                               printfn "You fell on a trap! -%d health" dam
                               printfn "Current health: %d" new_health
                               PlayMap map_fin new_pos
            | Teleporter (x,y) ->
                                let map_1 = Map.remove current_pos map
                                if (Map.containsKey (x,y) map) then
                                    let object_occup = Map.find (x,y) map
                                    match object_occup with
                                    | Trap (dam) ->
                                        let map_2 = Map.remove (x,y) map_1
                                        if (curr_health - dam < 1) then
                                            let map_fin = Map.add new_pos (Player (0)) map_2
                                            let map_str = MapToString map_fin
                                            printfn "%s" map_str
                                            printfn "Level failed. You died"
                                            map_fin
                                        else
                                            let new_health = curr_health - dam
                                            let map_fin = Map.add new_pos (Player (new_health)) map_2
                                            let map_str = MapToString map_fin
                                            printfn "%s" map_str
                                            printfn "You fell on a trap! -%d health" dam
                                            printfn "Current health: %d" new_health
                                            PlayMap map_fin new_pos
                                    | Goal ->
                                        let map_2 = Map.remove (x,y) map_1
                                        let map_fin = Map.add (x,y) (Player (curr_health)) map_2
                                        let map_str = MapToString map_fin
                                        printfn "%s" map_str
                                        printfn "You teleported to the goal! Level Completed!"
                                        map_fin
                                    | Wall ->
                                        printfn "A wall is blocking the destination of the teleporter."
                                        let map_str = MapToString map
                                        printfn "%s" map_str
                                        PlayMap map current_pos
                                    | _ -> failwith "Issue with map object. Teleporter transports you to object that is neither a trap nor a goal. Please reconstruct the map."
                                else
                                    let map_fin = Map.add (x,y) (Player (curr_health)) map_1
                                    let map_str = MapToString map_fin
                                    printfn "%s" map_str
                                    printfn "Current health: %d" curr_health
                                    PlayMap map_fin (x,y)
            | _ -> failwith "Issue with map object. Player has encountered an object that is not a wall, goal, or trap. Please reconstruct the map"
        with
        | Failure msg ->
                        printfn "%s" msg
                        map
        | _ -> map
        
    //Take input form user to choose where to move                
    match input with 
    | "w" ->
             let up_pos = ((fst current_pos), ((snd current_pos)-1))
             if (Map.containsKey up_pos  map) then
                 MatchMap map current_pos up_pos curr_health
             else
                 let map_1 = Map.remove current_pos map
                 let map_fin = Map.add up_pos (Player (curr_health)) map_1
                 let map_str = MapToString map_fin
                 printfn "%s" map_str
                 printfn "Current health: %d" curr_health
                 PlayMap map_fin up_pos
    | "s" -> 
               let down_pos = ((fst current_pos), ((snd current_pos)+1))
               if (Map.containsKey down_pos map) then
                   MatchMap map current_pos down_pos curr_health
               else
                   let map_1 = Map.remove current_pos map
                   let map_fin = Map.add down_pos (Player (curr_health)) map_1
                   let map_str = MapToString map_fin
                   printfn "%s" map_str
                   printfn "Current health: %d" curr_health
                   PlayMap map_fin down_pos
    | "a" -> 
               let left_pos = (((fst current_pos)-1), (snd current_pos))
               if (Map.containsKey left_pos map) then
                   MatchMap map current_pos left_pos curr_health
               else
                   let map_1 = Map.remove current_pos map
                   let map_fin = Map.add left_pos (Player (curr_health)) map_1
                   let map_str = MapToString map_fin
                   printfn "%s" map_str
                   printfn "Current health: %d" curr_health
                   PlayMap map_fin left_pos
    | "d" -> 
               let right_pos = (((fst current_pos)+1), (snd current_pos))
               if (Map.containsKey right_pos map) then
                   MatchMap map current_pos right_pos curr_health
               else
                   let map_1 = Map.remove current_pos map
                   let map_fin = Map.add right_pos (Player (curr_health)) map_1
                   let map_str = MapToString map_fin
                   printfn "%s" map_str
                   printfn "Current health: %d" curr_health
                   PlayMap map_fin right_pos
    | "quit" ->
               printfn "Quitting game...Thanks for Playing"
               map
    | _ -> 
          printfn "Please enter w/a/s/d, or if you would like to quit, please enter 'quit'"
          PlayMap map current_pos

