open System
open ProjectParser
open ProjectInterpreter
open LibraryMethods

[<EntryPoint>]
let main argv =
    // Convert file to string representation
    let sraw = System.IO.File.ReadAllText(argv.[0])
    let slength = String.length sraw
    
    // Vim and emacs create files differently. Depending on which text editor the user uses to create the .crall file
    // either the last character must be deleted, or the last two must be deleted.
    let e =
        if (parse sraw.[0..(slength-1)] = None) then  // Potentially emacs case
            if (parse sraw.[0..(slength-2)] = None) then  // If vim case also fails
                failwith "Invalid Program."
            else
                parse sraw.[0..(slength-2)]  // vim case
        else
            parse sraw.[0..(slength-1)]  // emacs case

    // Check to see if dimension: is the first line of code. If it is, then evaluate the expression and 
    // begin game play. If not, code is invalid, and the program must be exited
    let dimStr = sraw.[0..8]
    if (dimStr = "dimension") then
        let map =
            match e with
            | Some e -> eval e Map.empty
            | None ->
                failwith "Invalid Program."
        let arr_str = MapToString map
        printfn "%s" arr_str

        let curr_pos = find_pos map
        let play = PlayMap map curr_pos
        0 //return integer exit code
    else
        printfn "!ERROR!: Code must start by defining dimensions"
        0 // return an integer exit code
