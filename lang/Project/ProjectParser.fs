module ProjectParser
open System
open Parser

// Expression type for parser
type Expr =
|SetDim of int*int
|SetGoal of int*int
|SetStart of int*int
|SetWall of int*int
|SetLongWall of (int*int)*(int*int)                            
|SetTrap of int*int*int
|SetTeleporter of (int*int)*(int*int)
|SeqOp of Expr*Expr

let expr, exprImpl = recparser()

// Parser for two-digit numbers
let two_num = pseq (pdigit) (pdigit) (fun(a,b) -> (int (a.ToString() + b.ToString())))

// Parser for three-digit numbers
let three_num = pseq (two_num) (pdigit) (fun (a,b) -> (int (a.ToString() + b.ToString())))

// Parser for a one-digit coordinate
let coord = (pbetween (pchar '(') (pchar ')') (pseq (pleft (pdigit) (pchar ',')) (pdigit) (fun(a,b) -> (int (a.ToString()), int (b.ToString()))))) 

// Parser for two-digit coordinate
let two_dig_coord = (pbetween (pchar '(') (pchar ')') (pseq (pleft (two_num) (pchar ',')) (two_num) (fun(a,b) -> (int (a.ToString()), int (b.ToString())))))

// Parser for a coordinate of a two-digit and one-digit number
let two_one_coord = (pbetween (pchar '(') (pchar ')') (pseq (pleft (two_num) (pchar ',')) (pdigit) (fun(a,b) -> (int (a.ToString()), int (b.ToString())))))

// Parser for a coordinate of a one-digit and two-digit number
let one_two_coord = (pbetween (pchar '(') (pchar ')') (pseq (pleft (pdigit) (pchar ',')) (two_num) (fun(a,b) -> (int (a.ToString()), int (b.ToString())))))

// Parser for a coordinate of two three-digit numbers
let three_coord = (pbetween (pchar '(') (pchar ')') (pseq (pleft (three_num) (pchar ',')) (three_num) (fun(a,b) -> (int (a.ToString()), int (b.ToString())))))

// Parser for a coordinate of a three-digit and two-digit number
let three_two_coord = (pbetween (pchar '(') (pchar ')') (pseq (pleft (three_num) (pchar ',')) (two_num) (fun(a,b) -> (int (a.ToString()), int (b.ToString())))))

// Parser for a coordinate of a three-digit and one-digit number
let three_one_coord = (pbetween (pchar '(') (pchar ')') (pseq (pleft (three_num) (pchar ',')) (pdigit) (fun(a,b) -> (int (a.ToString()), int (b.ToString())))))

// Parser for a coordinate of a two-digit and three-digit number
let two_three_coord = (pbetween (pchar '(') (pchar ')') (pseq (pleft (two_num) (pchar ',')) (three_num) (fun(a,b) -> (int (a.ToString()), int (b.ToString())))))

// Parser for a coordinate of a one-digit and three-digit number
let one_three_coord = (pbetween (pchar '(') (pchar ')') (pseq (pleft (pdigit) (pchar ',')) (three_num) (fun(a,b) -> (int (a.ToString()), int (b.ToString())))))

// Uses all coordinate parsers until the appropriate one is found
let coord_mixer = three_coord <|> three_two_coord <|> three_one_coord <|> two_three_coord <|> two_dig_coord <|> two_one_coord <|> one_three_coord <|> one_two_coord <|> coord

// Parser for dimension function call
let setdim =
             pright (pstr "dimension: ") (pseq (three_num) (pright (pchar 'x') (three_num)) (fun(a,b) -> SetDim(int (a.ToString()), int (b.ToString()))))
            <|> pright (pstr "dimension: ") (pseq (three_num) (pright (pchar 'x') (two_num)) (fun(a,b) -> SetDim(int (a.ToString()), int (b.ToString()))))
            <|> pright (pstr "dimension: ") (pseq (three_num) (pright (pchar 'x') (pdigit)) (fun(a,b) -> SetDim(int (a.ToString()), int (b.ToString()))))
            <|> pright (pstr "dimension: ") (pseq (two_num) (pright (pchar 'x') (three_num)) (fun(a,b) -> SetDim(int (a.ToString()), int (b.ToString()))))
            <|> pright (pstr "dimension: ") (pseq (two_num) (pright (pchar 'x') (two_num)) (fun(a,b) -> SetDim(int (a.ToString()), int (b.ToString()))))
            <|> pright (pstr "dimension: ") (pseq (two_num) (pright (pchar 'x') (pdigit)) (fun(a,b) -> SetDim(int (a.ToString()), int (b.ToString())))) 
            <|> pright (pstr "dimension: ") (pseq (pdigit) (pright (pchar 'x') (three_num)) (fun(a,b) -> SetDim(int (a.ToString()), int (b.ToString()))))
            <|> pright (pstr "dimension: ") (pseq (pdigit) (pright (pchar 'x') (two_num)) (fun(a,b) -> SetDim(int (a.ToString()), int (b.ToString()))))
            <|> pright (pstr "dimension: ") (pseq (pdigit) (pright (pchar 'x') (pdigit)) (fun(a,b) -> SetDim(int (a.ToString()), int (b.ToString()))))

// Parser for goal function call
let setgoal = pseq (pstr "goal: ") (coord_mixer) (fun(a,b) -> SetGoal(b))

// Parser for start function call
let setstart =
              pseq (pstr "start: ") (coord_mixer) (fun(a,b) -> SetStart(b))

// Parser for wall function call
let setwall = 
            pseq (pstr "wall: ") (coord_mixer) (fun(a,b) -> SetWall(b))

// Parser for wall function call with two coordinates
let setlongwall = 
                pright (pstr "wall: ") (pseq (pleft (coord_mixer) (pchar ','))  coord_mixer (fun (a,b) -> SetLongWall(a,b)))

// Parser for trap function call
let settrap = 
              pright (pstr "trap: ") (pseq (coord_mixer) (pright (pchar ',') (three_num)) (fun (c,d) -> SetTrap((fst c), (snd c), int (d.ToString()))))
             <|> pright (pstr "trap: ") (pseq (coord_mixer) (pright (pchar ',') (two_num)) (fun (c,d) -> SetTrap((fst c), (snd c), int (d.ToString()))))
             <|> pright (pstr "trap: ") (pseq (coord_mixer) (pright (pchar ',') (pdigit)) (fun (c,d) -> SetTrap((fst c), (snd c), int (d.ToString()))))

// Parser for teleporter function call 
let settele = pright (pstr "teleporter: ") (pseq (pleft (coord_mixer) (pchar ','))  coord_mixer (fun (a,b) -> SetTeleporter(a,b)))

// Parser for a sequence of functions
let seqop = 
           pseq (pleft (setdim) (pchar '\n')) (expr) (fun(e1,e2) -> SeqOp(e1,e2))
          <|> pseq (pleft (setstart) (pchar '\n')) (expr) (fun(e1,e2) -> SeqOp(e1,e2))
          <|> pseq (pleft (setgoal) (pchar '\n')) (expr) (fun(e1,e2) -> SeqOp(e1,e2))
          <|> pseq (pleft (setlongwall) (pchar '\n')) (expr) (fun(e1,e2) -> SeqOp(e1,e2))
          <|> pseq (pleft (setwall) (pchar '\n')) (expr) (fun(e1,e2) -> SeqOp(e1,e2))
          <|> pseq (pleft (settrap) (pchar '\n')) (expr) (fun(e1,e2) -> SeqOp(e1,e2))
          <|> pseq (pleft (settele) (pchar '\n')) (expr) (fun(e1,e2) -> SeqOp(e1,e2))

exprImpl := seqop <|> setdim <|> setstart <|> setgoal <|> setlongwall <|> setwall <|> settrap <|> settele

let grammar = pleft expr peof

// Function to parse a string. If it succeeds, parse returns the expression representation of the string.
// Else, the function returns None
let parse input: Expr option =
    match grammar (prepare input) with
        | Success(e,_) -> Some e
        | Failure -> None
