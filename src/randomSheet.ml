let init = Random.self_init ()
    
let ratio = 100
  
let max   = 1024

let int () = Random.int max 
  
let random_int () =
  let i   = int () in
  let def = Definition.int i in
  Cell.cell ~v:i def



let random_count row col =
  let origin_row = Random.int row
  and origin_col = Random.int col
  and width     = 1 + Random.int (pred row)
  and length    = 1 + Random.int (pred col)
  and v         = int ()         in
  let def = Definition.count origin_col origin_row length width v in
  Cell.cell def

let random_cell row col =
  if Random.int ratio = 0 then
    random_count row col
  else
    random_int ()

let generate row col () =
  let len = row * col in
  let rec aux i acc =
    if i = 0 then
      acc
    else
      let c =random_cell row col in
      aux (pred i) (c::acc)
  in
  aux len []

let l =  generate 10000 10000 ()

let wrq l w cl = WriteRequest.write_request
    ~tag:"alice" ~time:Nativeint.zero
    ~origin:(Coordinates.coords 0 0)
    ~length:l ~width:w ~cells:cl

(* let rrq = ReadRequest.readrq "alice" Nativeint.zero (Coordinates.coords 0 0)  *)

(* let () = print_endline (Yojson.Safe.to_string (BlacsTypes.writeRQ_to_yojson wrq)) *)

(* let () = print_endline (Yojson.Safe.to_string (BlacsTypes.readRQ_to_yojson rrq)) *)

(* let () = flush_all () *)
  
