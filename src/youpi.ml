open Blacs
 
type test = Write | Read | Time

exception Test_failure
  
exception Connection_failure

let pi = acos (-. 1.)

let print str = print_string str; flush_all ()

let iters = 100

(** Normal Gauss distribution **)
let g = fun mu sigma x ->
  let sigma2 = sigma ** 2. in
  (1.  /.  sigma *. sqrt( 2. *. pi) *.
   exp (-. ((x -. mu) ** 2. ) /.
           (2. *. sigma2)        ))

let get_iterations iterations f =
   let rec aux i acc =
    match i with
    | 0 -> acc
    | _ ->
      try
        aux (i-1) ((f ())::acc)
      with
        Test_failure         -> []
      | Connection_failure   -> aux (i-1) acc
   in
   aux iterations []

let normal_distribution l s =
  let mu        = (List.fold_left ( +. ) 0. l)                     in
  let sigma     =
    (List.fold_left (fun x y -> ((x -. mu) ** 2.) +. y) 0. l) /. s in
  let g = g mu sigma in
  List.map (fun x -> (g x, x)) l

let max_elt l =
   List.fold_left (fun max x -> Pervasives.max max (Some (fst x))) None l
    
let bench_function iterations f  =
  let time_list = get_iterations iterations f            in
  let measures  = float_of_int (iterations)              in
  let nd        = normal_distribution time_list measures in
  let max       = max_elt nd                             in 
  match max with
    Some x -> List.assoc x nd
  | None   -> (- 1.0)

let run_bench =  List.iter (fun f -> (Printf.printf "%f, " (bench_function iters f)))

let directory_listing dirname =
  let l = Array.to_list (Sys.readdir dirname) in
  let p = fun x -> ".rq" = Filename.extension x in
  List.filter p l

let json path =
  Yojson.Safe.from_file path

let test_basename ty name = 
  let dir = match ty with
      Read  -> "requests/read/"
    | Time  -> "requests/time/"
    | Write -> "requests/write/"
  in
  dir ^ name
  
let http_body ty name = match ty with
    Time -> None
  | _    ->  
    let path  = (test_basename ty name) in
    Some (Yojson.Safe.to_string (json path))

let expected_response ty name =
  let name = Filename.remove_extension name in
  let path = (test_basename ty name) ^ "-response.json" in
  if Sys.file_exists path then
    Some (json path)
  else
    None

let test_service ty test =
  let body = http_body ty test in
  let service = match ty,body with
      Time,  None   -> get_time
    | Read,  Some b -> read_and_hash b
    | Write, Some b -> write b
    | _ -> assert false 
  in
  let code,time,response = service () in
  if 200 = code then
    time, response
  else
    raise Connection_failure

let make_test ty test expected_response () =
  let time,response = test_service ty test in
  match expected_response with
    Some (re) ->
    if (re =  (Yojson.Safe.from_string response))
    then time else raise Test_failure
  | None -> time

let write_test test_name =
  let expected_response = expected_response Write test_name in
  make_test Write test_name expected_response 
  
let read_test test_name  =
  let expected_response = expected_response Read test_name in
  make_test Read test_name expected_response
    
let time_test test_name  =
  let expected_response = expected_response Time test_name in
  make_test Time test_name expected_response 

let write_requests = directory_listing "requests/write"
    
let read_requests  = directory_listing "requests/read"

let time_requests  = directory_listing "requests/time"


  
let make_blacs_test ty name  = match ty with
    Write -> write_test name
  | Read  -> read_test  name
  | Time  -> time_test  name

let random_write () =
  let wrq = RandomSheet.(wrq 100 100 (generate 100 100 ())) in
  let body = WriteRequest.write_request_to_json wrq in
  let service = write body in
  let code,time,response = service () in
  if 200 = code then
    time
  else
    raise Connection_failure

let main () =
  let make_write_test = make_blacs_test Write in
  let make_read_test  = make_blacs_test Read  in
  let make_time_test  = make_blacs_test Time  in
  let write_tests     = List.map make_write_test write_requests in
  let read_tests      = List.map make_read_test  read_requests  in
  let time_tests      = List.map make_time_test  time_requests  in
  Printf.printf "%s, " Blacs.implementation;
  run_bench write_tests;
  run_bench read_tests;
  run_bench time_tests;
  run_bench [random_write];
  flush_all ()


let () = main ()


(* let run cmd = *)
(*   let status = Sys.command cmd in *)
(*   if status != 0 then *)
(*     print_endline (cmd ^ " failed with status " ^ (string_of_int status)) *)

(* let eval_test name () = *)
(*   let cmd = *)
(*     Printf.sprintf "docker run --rm --name  %s-%s %s %s test alice bob" *)
(*       name Blacs.implementation name Blacs.host *)
(*   in *)
(*   let t_0 = Unix.gettimeofday () in *)
(*   (for i=1 to 5 do *)
(*     run cmd; *)
(*   done); *)
(*   let t_1 = Unix.gettimeofday () in *)
(*   (t_1 -. t_0) /. 5. *)
