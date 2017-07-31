open Yojson.Safe

let args           = Sys.argv

let sheet          = "noname"

let host           = Array.get args 1

let implementation = Array.get args 2

exception Hash_failure of string

let get_size () = Io.size_service host sheet 

let get_time () = Clock.time_service host sheet 

let get_hash hash () = Io.hash_service host sheet hash 

let read_and_hash rr () =
  let open ReadPromise in
  let code,time,json = Io.read_service host sheet rr in
  if code = 200 then
    let rp  = ReadPromise.read_promise_of_json json in
    match rp with
      Result.Ok r -> 
      let t =
        (if r.date > (Unix.gettimeofday ())
         then
           let s = r.date -. (Unix.gettimeofday ()) +. 0.005 in
           Unix.sleepf s;
           s
         else 0.0)
      in
      let c,t',lcl = get_hash r.hash () in
      c,(time +. t +. t'),lcl
    | Result.Error e -> failwith e
  else
    raise (Hash_failure(json))

let write wr () = Io.write_service host sheet wr
