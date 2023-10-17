module Cache = Scache.Cache

type action = Read | Write | Remove

type bench = {
  mutable read : int;
  mutable write: int;
  mutable remove: int;
  mutable read_time: float;
  mutable write_time: float;
  mutable remove_time: float;
}

let timing f =
  let t = Unix.gettimeofday () in
  let r = f () in
  let time = (Unix.gettimeofday ()) -. t in
  (time, r)


let () = Scache.remove_cache ~name:"benchmarking"
let cache = Cache.start ~name:"benchmarking"

let rec read = function
  | [] -> []
  | (key,_)::tl -> (Cache.get cache ~key)::(read tl)

let write data =
  List.map (fun (key,value) -> let _ = Cache.set cache ~key ~value in None) @@ data

let remove data =
  List.map (fun (key,_) -> let _ = Cache.remove cache ~key in None) @@ data

let update bench n_times time = function
  | Read ->
    (bench.read <- bench.read + n_times;
    bench.read_time <- bench.read_time +. time)
  | Write ->
    (bench.write <- bench.write + n_times;
    bench.write_time <- bench.write_time +. time)
  | Remove ->
    (bench.remove <- bench.remove + n_times;
    bench.remove_time <- bench.remove_time +. time)

let operation bench n_times data action =
  let (time, _) =
    timing (fun () -> match action with
      | Read -> read data
      | Write -> write data
      | Remove -> remove data
    )
  in
  update bench n_times time action

let print_result bench =
  let b = bench in
  Fmt.pr "read: %d@.write: %d@.remove: %d@.read time: %f@.write time: %f@.remove time: %f@."
    b.read b.write b.remove b.read_time b.write_time b.remove_time

let () =
  Fmt.pr "Benchmarking...\n";
  let bench = {
    read = 0;
    write = 0;
    remove = 0;
    read_time = 0.0;
    write_time = 0.0;
    remove_time = 0.0;
  } in
  let n_times = 100_000 in
  let data = List.init n_times (fun i -> "key" ^ (string_of_int i), "same value") in
  let run = operation bench n_times data in
  run Write;
  run Read;
  run Remove;
  run Write;
  run Read;
  run Remove;
  print_result bench

