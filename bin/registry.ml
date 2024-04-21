open Core
open Cassis
open Stdint

let logdb, _ =
  let open Lmdb in
  Core_unix.mkdir_p "db";
  let env = Env.(create Rw ~max_maps:2) "db" in
  let log =
    Map.create Nodup ~key:Conv.int64_le ~value:Operation.conv ~name:"operations" env
  and state = Map.create Nodup ~key:Conv.int64_le ~value:State.Line.conv in
  log, state
;;

let lastlogkey =
  let open Lmdb in
  try
    Cursor.go Ro logdb (fun cursor ->
      let key, _ = Cursor.last cursor in
      key)
  with
  | Not_found -> 0L
;;

let serial = ref lastlogkey

let sec =
  (match Sys.getenv "SECRET_KEY" with
   | Some sec -> `Hex sec
   | None -> `Hex "0000000000000000000000000000000000000000000000000000000000000001")
  |> Hex.to_bytes
  |> Bip340.load_secret
;;

let pub = Bip340.public_key sec
let state = ref (State.init ())
let recently_accepted = ref (Array.create ~len:0 Operation.Unknown)
let seconds_threshold = Uint32.of_int 30

let () =
  Printf.printf "registry pubkey: %s\n%!" (Hex.of_bytes pub |> Hex.show);
  Dream.run ~interface:"0.0.0.0" ~port:3002
  @@ Dream.router
       [ Dream.get "/" (fun _ -> Dream.respond {|cassis registry|})
       ; Dream.post "/append" (fun req ->
           let%lwt body = Dream.body req in
           let op = Operation.of_json_string body in
           (* lock mutex, only one operation in flight at each point *)
           State.lock ();
           (* check if this wasn't already sent here in the past past_seconds*2 seconds *)
           let dupe =
             Array.find !recently_accepted ~f:(Operation.equal op) |> Option.is_some
           in
           if dupe
           then (
             State.unlock ();
             Dream.respond ~code:400 "{\"status\":\"failed\"}")
           else (
             (* check if this is not too old and not too in the future *)
             let timely =
               let ts = Operation.ts op in
               let now = Core_unix.time () |> Float.to_int64 |> Uint32.of_int64 in
               Poly.(
                 Uint32.(ts - seconds_threshold) < now
                 && Uint32.(ts + seconds_threshold) > now)
             in
             if not timely
             then (
               State.unlock ();
               Dream.respond ~code:400 "{\"status\":\"failed\"}")
             else (
               (* check validity of operation and prepare operations to be applied *)
               let valid, apply_steps = State.prepare !state op in
               if not valid
               then (
                 State.unlock ();
                 Dream.respond ~code:400 "{\"status\":\"failed\"}")
               else (
                 (* if all is ok, apply operations in memory *)
                 List.iter apply_steps ~f:(fun apply -> apply state);
                 (* then save on database *)
                 Lmdb.Map.set logdb !serial op;
                 serial := Int64.( + ) !serial 1L;
                 (* store this here for a while to prevent double-runs *)
                 let now = Core_unix.time () |> Float.to_int64 |> Uint32.of_int64 in
                 recently_accepted
                 := Array.append
                      (!recently_accepted
                       |> Array.filter ~f:(fun op ->
                         Poly.(Operation.ts op > Uint32.(now - seconds_threshold))))
                      (Array.create ~len:1 op);
                 (* unlock mutex *)
                 State.unlock ();
                 Dream.respond "{\"status\":\"ok\"}"))))
       ; Dream.get "/op/:id" (fun req ->
           let id = Dream.param req "id" |> Int64.of_string in
           let data = Lmdb.Map.get logdb id in
           let data_json = Operation.to_json_string data in
           Dream.respond data_json)
       ; Dream.get "/log" (fun _ ->
           let _ =
             Lmdb.Cursor.go Ro logdb (fun cursor ->
               Lmdb.Cursor.fold_left
                 ~cursor
                 ~f:(fun _ _ _ -> 0L)
                   (* initial ^ Int64.to_string key ^ ": " ^ value ^ "\n")*)
                 0L
                 logdb)
           in
           (*Printf.printf "%s" (Int64.to_string data);*)
           Dream.respond "nada")
       ]
;;
