open Core
open Cassis

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
let state = State.init ()

let () =
  Printf.printf "registry pubkey: %s\n%!" (Hex.of_bytes pub |> Hex.show);
  Dream.run ~interface:"0.0.0.0" ~port:3002
  @@ Dream.router
       [ Dream.get "/" (fun _ -> Dream.respond {|cassis registry|})
       ; Dream.post "/append" (fun req ->
           let%lwt body = Dream.body req in
           let op = Operation.of_json_string body in
           if not (Operation.validate op)
           then Dream.respond ~code:400 "{\"status\":\"failed\"}"
           else (
             State.lock ();
             let applied = State.inplace_validate_and_apply state op in
             if not applied
             then (
               State.unlock ();
               Dream.respond ~code:400 "{\"status\":\"failed\"}")
             else (
               Lmdb.Map.set logdb !serial op;
               serial := Int64.( + ) !serial 1L;
               State.unlock ();
               Dream.respond "{\"status\":\"ok\"}")))
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
