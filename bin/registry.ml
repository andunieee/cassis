open Core
open Cassis

let log, _ =
  let open Lmdb in
  Core_unix.mkdir_p "db";
  let env = Env.(create Rw ~max_maps:2) "db" in
  let log =
    Map.create Nodup ~key:Conv.int64_le ~value:Operation.conv ~name:"operations"
      env
  and state = Map.create Nodup ~key:Conv.int64_le ~value:State.Line.conv in
  (log, state)

let lastlogkey =
  let open Lmdb in
  try
    Cursor.go Ro log (fun cursor ->
        let key, _ = Cursor.last cursor in
        key)
  with Not_found -> 0L

let serial = ref lastlogkey
let sec = Sys.getenv_exn "SECRET_KEY"
let _ = `Hex sec |> Hex.to_bytes |> Bip340.load_secret

let () =
  Dream.run ~interface:"0.0.0.0" ~port:3002
  @@ Dream.router
       [
         Dream.get "/" (fun _ -> Dream.respond {|cassis registry|});
         Dream.post "/append" (fun _ ->
             (*let%lwt body = Dream.body req in*)
             Lmdb.Map.set log !serial Unknown;
             serial := Int64.( + ) !serial 1L;
             Dream.respond "ok\n");
         Dream.get "/op/:id" (fun req ->
             let id = Dream.param req "id" |> Int64.of_string in
             let data = Lmdb.Map.get log id in
             let data_json = Operation.to_json_string data in
             Dream.respond data_json);
         Dream.get "/log" (fun _ ->
             let _ =
               Lmdb.Cursor.go Ro log (fun cursor ->
                   Lmdb.Cursor.fold_left ~cursor
                     ~f:(fun _ _ _ -> 0L)
                       (* initial ^ Int64.to_string key ^ ": " ^ value ^ "\n")*)
                     0L log)
             in
             (*Printf.printf "%s" (Int64.to_string data);*)
             Dream.respond "nada");
       ]
