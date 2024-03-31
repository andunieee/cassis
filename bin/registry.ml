open Core
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type operation = { typ : string } [@@deriving yojson]

let map =
  let open Lmdb in
  Core_unix.mkdir_p "db";
  let env = Env.(create Rw ~max_maps:2) "db" in
  let map =
    Map.create Nodup ~key:Conv.int64_le ~value:Conv.string ~name:"operations"
      env
  in
  map

let lastkey =
  let open Lmdb in
  try
    Cursor.go Ro map (fun cursor ->
        let key, _ = Cursor.last cursor in
        key)
  with Not_found -> 0L

let serial = ref lastkey
let sec = "2d42a2771dc2f6a16523c7a4ce7ac5e3e2288a1a68068fc5935a3bfb7206061b"
let keypair = `Hex sec |> Hex.to_bytes |> Cassis.Bip340.load_secret

let () =
  Dream.log "pubkey: %s"
    (Cassis.Bip340.public_key keypair |> Hex.of_bytes |> Hex.show);
  Dream.log "sig: %s"
    (Cassis.Bip340.sign keypair "hello" |> Hex.of_bytes |> Hex.show);
  Dream.run ~interface:"0.0.0.0" ~port:3002
  @@ Dream.router
       [
         Dream.get "/" (fun _ -> Dream.respond {|cassis registry|});
         Dream.post "/append" (fun req ->
             let%lwt body = Dream.body req in
             Lmdb.Map.set map !serial body;
             serial := Int64.( + ) !serial 1L;
             Dream.respond "ok\n");
         Dream.get "/:id" (fun req ->
             let id = Dream.param req "id" |> Int64.of_string in
             let data = Lmdb.Map.get map id in
             Dream.respond data);
         Dream.get "/log" (fun _ ->
             let _ =
               Lmdb.Cursor.go Ro map (fun cursor ->
                   Lmdb.Cursor.fold_left ~cursor
                     ~f:(fun _ _ _ -> 0L)
                       (* initial ^ Int64.to_string key ^ ": " ^ value ^ "\n")*)
                     0L map)
             in
             (*Printf.printf "%s" (Int64.to_string data);*)
             Dream.respond "nada");
       ]
