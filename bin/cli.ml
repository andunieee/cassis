open Core
open Cassis
open Cassis.Util
open Stdint
open Cmdliner

(* arg converters *)
let parse_with t_of_str exp1 validate exp2 s =
  try
    let value = t_of_str s in
    if validate value then `Ok value else `Error exp2
  with
  | _ -> `Error exp1
;;

let uint32 =
  ( parse_with
      Uint32.of_string
      "must be a valid positive integer"
      Poly.(( > ) (Uint32.of_int 100000))
      "don't be reckless"
  , fun fmt v -> v |> Uint32.to_string |> Format.pp_print_string fmt )
;;

let hex =
  ( parse_with
      bytes_of_hex
      "must be a hex string"
      (fun v -> Bytes.length v = 32)
      "must be 32 bytes"
  , fun fmt v -> v |> hex_of_bytes |> Format.pp_print_string fmt )
;;

(* trust command *)
let trust =
  let get_sec =
    Arg.(
      required
      & opt (some hex) None
      & info [ "key" ] ~docv:"SECKEY" ~doc:"secret key of the caller, as hex")
  in
  let get_source =
    Arg.(
      required
      & opt (some uint32) None
      & info [ "source" ] ~docv:"SOURCE" ~doc:"serial number of the caller")
  in
  let get_target =
    Arg.(
      required
      & opt (some hex) None
      & info [ "target" ] ~docv:"PUBKEY" ~doc:"pubkey to trust, as hex")
  in
  let get_amount =
    Arg.(
      required
      & opt (some uint32) None
      & info [ "amount" ] ~docv:"SATOSHIS" ~doc:"amount with which to trust PUBKEY")
  in
  let run key_hex source target amount =
    Printf.printf
      "trusting %s with %s\n%!"
      (target |> hex_of_bytes)
      (amount |> Uint32.to_string);
    let sec = Bip340.load_secret key_hex in
    let trust : Operation.Trust.t =
      { ts = Core_unix.time () |> Float.to_int64 |> Uint32.of_int64
      ; source
      ; amount
      ; target
      ; signature = Bytes.create 64
      }
    in
    let ser_nosig = Operation.Trust.serialise_nosig trust |> Bytes.to_string in
    trust.signature <- Bip340.sign ~keypair:sec ser_nosig;
    let resp =
      Ezcurl.post
        ~url:"http://localhost:3000/append"
        ~content:(`String (trust |> Operation.Trust.to_json |> Yojson.to_string))
        ~params:[]
        ()
    in
    match resp with
    | Ok c -> Printf.printf "resp: %s" c.body
    | Error (_, s) -> Printf.printf "error: %s" s
  in
  Cmd.v
    Cmd.(info ~doc:"declare trust in some other pubkey" "trust")
    Term.(const run $ get_sec $ get_source $ get_target $ get_amount)
;;

(* transfer command *)
let transfer =
  Cmd.v
    Cmd.(info ~doc:"transfer money to some destination" "transfer")
    Term.(const (fun _ -> print_endline "x") $ const ())
;;

(* default *)
let cassis () = print_endline "cassis 0.1"
let default = Term.(const cassis $ const ())
let group = Cmd.group ~default (Cmd.info "cassis") [ trust; transfer ]
let () = if !Sys.interactive then () else Cmd.eval group |> exit
