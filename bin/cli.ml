open Core
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
      (fun s -> `Hex s |> Hex.to_bytes)
      "must be a hex string"
      (fun v -> Bytes.length v = 32)
      "must be 32 bytes"
  , fun fmt v -> v |> Hex.of_bytes |> Hex.show |> Format.pp_print_string fmt )
;;

(* trust command *)
let trust =
  let run target amount =
    match target, amount with
    | Some target, Some amount ->
      Printf.printf
        "trusting %s with %s"
        (target |> Hex.of_bytes |> Hex.show)
        (amount |> Uint32.to_string)
    | _, _ -> ()
  in
  let get_target =
    Arg.(
      value
      & opt (some hex) None
      & info [ "target" ] ~docv:"PUBKEY" ~doc:"pubkey to trust, as hex")
  in
  let get_amount =
    Arg.(
      value
      & opt (some uint32) None
      & info [ "amount" ] ~docv:"SATOSHIS" ~doc:"amount with which to trust PUBKEY")
  in
  Cmd.v
    Cmd.(info ~doc:"declare trust in some other pubkey" "trust")
    Term.(const run $ get_target $ get_amount)
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
