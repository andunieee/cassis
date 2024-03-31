open Ctypes
open Foreign

(* types *)

type secp256k1_context = unit ptr

let secp256k1_context : secp256k1_context typ = ptr void

type secp256k1_keypair = unit ptr

let secp256k1_keypair : unit abstract typ =
  abstract ~name:"secp256k1_keypair" ~size:96 ~alignment:0

type secp256k1_xonly_pubkey = unit ptr

let secp256k1_xonly_pubkey : unit abstract typ =
  abstract ~name:"secp256k1_xonly_pubkey" ~size:64 ~alignment:0

(* bindings *)

let secp256k1_context_create =
  foreign "secp256k1_context_create" (uint @-> returning secp256k1_context)

let secp256k1_schnorrsig_sign32 =
  foreign "secp256k1_schnorrsig_sign32"
    (secp256k1_context @-> ptr char @-> ptr char @-> ptr secp256k1_keypair
   @-> ptr char @-> returning int)

let secp256k1_keypair_create =
  foreign "secp256k1_keypair_create"
    (secp256k1_context @-> ptr secp256k1_keypair @-> ptr char @-> returning int)

let secp256k1_xonly_pubkey_parse =
  foreign "secp256k1_xonly_pubkey_parse"
    (secp256k1_context @-> ptr secp256k1_xonly_pubkey @-> ptr char
   @-> returning int)

(* wrappers *)

let ctx =
  secp256k1_context_create
    (Unsigned.UInt.of_int ((1 lsl 0) lor (1 lsl 8) lor (1 lsl 9)))

let sign =
  let msg32 = allocate_n char ~count:32 in
  let sig64 = allocate_n char ~count:64 in
  let aux_rand = allocate_n char ~count:32 in
  let sec = allocate_n char ~count:32 in
  sec +@ 31 <-@ '1';
  let keypair_alloc = allocate_n secp256k1_keypair ~count:1 in
  let keypair = keypair_alloc +@ 0 in
  let ok = secp256k1_keypair_create ctx keypair sec in
  Printf.printf "ok? %d" ok;
  let ok = secp256k1_schnorrsig_sign32 ctx sig64 msg32 keypair aux_rand in
  Printf.printf "ok? %d" ok;
  Bytes.init 64 (fun i -> !@(sig64 +@ i))
