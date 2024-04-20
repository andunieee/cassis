open Stdint
open Core

module Line = struct
  type t =
    { (* a tuple of the peers identified by their serial number, sorted *)
      peers : uint32 * uint32
    ; (* first element: how much the left side is trusted by the right side - and vice-versa *)
      trust : uint32 * uint32
    ; (* true: left side owes right side; false: right side owes left side *)
      balance : bool * uint32
    }

  let serialise alloc line =
    let baf = alloc (1 + (4 * 2) + (4 * 2) + 1 + 4) in
    Bigstringaf.unsafe_set baf 0 (Char.unsafe_of_int 0);
    let first_peer, second_peer = line.peers in
    Bigstringaf.unsafe_set_int32_le baf 1 (Uint32.to_int32 first_peer);
    Bigstringaf.unsafe_set_int32_le baf 5 (Uint32.to_int32 second_peer);
    let trust_to_first, trust_to_second = line.trust in
    Bigstringaf.unsafe_set_int32_le baf 9 (Uint32.to_int32 trust_to_first);
    Bigstringaf.unsafe_set_int32_le baf 13 (Uint32.to_int32 trust_to_second);
    let left_owes_right, amount = line.balance in
    Bigstringaf.unsafe_set baf 18 (Char.unsafe_of_int (if left_owes_right then 1 else 0));
    Bigstringaf.unsafe_set_int32_le baf 19 (Uint32.to_int32 amount);
    baf
  ;;

  let deserialise baf =
    let first_peer = Bigstringaf.unsafe_get_int32_le baf 1 |> Uint32.of_int32 in
    let second_peer = Bigstringaf.unsafe_get_int32_le baf 5 |> Uint32.of_int32 in
    let trust_to_first = Bigstringaf.unsafe_get_int32_le baf 9 |> Uint32.of_int32 in
    let trust_to_second = Bigstringaf.unsafe_get_int32_le baf 13 |> Uint32.of_int32 in
    let left_owes_right = Bigstringaf.unsafe_get baf 18 |> Char.to_int |> equal 1 in
    let amount = Bigstringaf.unsafe_get_int32_le baf 19 |> Uint32.of_int32 in
    { peers = first_peer, second_peer
    ; trust = trust_to_first, trust_to_second
    ; balance = left_owes_right, amount
    }
  ;;

  let conv = Lmdb.Conv.make ~serialise ~deserialise ()
end

type t =
  { pubkeys : bytes Core.Array.t
  ; lines : (int64, Line.t) Core.Hashtbl.t
  }

let _mutex = Error_checking_mutex.create ()
let lock _ = Error_checking_mutex.lock _mutex
let unlock _ = Error_checking_mutex.unlock _mutex

let init _ : t =
  { pubkeys = Core.Array.create ~len:0 (Bytes.create 0)
  ; lines = Hashtbl.create (module Int64)
  }
;;

let validate_and_apply_inplace state op =
  let open Operation in
  match op with
  | Unknown -> false
  | Trust t ->
    let sourceKey = Core.Array.get state.pubkeys (t.source |> Uint32.to_int) in
    let target =
      match Core.Array.findi ~f:(fun _ v -> Bytes.equal v t.target) state.pubkeys with
      | Some (i, _) -> i
      | None -> 0
    in
    let key =
      let kleft = Uint32.to_int64 t.source in
      let ksleft = Int64.(kleft lsl 32) in
      let kright = Int.to_int64 target in
      Int64.(ksleft lor kright)
    in
    let ser = Trust.serialise Bigstringaf.create t in
    let without_sig = Bytes.create 38 in
    let just_sig = Bytes.create 64 in
    Bigstringaf.unsafe_blit_to_bytes ser ~src_off:0 without_sig ~dst_off:0 ~len:38;
    Bigstringaf.unsafe_blit_to_bytes ser ~src_off:38 just_sig ~dst_off:0 ~len:32;
    let without_sig_str = Bytes.to_string without_sig in
    let valid = Bip340.verify ~pubkey:sourceKey without_sig_str just_sig in
    if valid
    then
      Hashtbl.update state.lines key ~f:(fun currp : Line.t ->
        match currp with
        | Some curr -> curr
        | None ->
          { peers = t.source, target |> Uint32.of_int
          ; trust = Uint32.zero, t.amount
          ; balance = false, Uint32.zero
          });
    true
  | _ -> false
;;
