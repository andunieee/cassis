open Stdint
open Core

module Line = struct
  type t =
    { (* a tuple of the pubkeys, sorted *)
      peers : bytes * bytes
    ; (* first element: how much the left side is trusted by the right side - and vice-versa *)
      trust : uint32 * uint32
    ; (* true: left side owes right side; false: right side owes left side *)
      balance : bool * uint32
    }

  let serialise alloc line =
    let baf = alloc (1 + 32 + 4 + 4 + 1 + 5) in
    Bigstringaf.unsafe_set baf 0 (Char.unsafe_of_int 0);
    let first_peer, second_peer = line.peers in
    Bigstringaf.unsafe_blit_from_bytes first_peer ~src_off:0 baf ~dst_off:1 ~len:32;
    Bigstringaf.unsafe_blit_from_bytes second_peer ~src_off:0 baf ~dst_off:33 ~len:32;
    let trust_to_first, trust_to_second = line.trust in
    Bigstringaf.unsafe_set_int32_le baf 65 (Uint32.to_int32 trust_to_first);
    Bigstringaf.unsafe_set_int32_le baf 69 (Uint32.to_int32 trust_to_second);
    let left_owes_right, amount = line.balance in
    Bigstringaf.unsafe_set baf 73 (Char.unsafe_of_int (if left_owes_right then 1 else 0));
    Bigstringaf.unsafe_set_int32_le baf 74 (Uint32.to_int32 amount);
    baf
  ;;

  let deserialise baf =
    let first_peer = Bytes.create 32
    and second_peer = Bytes.create 32 in
    Bigstringaf.unsafe_blit_to_bytes baf ~src_off:1 first_peer ~dst_off:0 ~len:32;
    Bigstringaf.unsafe_blit_to_bytes baf ~src_off:33 second_peer ~dst_off:0 ~len:32;
    let trust_to_first = Bigstringaf.unsafe_get_int32_le baf 65 |> Uint32.of_int32 in
    let trust_to_second = Bigstringaf.unsafe_get_int32_le baf 69 |> Uint32.of_int32 in
    let left_owes_right = Bigstringaf.unsafe_get baf 73 |> Char.to_int |> equal 1 in
    let amount = Bigstringaf.unsafe_get_int32_le baf 74 |> Uint32.of_int32 in
    { peers = first_peer, second_peer
    ; trust = trust_to_first, trust_to_second
    ; balance = left_owes_right, amount
    }
  ;;

  let conv = Lmdb.Conv.make ~serialise ~deserialise ()
end

let _mutex = Error_checking_mutex.create ()
let lock _ = Error_checking_mutex.lock _mutex
let unlock _ = Error_checking_mutex.unlock _mutex
let init _ : (string, Line.t) Base.Hashtbl.t = Hashtbl.create (module String)

let inplace_validate_and_apply state op =
  let open Operation in
  match op with
  | Unknown -> false
  | Trust t ->
    let key = Bytes.to_string t.source ^ Bytes.to_string t.target in
    Hashtbl.update state key ~f:(fun currp : Line.t ->
      match currp with
      | Some curr -> curr
      | None ->
        { peers = t.source, t.target
        ; trust = Uint32.zero, t.amount
        ; balance = false, Uint32.zero
        });
    true
  | _ -> false
;;
