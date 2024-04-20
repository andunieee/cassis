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

  let key_of_serials (s1 : uint32) (s2 : uint32) =
    let left, right = if Poly.(s1 < s2) then s2, s1 else s1, s2 in
    let kleft = Uint32.to_int64 left in
    let ksleft = Int64.(kleft lsl 32) in
    let kright = Uint32.to_int64 right in
    Int64.(ksleft lor kright)
  ;;

  let available_to_receive (line : t) (target : uint32) : uint32 =
    let (first : uint32), (second : uint32) = line.peers in
    let tfirst, tsecond = line.peers in
    let bc, balance = line.balance in
    if Poly.(first = target)
    then
      if bc
      then
        if (* left side is owed by right side already, so it has less room to receive *)
           Poly.(tfirst > balance)
           (* but also we can't go below zero so we do this careful check *)
        then Uint32.(tfirst - balance)
        else Uint32.zero
      else
        (* left side owes right side, so it has more room to receive *)
        Uint32.(tfirst + balance)
    else if Poly.(second = target)
    then
      if bc
      then
        (* right side owes left side, so it has more room to receive *)
        Uint32.(tsecond + balance)
      else if (* right side is owed by left side, so it has less room to receive *)
              Poly.(tsecond > balance)
              (* but also we can't go below zero so we do this careful check *)
      then Uint32.(tsecond - balance)
      else Uint32.zero
    else Uint32.zero
  ;;
end

type t =
  { pubkeys : bytes Array.t
  ; lines : (int64, Line.t) Core.Hashtbl.t
  }

let _mutex = Error_checking_mutex.create ()
let lock _ = Error_checking_mutex.lock _mutex
let unlock _ = Error_checking_mutex.unlock _mutex

let init _ : t =
  { pubkeys = Array.create ~len:0 (Bytes.create 0)
  ; lines = Hashtbl.create (module Int64)
  }
;;

let prepare (state : t) (op : Operation.t) : bool * (t ref -> unit) list =
  match op with
  | Unknown -> false, []
  | Trust t ->
    let source = Uint32.to_int t.source in
    let sourceKey = Array.get state.pubkeys source in
    (* get serial number for pubkey *)
    let target =
      match Array.findi ~f:(fun _ v -> Bytes.equal v t.target) state.pubkeys with
      | Some (i, _) -> i
      | None -> Array.length state.pubkeys
    in
    let left, right = if source < target then source, target else target, source in
    (* key with which to fetch the stored line *)
    let key = Line.key_of_serials (Uint32.of_int target) t.source in
    (* signature validation *)
    let ser = Operation.Trust.serialise Bigstringaf.create t in
    let without_sig = Bytes.create 38 in
    let just_sig = Bytes.create 64 in
    Bigstringaf.unsafe_blit_to_bytes ser ~src_off:0 without_sig ~dst_off:0 ~len:38;
    Bigstringaf.unsafe_blit_to_bytes ser ~src_off:38 just_sig ~dst_off:0 ~len:32;
    let without_sig_str = Bytes.to_string without_sig in
    let valid = Bip340.verify ~pubkey:sourceKey without_sig_str just_sig in
    (* return if it is valid and steps to apply (i.e. add the new trust or modify an existing one) *)
    ( valid
    , [ (fun state_ref ->
          let state = !state_ref in
          Hashtbl.update state.lines key ~f:(fun currp ->
            match currp with
            | None ->
              (* new line created *)
              { peers = Uint32.of_int left, Uint32.of_int right
              ; balance = false, Uint32.zero
              ; trust =
                  (if source < target
                   then Uint32.zero, t.amount
                   else t.amount, Uint32.zero)
              }
            | Some curr ->
              (* updated line *)
              { curr with
                trust =
                  (if source < target
                   then fst curr.trust, t.amount
                   else t.amount, snd curr.trust)
              }))
      ] )
  | Transfer x ->
    (* sanity check: make sure the same line isn't invoked twice *)
    let sane =
      x.hops
      |> Array.map ~f:(fun (hop : Operation.Hop.t) ->
        Line.key_of_serials hop.source hop.target)
      |> Set.of_array (module Int64)
      |> Set.length
      = Array.length x.hops
    in
    (* check if transfers are possible -- i.e. if there is enough trust to complete all the transfers in all lines *)
    if not sane
    then false, []
    else (
      let possible =
        Array.for_all
          ~f:(fun (hop : Operation.Hop.t) ->
            let key = Line.key_of_serials hop.source hop.target in
            match Hashtbl.find state.lines key with
            | None -> false
            | Some line -> Poly.(Line.available_to_receive line hop.target > hop.amount))
          x.hops
      in
      if not possible
      then false, []
      else (
        (* check if transfers are authorized -- i.e. if everybody who lost money in the transfer has signed *)
        let authorized =
          x.hops
          |> Array.fold
               ~init:(Map.empty (module Int))
               ~f:(fun acc (hop : Operation.Hop.t) ->
                 let with_source =
                   Map.update acc (Uint32.to_int hop.source) ~f:(fun v ->
                     match v with
                     | None -> Int64.zero
                     | Some _ -> Int64.zero)
                 in
                 let with_target =
                   Map.update with_source (Uint32.to_int hop.target) ~f:(fun v ->
                     match v with
                     | None -> Int64.zero
                     | Some _ -> Int64.zero)
                 in
                 with_target)
          |> Map.filter ~f:(fun v -> Int64.(v < zero))
          |> Map.keys
          |> List.for_all ~f:(fun key_serial ->
            Array.find
              ~f:(fun sig_ -> equal (Uint32.to_int sig_.signer) key_serial)
              x.signatures
            |> Option.is_some)
        in
        if not authorized
        then false, []
        else (
          (* get signature target *)
          let ser = Operation.Transfer.serialise Bigstringaf.create x in
          let without_sig =
            Bytes.create
              (Bigstringaf.length ser
               - (Array.length x.signatures * Operation.Signature.size))
          in
          Bigstringaf.unsafe_blit_to_bytes ser ~src_off:0 without_sig ~dst_off:0 ~len:38;
          let without_sig_str = Bytes.to_string without_sig in
          (* get pubkeys from serial numbers and validate signatures *)
          let valid =
            Array.for_all
              ~f:(fun sig_ ->
                let key = Array.get state.pubkeys (Uint32.to_int sig_.signer) in
                Bip340.verify ~pubkey:key without_sig_str sig_.signature)
              x.signatures
          in
          (* return if it is valid and steps to apply (i.e. add modify all touched lines -- each hop touches a different line) *)
          let modifier_per_hop (hop : Operation.Hop.t) =
            let modify_line (line : Line.t) = { line with balance = true, Uint32.zero } in
            let modify_state (state_ref : t ref) =
              let state = !state_ref in
              Hashtbl.change
                state.lines
                (Line.key_of_serials hop.source hop.target)
                ~f:(fun line_opt : Line.t option -> line_opt |> Option.map ~f:modify_line)
            in
            modify_state
          in
          valid, x.hops |> List.of_array |> List.map ~f:modifier_per_hop)))
;;
