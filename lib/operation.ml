open Stdint

exception Invalid_operation_json

module Trust = struct
  type t =
    { ts : uint32
    ; source : uint32
    ; target : bytes
    ; amount : uint32
    ; signature : bytes
    }

  let tag = 't'

  let serialise alloc v =
    let baf = alloc (1 + 4 + 32 + 4 + 64) in
    Bigstringaf.unsafe_set baf 0 tag;
    Bigstringaf.unsafe_set_int32_le baf 1 (Int32.of_uint32 v.ts);
    Bigstringaf.unsafe_set_int32_le baf 5 (Int32.of_uint32 v.source);
    Bigstringaf.unsafe_blit_from_bytes v.target ~src_off:0 baf ~dst_off:9 ~len:32;
    Bigstringaf.unsafe_set_int32_le baf 41 (Int32.of_uint32 v.amount);
    Bigstringaf.unsafe_blit_from_bytes v.signature ~src_off:0 baf ~dst_off:45 ~len:64;
    baf
  ;;

  let deserialise baf =
    let signature = Bytes.create 64 in
    let target = Bytes.create 32 in
    let ts = Bigstringaf.unsafe_get_int32_le baf 1 |> Uint32.of_int32 in
    let source = Bigstringaf.unsafe_get_int32_le baf 5 |> Uint32.of_int32 in
    Bigstringaf.unsafe_blit_to_bytes baf ~src_off:9 target ~dst_off:0 ~len:32;
    let amount = Bigstringaf.unsafe_get_int32_le baf 41 |> Uint32.of_int32 in
    Bigstringaf.unsafe_blit_to_bytes baf ~src_off:45 signature ~dst_off:0 ~len:64;
    { ts; source; target; amount; signature }
  ;;

  let of_json json =
    let open Yojson.Basic.Util in
    { ts = json |> member "ts" |> to_int |> Uint32.of_int
    ; source = json |> member "source" |> to_int |> Uint32.of_int
    ; target = json |> member "target" |> to_string |> Util.bytes_of_hex
    ; amount = json |> member "amount" |> to_int |> Uint32.of_int
    ; signature = json |> member "signature" |> to_string |> Util.bytes_of_hex
    }
  ;;

  let to_json v =
    `Assoc
      [ "tag", `String (tag |> Char.escaped)
      ; "ts", `Int (v.ts |> Uint32.to_int)
      ; "source", `Int (v.source |> Uint32.to_int)
      ; "target", `String (v.target |> Hex.of_bytes |> Hex.show)
      ; "amount", `Int (v.amount |> Uint32.to_int)
      ; "signature", `String (v.signature |> Hex.of_bytes |> Hex.show)
      ]
  ;;
end

module Hop = struct
  type t =
    { source : uint32
    ; target : uint32
    ; amount : uint32
    }

  let size = 4 + 4 + 4

  let serialise baf ~offset v =
    Bigstringaf.unsafe_set_int32_le baf (offset + 0) (Int32.of_uint32 v.source);
    Bigstringaf.unsafe_set_int32_le baf (offset + 4) (Int32.of_uint32 v.target);
    Bigstringaf.unsafe_set_int32_le baf (offset + 8) (Int32.of_uint32 v.amount)
  ;;

  let deserialise baf ~offset =
    let source = Bigstringaf.unsafe_get_int32_le baf (offset + 0) |> Uint32.of_int32 in
    let target = Bigstringaf.unsafe_get_int32_le baf (offset + 4) |> Uint32.of_int32 in
    let amount = Bigstringaf.unsafe_get_int32_le baf (offset + 8) |> Uint32.of_int32 in
    { source; target; amount }
  ;;

  let of_json json =
    let open Yojson.Basic.Util in
    { source = json |> member "source" |> to_int |> Uint32.of_int
    ; target = json |> member "target" |> to_int |> Uint32.of_int
    ; amount = json |> member "amount" |> to_int |> Uint32.of_int
    }
  ;;

  let to_json v =
    `Assoc
      [ "source", `Int (v.source |> Uint32.to_int)
      ; "target", `Int (v.target |> Uint32.to_int)
      ; "amount", `Int (v.amount |> Uint32.to_int)
      ]
  ;;
end

module Signature = struct
  type t =
    { signer : uint32
    ; signature : bytes
    }

  let size = 4 + 64

  let serialise baf ~offset v =
    Bigstringaf.unsafe_set_int32_le baf (offset + 0) (Int32.of_uint32 v.signer)
  ;;

  let deserialise baf ~offset =
    let signer = Bigstringaf.unsafe_get_int32_le baf (offset + 0) |> Uint32.of_int32 in
    let signature = Bytes.create 64 in
    Bigstringaf.unsafe_blit_to_bytes
      baf
      ~src_off:(offset + 4)
      signature
      ~dst_off:0
      ~len:64;
    { signer; signature }
  ;;

  let of_json json =
    let open Yojson.Basic.Util in
    { signer = json |> member "signer" |> to_int |> Uint32.of_int
    ; signature = json |> member "signature" |> to_string |> Util.bytes_of_hex
    }
  ;;

  let to_json v =
    `Assoc
      [ "signer", `Int (v.signer |> Uint32.to_int)
      ; "signatures", `String (v.signature |> Hex.of_bytes |> Hex.show)
      ]
  ;;
end

module Transfer = struct
  type t =
    { ts : uint32
    ; hops : Hop.t array
    ; signatures : Signature.t array
    }

  let tag = 'x'

  let serialise alloc v =
    let nhops = Array.length v.hops in
    let nsigs = Array.length v.signatures in
    let baf = alloc (1 + 32 + 1 + (nhops * Hop.size) + 64) in
    Bigstringaf.unsafe_set baf 0 tag;
    Bigstringaf.unsafe_set baf 1 (char_of_int nhops);
    Bigstringaf.unsafe_set baf 2 (char_of_int nsigs);
    Bigstringaf.unsafe_set_int32_le baf 3 (Int32.of_uint32 v.ts);
    Array.iteri
      (fun i hop -> hop |> Hop.serialise baf ~offset:(8 + (i * Hop.size)))
      v.hops;
    Array.iteri
      (fun i sig_ ->
        sig_
        |> Signature.serialise baf ~offset:(8 + (nhops * Hop.size) + (i * Signature.size)))
      v.signatures;
    baf
  ;;

  let deserialise baf =
    let nhops = Bigstringaf.unsafe_get baf 1 |> int_of_char in
    let nsigs = Bigstringaf.unsafe_get baf 2 |> int_of_char in
    let ts = Bigstringaf.unsafe_get_int32_le baf 3 |> Uint32.of_int32 in
    let hops =
      Array.init nhops (fun i -> Hop.deserialise baf ~offset:(8 + (i * Hop.size)))
    in
    let signatures =
      Array.init nsigs (fun i ->
        Signature.deserialise baf ~offset:(8 + (nhops * Hop.size) + (i * Signature.size)))
    in
    { ts; hops; signatures }
  ;;

  let of_json json =
    let open Yojson.Basic.Util in
    { ts = json |> member "ts" |> to_int |> Uint32.of_int
    ; hops = json |> member "hops" |> to_list |> List.map Hop.of_json |> Array.of_list
    ; signatures =
        json
        |> member "signatures"
        |> to_list
        |> List.map Signature.of_json
        |> Array.of_list
    }
  ;;

  let to_json v =
    `Assoc
      [ "tag", `String (tag |> Char.escaped)
      ; "ts", `Int (v.ts |> Uint32.to_int)
      ; "hops", `List (v.hops |> Array.map Hop.to_json |> Array.to_list)
      ; "signatures", `List (v.signatures |> Array.map Signature.to_json |> Array.to_list)
      ]
  ;;
end

type t =
  | Trust of Trust.t
  | Transfer of Transfer.t
  | Unknown

let serialise alloc op =
  match op with
  | Trust t -> Trust.serialise alloc t
  | Transfer x -> Transfer.serialise alloc x
  | Unknown -> alloc 1
;;

let deserialise baf =
  let tag = Bigstringaf.get baf 0 in
  match tag with
  | 't' -> Trust (Trust.deserialise baf)
  | 'x' -> Transfer (Transfer.deserialise baf)
  | _ -> Unknown
;;

let conv = Lmdb.Conv.make ~serialise ~deserialise ()

let of_json json =
  let open Yojson.Basic.Util in
  try
    match json |> member "tag" |> to_string with
    | "t" -> Trust (Trust.of_json json)
    | "x" -> Transfer (Transfer.of_json json)
    | _ -> raise Invalid_operation_json
  with
  | Yojson.Json_error _ -> raise Invalid_operation_json
  | Yojson.End_of_array -> raise Invalid_operation_json
  | Yojson.End_of_input -> raise Invalid_operation_json
  | Yojson.End_of_tuple -> raise Invalid_operation_json
  | Yojson.End_of_object -> raise Invalid_operation_json
  | Yojson.Basic.Util.Type_error _ -> raise Invalid_operation_json
;;

let of_json_string str = Yojson.Basic.from_string str |> of_json

let to_json op =
  match op with
  | Trust t -> Trust.to_json t
  | Transfer x -> Transfer.to_json x
  | Unknown -> `Null
;;

let to_json_string op = to_json op |> Yojson.to_string
