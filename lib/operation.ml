open Stdint

module Trust = struct
  type t = {
    source : bytes;
    target : bytes;
    amount : uint32;
    signature : bytes;
  }

  let tag = 't'

  let serialize alloc v =
    let baf = alloc (1 + 32 + 32 + 4 + 64) in
    Bigstringaf.unsafe_set baf 0 tag;
    Bigstringaf.unsafe_blit_from_bytes v.source ~src_off:0 baf ~dst_off:1
      ~len:32;
    Bigstringaf.unsafe_blit_from_bytes v.target ~src_off:0 baf ~dst_off:33
      ~len:32;
    Bigstringaf.unsafe_blit_from_bytes
      (let b = Bytes.create 4 in
       Uint32.to_bytes_big_endian v.amount b 0;
       b)
      ~src_off:0 baf ~dst_off:65 ~len:4;
    Bigstringaf.unsafe_blit_from_bytes v.signature ~src_off:0 baf ~dst_off:69
      ~len:64;
    baf

  let deserialize baf =
    let source = Bytes.create 32
    and target = Bytes.create 32
    and signature = Bytes.create 64
    and amount_bytes = Bytes.create 4 in
    Bigstringaf.unsafe_blit_to_bytes baf ~src_off:1 source ~dst_off:0 ~len:32;
    Bigstringaf.unsafe_blit_to_bytes baf ~src_off:33 target ~dst_off:0 ~len:32;
    Bigstringaf.unsafe_blit_to_bytes baf ~src_off:65 amount_bytes ~dst_off:0
      ~len:4;
    let amount = Uint32.of_bytes_big_endian amount_bytes 0 in
    Bigstringaf.unsafe_blit_to_bytes baf ~src_off:69 signature ~dst_off:0
      ~len:64;
    { source; target; amount; signature }

  let of_json json =
    let open Yojson.Basic.Util in
    {
      source =
        json |> member "source" |> to_string |> Hex.of_string |> Hex.to_bytes;
      target =
        json |> member "target" |> to_string |> Hex.of_string |> Hex.to_bytes;
      amount = json |> member "amount" |> to_int |> Uint32.of_int;
      signature =
        json |> member "signature" |> to_string |> Hex.of_string |> Hex.to_bytes;
    }

  let to_json v =
    `Assoc
      [
        ("tag", `String (tag |> Char.escaped));
        ("source", `String (v.source |> Hex.of_bytes |> Hex.show));
        ("target", `String (v.target |> Hex.of_bytes |> Hex.show));
        ("amount", `Int (v.amount |> Uint32.to_int));
        ("signature", `String (v.signature |> Hex.of_bytes |> Hex.show));
      ]
end

module Hop = struct
  type t = { amount : uint32; target : bytes }

  let size = 4 + 32

  let serialize baf ~offset v =
    Bigstringaf.unsafe_blit_from_bytes
      (let b = Bytes.create 4 in
       Uint32.to_bytes_big_endian v.amount b 0;
       b)
      ~src_off:0 baf ~dst_off:(offset + 0) ~len:4;
    Bigstringaf.unsafe_blit_from_bytes v.target ~src_off:0 baf
      ~dst_off:(offset + 4) ~len:32

  let deserialize baf ~offset =
    let target = Bytes.create 32 and amount_bytes = Bytes.create 4 in
    Bigstringaf.unsafe_blit_to_bytes baf ~src_off:offset target ~dst_off:0
      ~len:4;
    Bigstringaf.unsafe_blit_to_bytes baf ~src_off:(offset + 4) amount_bytes
      ~dst_off:0 ~len:32;
    let amount = Uint32.of_bytes_big_endian amount_bytes 0 in
    { target; amount }

  let of_json json =
    let open Yojson.Basic.Util in
    {
      amount = json |> member "amount" |> to_int |> Uint32.of_int;
      target =
        json |> member "target" |> to_string |> Hex.of_string |> Hex.to_bytes;
    }

  let to_json v =
    `Assoc
      [
        ("amount", `Int (v.amount |> Uint32.to_int));
        ("target", `String (v.target |> Hex.of_bytes |> Hex.show));
      ]
end

module Send = struct
  type t = { source : bytes; hops : Hop.t array; signature : bytes }

  let tag = 's'

  let serialize alloc v =
    let nhops = Array.length v.hops in
    let baf = alloc (1 + 32 + 1 + (nhops * Hop.size) + 64) in
    Bigstringaf.unsafe_set baf 0 tag;
    Bigstringaf.unsafe_blit_from_bytes v.source ~src_off:0 baf ~dst_off:1
      ~len:32;
    Bigstringaf.unsafe_set baf 33 (char_of_int nhops);
    v.hops
    |> Array.iteri (fun i hop ->
           Hop.serialize baf ~offset:(34 + (i * Hop.size)) hop);
    Bigstringaf.unsafe_blit_from_bytes v.signature ~src_off:0 baf
      ~dst_off:(34 + (nhops * Hop.size))
      ~len:64;
    baf

  let deserialize baf =
    let source = Bytes.create 32 in
    let signature = Bytes.create 64 in
    Bigstringaf.unsafe_blit_to_bytes baf ~src_off:1 source ~dst_off:0 ~len:32;
    let nhops = Bigstringaf.unsafe_get baf 33 |> int_of_char in
    let hops =
      Array.init nhops (fun i ->
          Hop.deserialize baf ~offset:(34 + (i * Hop.size)))
    in
    Bigstringaf.unsafe_blit_to_bytes baf
      ~src_off:(34 + (nhops * Hop.size))
      signature ~dst_off:0 ~len:64;
    { source; hops; signature }

  let of_json json =
    let open Yojson.Basic.Util in
    {
      source =
        json |> member "source" |> to_string |> Hex.of_string |> Hex.to_bytes;
      signature =
        json |> member "signature" |> to_string |> Hex.of_string |> Hex.to_bytes;
      hops =
        json |> member "hops" |> to_list |> List.map Hop.of_json
        |> Array.of_list;
    }

  let to_json v =
    `Assoc
      [
        ("tag", `String (tag |> Char.escaped));
        ("source", `String (v.source |> Hex.of_bytes |> Hex.show));
        ("hops", `List (v.hops |> Array.map Hop.to_json |> Array.to_list));
        ("signature", `String (v.signature |> Hex.of_bytes |> Hex.show));
      ]
end

type t = Trust of Trust.t | Send of Send.t | Unknown

let serialise alloc op =
  match op with
  | Trust t -> Trust.serialize alloc t
  | Send t -> Send.serialize alloc t
  | Unknown -> alloc 1

let deserialise baf =
  let tag = Bigstringaf.get baf 0 in
  match tag with
  | 't' -> Trust (Trust.deserialize baf)
  | 's' -> Send (Send.deserialize baf)
  | _ -> Unknown

let conv = Lmdb.Conv.make ~serialise ~deserialise ()

let of_json str =
  let json = Yojson.Basic.from_string str in
  let open Yojson.Basic.Util in
  match json |> member "tag" |> to_string with
  | "t" -> Trust (Trust.of_json json)
  | "s" -> Send (Send.of_json json)
  | _ -> raise (Yojson.Json_error "unexpected 'tag'")

let to_json op =
  match op with
  | Trust t -> Trust.to_json t
  | Send t -> Send.to_json t
  | Unknown -> `Null
