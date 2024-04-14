open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type message = { typ : string } [@@deriving yojson]

let () =
  Dream.run ~interface:"0.0.0.0" ~port:3002
  @@ Dream.router [ Dream.get "/" (fun _ -> Dream.respond "cassis router") ]
;;
