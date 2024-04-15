let () =
  Dream.run ~interface:"0.0.0.0" ~port:3002
  @@ Dream.router [ Dream.get "/" (fun _ -> Dream.respond "cassis router") ]
;;
