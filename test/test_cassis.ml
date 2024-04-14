open Alcotest
open Cassis

let of_json_to_json label input =
  let fn _ =
    check string label input (Operation.of_json_string input |> Operation.to_json_string)
  in
  fn
;;

let () =
  run
    "operation"
    [ ( "json"
      , [ test_case
            "trust of json to json"
            `Quick
            (of_json_to_json
               "operation"
               "{\"tag\":\"t\",\"source\":\"7d6f6a1f8e893d4eafd46379a872b91b1f609c0daa697804b53ef03c47ffc8cb\",\"target\":\"b7dcfa1528c4cd3bb1808140cb0baeca3638f43ad13138d2e08d1d27b97afba7\",\"amount\":65049,\"signature\":\"7bf904162753131b2a184367d8e53590f6777fb0a525c8e68c27555d44414cf89c31a66225784698dc2a234ed4a2fe0a5adb08146458346023cf80b5a3812bef\"}")
        ; test_case
            "send of json to json"
            `Quick
            (of_json_to_json
               "send"
               "{\"tag\":\"s\",\"source\":\"7d6f6a1f8e893d4eafd46379a872b91b1f609c0daa697804b53ef03c47ffc8cb\",\"hops\":[{\"amount\":500000,\"target\":\"a49e8d4ccdce214870820810658a2045df81d317cfc86be3d7f6dbd0420a9194\"},{\"amount\":500000,\"target\":\"0c097eea2a8f684f2f32c3ea154edf849c6908e76fcee0d95feb0562d197fcc3\"},{\"amount\":500000,\"target\":\"a49e8d4ccdce214870820810658a2045df81d317cfc86be3d7f6dbd0420a9194\"},{\"amount\":500000,\"target\":\"0c097eea2a8f684f2f32c3ea154edf849c6908e76fcee0d95feb0562d197fcc3\"}],\"signature\":\"7bf904162753131b2a184367d8e53590f6777fb0a525c8e68c27555d44414cf89c31a66225784698dc2a234ed4a2fe0a5adb08146458346023cf80b5a3812bef\"}")
        ; test_case "json_failure" `Quick (fun _ ->
            check_raises "invalid" Operation.Invalid_operation_json (fun _ ->
              let _ = Operation.of_json_string "{\"a\":\"b\"}" in
              ()))
        ; test_case "json_failure" `Quick (fun _ ->
            check_raises "invalid" Operation.Invalid_operation_json (fun _ ->
              let _ = Operation.of_json_string "{\"tag\":\"b\"}" in
              ()))
        ] )
    ];
  run
    "bip340"
    [ ( "keys"
      , [ test_case "private key to public key" `Quick (fun _ ->
            check
              string
              "pubkey"
              (Bip340.load_secret
                 (`Hex "e09a4ae9d179c85c1842ad53e19cad764f471ab8072b7cb7f8d58a76aa62e0aa"
                  |> Hex.to_bytes)
               |> Bip340.public_key
               |> Hex.of_bytes
               |> Hex.show)
              "d8c8987a525fa8c8bd895eef6cb84267391cedeb6fde3dcb70d3a192b342ed2d")
        ] )
    ; ( "signatures"
      , [ test_case "sign and verify" `Quick (fun _ ->
            let sec =
              Bip340.load_secret
                (`Hex "e09a4ae9d179c85c1842ad53e19cad764f471ab8072b7cb7f8d58a76aa62e0aa"
                 |> Hex.to_bytes)
            in
            let sig_ = Bip340.sign ~keypair:sec "message" in
            let pub =
              `Hex "d8c8987a525fa8c8bd895eef6cb84267391cedeb6fde3dcb70d3a192b342ed2d"
              |> Hex.to_bytes
            in
            let verified = Bip340.verify ~pubkey:pub "message" sig_ in
            check bool "sign verify" verified true)
        ] )
    ]
;;
