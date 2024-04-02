open Alcotest

let test_nothing () = check string "nothing" "a" "a"

let test_operation_of_json () =
  let base =
    "{\"source\":\"7d6f6a1f8e893d4eafd46379a872b91b1f609c0daa697804b53ef03c47ffc8cb\",\"target\":\"b7dcfa1528c4cd3bb1808140cb0baeca3638f43ad13138d2e08d1d27b97afba7\",\"amount\":65049,\"signature\":\"7bf904162753131b2a184367d8e53590f6777fb0a525c8e68c27555d44414cf89c31a66225784698dc2a234ed4a2fe0a5adb08146458346023cf80b5a3812bef\"}"
  in
  check string "json stuff"
    (Cassis.Operation.of_json base
    |> Cassis.Operation.to_json |> Yojson.to_string)
    base

let () =
  run "operation"
    [
      ("nothing", [ test_case "nothing" `Quick test_nothing ]);
      ("of_json", [ test_case "of_json" `Quick test_operation_of_json ]);
    ]
