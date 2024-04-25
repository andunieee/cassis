let bytes_of_hex s = Hex.to_bytes (`Hex s)
let hex_of_bytes x = x |> Hex.of_bytes |> Hex.show
