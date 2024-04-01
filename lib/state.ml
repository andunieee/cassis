open Stdint

module Line = struct
  type t = {
    peers : bytes * bytes;
    (* first element: how much the left side is trusted by the right side - and vice-versa *)
    trust : uint32 * uint32;
    (* true: left side owes right side; false: right side owes left side *)
    balance : bool * uint32;
  }

  let conv =
    Lmdb.Conv.make
      ~serialise:(fun alloc _ -> alloc 1)
      ~deserialise:(fun _ ->
        {
          peers = (Bytes.empty, Bytes.empty);
          trust = (Uint32.of_int 0, Uint32.of_int 0);
          balance = (false, Uint32.of_int 0);
        })
      ()
end
