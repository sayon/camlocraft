open Io.RawBuffer

let%test _ =
  let ba = Io.RawBuffer.create 8 in
  set_raw_int ba 0 0xAABBCCDD;
  0xDD = Bigarray.Array1.get ba 0 &&
  0xCC = Bigarray.Array1.get ba 1 &&
  0xBB = Bigarray.Array1.get ba 2 &&
  0xAA = Bigarray.Array1.get ba 3



let%test _ =
  let ba = Io.RawBuffer.create 8 in
  set_raw_float ba 0 1.0;
  0x00 = Bigarray.Array1.get ba 0 &&
  0x00 = Bigarray.Array1.get ba 1 &&
  128 = Bigarray.Array1.get ba 2 &&
  63 = Bigarray.Array1.get ba 3 &&
  0 = Bigarray.Array1.get ba 4 &&
  0 = Bigarray.Array1.get ba 5 &&
  0 = Bigarray.Array1.get ba 6 &&
  0 = Bigarray.Array1.get ba 7
