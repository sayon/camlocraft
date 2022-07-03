(** Bigarray extensions *)
include Bigarray

let createFloats = Bigarray.Array1.of_array Bigarray.float32 Bigarray.c_layout
let createInts = Bigarray.Array1.of_array Bigarray.int32 Bigarray.c_layout
let createChars = Bigarray.Array1.of_array Bigarray.char Bigarray.c_layout
let createIntBuffer ~size = createInts (Array.make size (Int32.of_int 0))
let createCharBuffer ~size = createChars (Array.make size (Char.chr 0))
let createByteBuffer ~size = Bigstring.create size
let get ~buffers ~id = Int32.to_int (Bigarray.Array1.unsafe_get buffers id)
