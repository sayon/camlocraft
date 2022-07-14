open Bigarray
open Bigarray.Array1

let create_floats = of_array float32 c_layout
let create_ints = of_array int32 c_layout
let create_chars = of_array char c_layout
let create_int_buffer ~size = create_ints (Array.make size (Int32.of_int 0))
let create_char_buffer ~size = create_chars (Array.make size (Char.chr 0))
let create_byte_buffer ~size = Bigstring.create size
let get ~buffers ~id = Int32.to_int (unsafe_get buffers id)

    (** Utility function to use generators returning their result by a
pointer. *)
  let get_through_buffer generator =
  let bufferForID = create_int_buffer ~size:1 in
  generator bufferForID;
    get ~buffers:bufferForID ~id:0
