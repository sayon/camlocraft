open Bigarray

module Bigarray_ext = struct

  let create_floats = Bigarray.Array1.of_array Bigarray.float32 Bigarray.c_layout
  let create_ints = Bigarray.Array1.of_array Bigarray.int32 Bigarray.c_layout
  let create_chars = Bigarray.Array1.of_array Bigarray.char Bigarray.c_layout
  let create_int_buffer ~size = create_ints (Array.make size (Int32.of_int 0))
  let create_char_buffer ~size = create_chars (Array.make size (Char.chr 0))
  let create_byte_buffer ~size = Bigstring.create size
  let get ~buffers ~id = Int32.to_int (Bigarray.Array1.unsafe_get buffers id)

  (** Utility function to use generators returning their result by a
      pointer. *)
  let get_through_buffer generator =
    let bufferForID = create_int_buffer ~size:1 in
    generator bufferForID;
    get ~buffers:bufferForID ~id:0
end

module RawBuffer = struct
  (** A type of a raw buffer of bytes. *)
  type raw_buffer = (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

  let raw_buffer_create size = Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout size

  (** Get a slice of {!Bigarray.Array1} starting at [from] and up to, but not
      including [limit]. The result is an instance of {!Bytes.t}. *)
  let slice_bytes (array:raw_buffer) from limit =
    let length = limit - from in
    let sub = Bigarray.Array1.sub array from length in
    Bytes.init length (fun x -> Char.chr (Bigarray.Array1.unsafe_get sub x))


  let copy a1 s1 a2 s2 length =
    for i = s1 to s1 + length - 1 do
      let tmp = Bigarray.Array1.unsafe_get a1 (s1+i) in
      Bigarray.Array1.unsafe_set a2 (s2+i) tmp
    done
end

module File = struct
  (** Reads all text from a specified file. Returns {!Either.Left} of text or
      {!Either.right} of an error message. *)
  let read filename =
    try
      let ch = open_in filename in
      let s = really_input_string ch (in_channel_length ch) in
      close_in ch;
      Either.Left s
    with | Sys_error e -> Either.Right e


  (** Helper to efficiently map an entire file in memory. See {!Unix.map_file}.  *)
  let map fd : RawBuffer.raw_buffer =
    let let_map_file_decide_on_array_dimensions = -1 in
    let genarray = Unix.map_file fd int8_unsigned c_layout false
        [| let_map_file_decide_on_array_dimensions |] in
    let len = (Bigarray.Genarray.dims genarray).(0) in
    Bigarray.reshape_1 genarray len

end
