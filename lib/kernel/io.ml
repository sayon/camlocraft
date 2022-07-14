open Bigarray
open Logger

module RawBuffer = struct
  (** A type of a raw buffer of bytes. *)

  open Bigarray

  type raw_buffer = (int, int8_unsigned_elt, c_layout) Array1.t

  let create size = Array1.create int8_unsigned c_layout size

  (** Get a slice of {!Array1} starting at [from] and up to, but not
      including [limit]. The result is an instance of {!Bytes.t}. *)
  let slice_bytes (array:raw_buffer) from limit =
    let length = limit - from in
    let sub = Array1.sub array from length in
    Bytes.init length (fun x -> Char.chr (Array1.unsafe_get sub x))


  let copy a1 s1 a2 s2 length =
    for i = s1 to s1 + length - 1 do
      let tmp = Array1.unsafe_get a1 (s1+i) in
      Array1.unsafe_set a2 (s2+i) tmp
    done

  external set_raw_float: raw_buffer -> int -> float -> unit = "caml_stub_set_raw_float"
  external set_raw_int: raw_buffer -> int -> int -> unit = "caml_stub_set_raw_int"

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


  let load_file loader filename =
    try
      begin
        log GeneralLog @@ Printf.sprintf "Trying to open file \"%s\" for reading" filename;
        let fd = Unix.openfile filename [Unix.O_RDONLY] 700 in
        log GeneralLog @@ Printf.sprintf "Opened file \"%s\" for reading" filename;
        let mapped_file : RawBuffer.raw_buffer = map fd in
        let result = Some (loader filename mapped_file) in
        Unix.close fd;
        result
      end
    with
    | Unix.Unix_error (e, s1, s2) ->
      log IOLog @@
      Printf.sprintf
        "Error UnixError while reading file \"%s\":\n Message: %s\n Function: %s \n Parameter: %s"
        filename (Unix.error_message e) s1 s2;
      None
    | Invalid_argument s ->
      log IOLog @@ Printf.sprintf "Error while reading file \"%s\": %s" filename s; None
    | Failure s ->
      log IOLog @@ Printf.sprintf "Failure while reading file \"%s\": %s" filename s; None
end
