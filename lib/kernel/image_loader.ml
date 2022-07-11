open Io.RawBuffer
open Logger
open Bigarray

(** Different pixel formats and associated constants. *)
module Formats = struct
  module BytesIn = struct
  let rgb = 3 and bgr = 3
  let rgba = 4 and bgra = 4
end

  type pixel3 = {r:int; g:int; b:int}
  type pixel4 = {r: int; g:int; b:int; a: int}

  let pixel4_from_rgb r g b = {r; g; b; a=255}
  let pixel4_from_bgr b g r = {r; g; b; a=255}

  let pixel4_from_pixel3: pixel3 -> pixel4 =
    function | {r;g;b} -> pixel4_from_rgb r g b

  let string_of_pixel4 =
    function | {r;g;b;a} ->
      Printf.sprintf
        "Pixel {red: %d; green: %d; blue: %d; alpha: %d}"
        r g b a
end


(** Internal image format ready for OpenGL. The type {image} contains a buffer storing pixels in RGBA8
    format without padding, and the 0-th pixel line is in the bottom of texture. *)
module Image = struct
  (** Image metadata -- just enough to use it for the engine later. *)
  type meta = {
    name: string;
    width: int;
    height: int;
  }

  (** Type of an image that is ready to be passed to OpenGL to create a texture
      from it.The buffer should be in RGBA8 format, and the pixel lines start
      from the bottom.*)
  type image = {
    info: meta;
    pixels: raw_buffer
  }

  (** Generate an empty image from metadata. *)
  let from_meta m = {
    info = m;
    pixels = raw_buffer_create @@ m.width * m.height * Formats.BytesIn.rgba
  }

  let byte_offset_from_xy img x y =
    let w = img.info.width * Formats.BytesIn.rgba
    and h = img.info.height in
    w*(h-y-1) + x * Formats.BytesIn.rgba

  let set img ~x ~y px =
    let open Formats in
    let set_ba = Bigarray.Array1.set in
    let ofs = byte_offset_from_xy img x y in
    set_ba img.pixels (ofs+0) px.a;
    set_ba img.pixels (ofs+1) px.r;
    set_ba img.pixels (ofs+2) px.g;
    set_ba img.pixels (ofs+3) px.b


  let get img ~x ~y : Formats.pixel4 =
    let open Formats in
    let get_ba = Bigarray.Array1.get in
    let ofs = byte_offset_from_xy img x y in
    let a = get_ba img.pixels (ofs+0)
    and r = get_ba img.pixels (ofs+1)
    and g = get_ba img.pixels (ofs+2)
    and b = get_ba img.pixels (ofs+3) in
    {r;g;b;a}
end


(** BMP file format reader

    BMP files have the following structure:

    0       uint16_t bfType;
    2       uint32_t  bfileSize;
    6       uint32_t bfReserved;
    10      uint32_t bOffBits;
    14      uint32_t biSize;
    18      uint32_t biWidth;
    22      uint32_t  biHeight;
        uint16_t  biPlanes;
        uint16_t biBitCount;
        uint32_t biCompression;
        uint32_t biSizeImage;
        uint32_t biXPelsPerMeter;
        uint32_t biYPelsPerMeter;
        uint32_t biClrUsed;
        uint32_t  biClrImportant;

    ...
    Then goes the palette and then a pixel array.
*)
module BMP = struct

  (** A piece of BMP header with only important parts. *)
  type header = { width: int; height: int }

  (** Length of all BMP headers combined, in bytes. *)
  let header_length = 54

  let get_padding width = width mod 3

  (** Computes the real width of a pixel line in bytes, accounting for the
  padding at the end of the line. *)
  let get_real_byte_width width = width * Formats.BytesIn.bgr + get_padding width

  let header_from_bigarray (bigarray:raw_buffer) : header =
    let width_offset = 18 and height_offset = 22 in
    let bts = slice_bytes bigarray 0 header_length in
    let width  = Int32.to_int @@ Bytes.get_int32_le bts width_offset
    and height = Int32.to_int @@ Bytes.get_int32_le bts height_offset in
    { width; height }

  let byte_offset_from_xy (header:header) x y =
    let w = get_real_byte_width header.width in
    w * y + x * Formats.BytesIn.bgr

  (** Get pixel value from an array of bmp pixels. *)
  let get_pixel (header:header) ba x y : Formats.pixel3 =
    let ofs = byte_offset_from_xy header x y in
    let get_byte = Array1.get ba in
    let b = get_byte ofs
    and g = get_byte (ofs+1)
    and r = get_byte (ofs+2) in
    {b;g;r}

  let header_to_meta name : header -> Image.meta = function |{width;height} -> {name; width; height}

  let image_of_mapped_bmp_array name bmp_header (mapped_image:raw_buffer) : Image.image =
    let meta = header_to_meta name bmp_header in
    let result = Image.from_meta meta in
    for y = 0 to meta.height-1 do
      for x = 0 to meta.width-1 do
        let px = get_pixel bmp_header mapped_image x y in
        Image.set result ~x:x ~y:y (Formats.pixel4_from_pixel3 px)
      done
    done;
    result

  let image_of_bigarray name (ba:raw_buffer): Image.image =
    (* We use [0] as {!file_perm} because we do not want the file to be
        created if it does not exist, so no value of {!file_perm} will make sense. *)
    (* Assume that the file does not have a palette *)
    let header = header_from_bigarray ba in
    let mapped_length = Bigarray.Array1.dim ba in
    let pixels_length = mapped_length - header_length in
    let mapped_image = Bigarray.Array1.sub ba header_length pixels_length in
    image_of_mapped_bmp_array name header mapped_image

  let image_of_file filename =
    try
      begin
        log GeneralLog @@ Printf.sprintf "Trying to open file \"%s\" for reading" filename;
        let fd = Unix.openfile filename [Unix.O_RDONLY] 700 in
        log GeneralLog @@ Printf.sprintf "Opened file \"%s\" for reading" filename;
        let mapped_file = Io.File.map fd in
        let result = Some (image_of_bigarray filename mapped_file) in
        Unix.close fd;
        result
      end
    with
    | Unix.Unix_error (e, s1, s2) ->
      log TexturesLog @@
      Printf.sprintf
        "Error UnixError while reading texture file \"%s\":\n Message: %s\n Function: %s \n Parameter: %s"
        filename (Unix.error_message e) s1 s2;
      None
    | Invalid_argument s ->
      log TexturesLog @@ Printf.sprintf "Error while reading texture file \"%s\": %s" filename s; None
    | Failure s ->
      log TexturesLog @@ Printf.sprintf "Failure while reading texture file \"%s\": %s" filename s; None

end