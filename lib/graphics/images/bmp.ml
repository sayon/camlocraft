open Kernel.Io.RawBuffer
open Kernel.Logger
open Bigarray
open Image


(** BMP file format reader. *)

(** BMP header. See {!header_from_bigarray} for more details. *)
type header = {
  typ: int;
  fileSize: int;
  reserved: int;
  offBits: int;
  size: int;
  width: int;
  height: int;
  planes: int;
  bitCount: int;
  compression: int;
  sizeImage: int;
  xPelsPerMeter: int;
  yPelsPerMeter: int;
  clrUsed: int;
  clrImportant: int;
}

(** Length of all BMP headers combined, in bytes. *)
let header_length = 54
let header_show {typ; fileSize; reserved; offBits; size; width; height; planes; bitCount; compression; sizeImage; xPelsPerMeter; yPelsPerMeter; clrUsed; clrImportant;} : string =
Printf.sprintf "
  typ           = %d
  fileSize      = %d
  reserved      = %d
  offBits       = %d
  size          = %d
  width         = %d
  height        = %d
  planes        = %d
  bitCount      = %d
  compression   = %d
  sizeImage     = %d
  xPelsPerMeter = %d
  yPelsPerMeter = %d
  clrUsed       = %d
  clrImportant  = %d
" typ fileSize reserved offBits size width height planes bitCount compression sizeImage xPelsPerMeter yPelsPerMeter clrUsed clrImportant

let get_padding width = width mod 4

(** Computes the real width of a pixel line in bytes, accounting for the
    padding at the end of the line. *)
let get_real_byte_width width =
  (width * Formats.BytesIn.bgr) + get_padding width

let header_from_bigarray (bigarray:raw_buffer) : header =
  let bts = slice_bytes bigarray 0 header_length in
  let i32 ofs = Int32.to_int @@ Bytes.get_int32_le bts ofs in
  let i16 ofs = Bytes.get_int16_le bts ofs in
  {
    typ           = i16 0;
    fileSize      = i32 2;
    reserved      = i32 6;
    offBits       = i32 10;
    size          = i32 14;
    width         = i32 18;
    height        = i32 22;
    planes        = i16 24;
    bitCount      = i16 26;
    compression   = i32 30;
    sizeImage     = i32 34;
    xPelsPerMeter = i32 38;
    yPelsPerMeter = i32 42;
    clrUsed       = i32 46;
    clrImportant  = i32 50;
  }

let byte_offset_from_xy (header:header) x y =

  let w = get_real_byte_width header.width in
  w * y + x * Formats.BytesIn.bgr

(** Get pixel value from an array of bmp pixels. *)
let get_pixel (header:header) (ba:raw_buffer) x y : Formats.pixel3 =
  let ofs = byte_offset_from_xy header x y in
  let get_byte = Array1.get ba in
  let b = get_byte ofs
  and g = get_byte (ofs+1)
  and r = get_byte (ofs+2) in
  {b;g;r}

let header_to_meta name : header -> Image.meta = function |{width;height;_} -> {name; width; height}

let image_of_mapped_bmp_array name bmp_header (mapped_image:raw_buffer) : Image.image =
  log GeneralLog @@ Printf.sprintf "Reading pixels of BMP file...";
  let meta = header_to_meta name bmp_header in
  log GeneralLog @@ header_show bmp_header;
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
  log GeneralLog @@ Printf.sprintf "Reading header of BMP file successful";
  let mapped_length = Bigarray.Array1.dim ba in
  let pixels_length = mapped_length - header_length in
  let mapped_image = Bigarray.Array1.sub ba header_length pixels_length in
  image_of_mapped_bmp_array name header mapped_image

let image_of_file = Kernel.Io.File.load_file image_of_bigarray
