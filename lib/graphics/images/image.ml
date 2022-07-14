open Kernel.Io.RawBuffer

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

  let pixel4_to_string =
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
    pixels = Kernel.Io.RawBuffer.create @@ m.width * m.height * Formats.BytesIn.rgba
  }

  let byte_offset_from_xy img x y =
    let w = img.info.width * Formats.BytesIn.rgba
    and h = img.info.height in
    w*(h-y-1) + x * Formats.BytesIn.rgba

  let set img ~x ~y px =
    let open Formats in
    let ofs = byte_offset_from_xy img x y in
    let set_ba i = Bigarray.Array1.set img.pixels (ofs+i) in
    List.iteri (fun i -> set_ba i)
    (* [ px.a; px.r; px.g; px.b ] *)
    [ px.r; px.g; px.b; px.a ]


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
