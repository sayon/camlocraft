open Kernel.Io.RawBuffer
open Kernel.Bigarray_ext

open struct
  module Gl = Tgl4.Gl
end

type texture_id = { id: int }

type texture = {
  id: texture_id;
  width: int;
  height: int;
  bpp: int;
  path: string;
  buffer: raw_buffer
}

let texture_of_image (image: Image.Image.image) =
  let open Gl in
  let id = get_through_buffer @@ gen_textures 1 in
  bind_texture texture_2d id;
  tex_parameteri texture_2d texture_min_filter linear_mipmap_linear;
  tex_parameteri texture_2d texture_mag_filter linear;
  tex_parameteri texture_2d texture_wrap_s     clamp_to_border;
  tex_parameteri texture_2d texture_wrap_t     clamp_to_border;
  tex_image2d texture_2d 0 rgba
    image.info.width
    image.info.height
    0 rgba unsigned_byte (`Data image.pixels);
  generate_mipmap texture_2d;
  bind_texture texture_2d 0;
  {
    id = {id};
    width = image.info.width;
    height = image.info.height;
    bpp = 0;
    path = image.info.name;
    buffer = image.pixels
  }


let bind () =
  Gl.active_texture @@ Gl.texture0;
  Gl.bind_texture Gl.texture_2d 0
