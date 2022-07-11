open Kernel.Io.RawBuffer
open Kernel.Io.Bigarray_ext
open Kernel.Image_loader

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

let texture_of_image (image: Image.image) =
  let open Gl in
  let id = get_through_buffer @@ gen_textures 1 in
  bind_texture texture_2d id;
  Gl.tex_parameteri Gl.texture_2d Gl.texture_min_filter Gl.linear;
  Gl.tex_parameteri Gl.texture_2d Gl.texture_mag_filter Gl.linear;
  Gl.tex_parameteri Gl.texture_2d Gl.texture_wrap_s     Gl.clamp_to_border;
  Gl.tex_parameteri Gl.texture_2d Gl.texture_wrap_t     Gl.clamp_to_border;

  Gl.tex_image2d Gl.texture_2d 0 Gl.rgba8 image.info.width image.info.height 0 Gl.rgba Gl.unsigned_byte (`Data image.pixels);
  Gl.bind_texture Gl.texture_2d 0;
  {
    id = {id};
    width = image.info.width;
    height = image.info.height;
    bpp = 0;
    path = image.info.name;
    buffer = image.pixels
  }


let bind slot texture =
  Gl.active_texture @@ Gl.texture0 + slot;
  Gl.bind_texture Gl.texture_2d texture.id.id
