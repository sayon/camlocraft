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

let from_image (image: Image.Image.image) (width:int) (height:int) =
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
  pixel_storei unpack_alignment 1;

  generate_mipmap texture_2d;
  bind_texture texture_2d 0;
  {
    id = {id};
    width = width;
    height = height;
    bpp = 0;
    path = image.info.name;
    buffer = image.pixels
  }


let bind () =
  Gl.active_texture @@ Gl.texture0;
  Gl.bind_texture Gl.texture_2d 0



module Atlas = struct

  module TextureMap = Map.Make(struct type t = string let compare = Stdlib.compare end)

  type atlas_idx = int * int
  type atlas = {
    width: int;
    height: int;
    tile_width: int;
    tile_height: int;
    map: atlas_idx TextureMap.t;
    texture: texture
  }

  let from_texture (texture:texture) (key_list: string list list) tile_dim : atlas =
    let kvp_seq = List.flatten @@
      List.mapi (fun i -> List.mapi (fun j key -> (key,(j,i)))) key_list in
    {
      width = texture.width;
      height = texture.height;
      tile_width = tile_dim;
      tile_height = tile_dim;
      map = TextureMap.of_seq (List.to_seq kvp_seq);
      texture
    }

  let coord_of atlas name : atlas_idx =
    TextureMap.find name atlas.map
end

module AtlasFragment = struct
  open Atlas

  type atlas_coord = float * float
  type fragment = {
    top_left:  atlas_coord;
    top_right: atlas_coord;
    bot_left:  atlas_coord;
    bot_right: atlas_coord;
    atlas: atlas
  }


  open struct
    let slots_h atlas = atlas.height / atlas.tile_height
    let slots_w atlas = atlas.width / atlas.tile_width

    let tile_width_tx  atlas = 1.0 /. (Float.of_int @@ slots_w atlas)
    let tile_height_tx atlas = 1.0 /. (Float.of_int @@ slots_h atlas)

    let top_left atlas (x,y) : atlas_coord =
      (Float.of_int x *. tile_width_tx atlas,
       Float.of_int (slots_h atlas - 1 - y) *. tile_height_tx atlas)

    let bot_left atlas (x,y) : atlas_coord =
      let fx,fy = top_left atlas (x,y) in
      (fx, fy +. tile_height_tx atlas)

    let top_right atlas (x,y) : atlas_coord =
      let fx,fy = top_left atlas (x,y) in
      (fx +. tile_width_tx atlas, fy)

    let bot_right atlas (x,y) : atlas_coord =
      let fx,fy = top_left atlas (x,y) in
      (fx +. tile_width_tx atlas, fy +. tile_height_tx atlas)
  end

  let fragment_of_idx atlas idx  : fragment =
    {
      top_left  = top_left  atlas idx;
      top_right = top_right atlas idx;
      bot_left  = bot_left  atlas idx;
      bot_right = bot_right atlas idx;
      atlas
    }

  let load filename keys tile =
    match Bmp.image_of_file filename with
    | Some img ->
      let texture = from_image img img.info.width img.info.height in
      from_texture texture keys tile
    | None ->
      failwith @@
      Printf.sprintf
        "Failed to create a texture atlas from an image file %s"
        filename
end
