open struct
  module Gl = Tgl4.Gl
end
open Kernel.Io.Bigarray_ext

type vertex_array_object = { value: int}
type vertex_buffer_object = { value: int }

type vertex_array_format =  Vertices | VerticesAndNormals | VerticesAndNormalsAndTexture

(** Returns a VBO id containing all vertices. *)
let gen_vertex_buffer ~vertex_buffer : vertex_buffer_object =
  let buffer_id = get_through_buffer @@ Gl.gen_buffers 1 in
  Gl.bind_buffer Gl.array_buffer buffer_id;
  let float_size = Bigarray.kind_size_in_bytes Bigarray.float32 in
  let length = (Bigarray.Array1.dim vertex_buffer) * float_size  in
  Gl.buffer_data Gl.array_buffer length (Some vertex_buffer) Gl.static_draw;
  Gl.bind_buffer Gl.array_buffer 0;
  { value = buffer_id }

let stride layout = Bigarray.kind_size_in_bytes Bigarray.float32 *
    match layout with
    | Vertices -> 3
    | VerticesAndNormals->6
    | VerticesAndNormalsAndTexture->8

(** Returns a VAO id containing all vertices. *)
let gen_vertex_array ~layout ~vertex_buffer : (vertex_array_object  * vertex_buffer_object) =
  let stride = stride layout in
  let vao = get_through_buffer @@ Gl.gen_vertex_arrays 1 in
  Gl.bind_vertex_array  vao;
  let vbo = gen_vertex_buffer ~vertex_buffer:vertex_buffer in
  Gl.bind_buffer Gl.array_buffer vbo.value;
  begin
  match layout with
   | Vertices ->
     Gl.enable_vertex_attrib_array 0;
     Gl.vertex_attrib_pointer 0 3 Gl.float false stride (`Offset 0) ;
   | VerticesAndNormals->
     Gl.enable_vertex_attrib_array 0;
     Gl.vertex_attrib_pointer 0 3 Gl.float false stride (`Offset 0) ;
     Gl.enable_vertex_attrib_array 1;
     Gl.vertex_attrib_pointer 1 3 Gl.float true stride (`Offset 0)
   | VerticesAndNormalsAndTexture ->
     Gl.enable_vertex_attrib_array 0;
     Gl.vertex_attrib_pointer 0 3 Gl.float false stride (`Offset 0) ;
     Gl.enable_vertex_attrib_array 1;
     Gl.vertex_attrib_pointer 1 3 Gl.float true stride (`Offset 0);
     Gl.enable_vertex_attrib_array 2;
     Gl.vertex_attrib_pointer 2 2 Gl.float false stride (`Offset 0)
  end;
  Gl.bind_buffer Gl.array_buffer 0;
  Gl.bind_vertex_array 0;
  ({value=vao},vbo)
