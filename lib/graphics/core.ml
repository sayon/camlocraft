open struct
  module Gl = Tgl4.Gl
end
open Bigarray_ext
open Util

type shader_program = int
type vertex_array_object = int
type vertex_buffer_object = int

type graphics_ctx = {
  window: GLFW.window ;
  program: shader_program ;
}

(** Returns a VBO id containing all vertices. *)
let gen_vertex_buffer ~vertex_buffer=
  let bufferID = getThroughTempBuffer @@ Gl.gen_buffers 1 in
  Gl.bind_buffer Gl.array_buffer bufferID;
  let floatSize = Bigarray.kind_size_in_bytes Bigarray.float32 in
  let vertexBufferLength = Bigarray.Array1.dim vertex_buffer* floatSize  in
  Gl.buffer_data Gl.array_buffer vertexBufferLength (Some vertex_buffer) Gl.static_draw;
  Gl.bind_buffer Gl.array_buffer 0;
  bufferID

(** Returns a VAO id containing all vertices. *)
let gen_vertex_array ~vertex_buffer : (vertex_array_object* vertex_buffer_object)=
  let vao = getThroughTempBuffer @@ Gl.gen_vertex_arrays 1 in
  Gl.bind_vertex_array  vao;

  let vbo = gen_vertex_buffer ~vertex_buffer:vertex_buffer in

  Gl.bind_buffer Gl.array_buffer vbo;

  let stride = 3 * kind_size_in_bytes float32 in
  Gl.vertex_attrib_pointer 0 3 Gl.float false stride (`Offset 0) ;
  Gl.vertex_attrib_pointer 0 3 Gl.float false 0 (`Offset 0) ;
  Gl.enable_vertex_attrib_array 0;

  Gl.bind_buffer Gl.array_buffer 0;

  Gl.bind_vertex_array 0;
  (vao,vbo)

(** Returns a VBO id containing all vertices. *)
let genVertexBuffer ~vertices =
  let bufferID = getThroughTempBuffer @@ Gl.gen_buffers 1 in
  Gl.bind_buffer Gl.array_buffer bufferID;
  let vertexBuffer = createFloats vertices in
  let floatSize = Bigarray.kind_size_in_bytes Bigarray.float32 in
  let vertexBufferLength = Array.length vertices * floatSize  in
  Gl.buffer_data Gl.array_buffer vertexBufferLength (Some vertexBuffer) Gl.static_draw;
  Gl.bind_buffer Gl.array_buffer 0;
  bufferID

(** Returns a VAO id containing all vertices. *)
let genVertexArray ~vertices : (vertex_array_object* vertex_buffer_object)=
  let vao = getThroughTempBuffer @@ Gl.gen_vertex_arrays 1 in
  Gl.bind_vertex_array  vao;

  let vbo = genVertexBuffer ~vertices:vertices in

  Gl.bind_buffer Gl.array_buffer vbo;

  let stride = 3 * kind_size_in_bytes float32 in
  Gl.vertex_attrib_pointer 0 3 Gl.float false stride (`Offset 0) ;
  Gl.vertex_attrib_pointer 0 3 Gl.float false 0 (`Offset 0) ;
  Gl.enable_vertex_attrib_array 0;

  Gl.bind_buffer Gl.array_buffer 0;

  Gl.bind_vertex_array 0;
  (vao,vbo)

