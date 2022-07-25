open struct
  module Gl = Tgl4.Gl
  module Matrix4F = Ccmath.Matrix.Matrix4F
  module Vector4F = Ccmath.Vector.Vector4F
end

open Mesh
open Programs
open Opengl_ext
open Vector4F


let clear_color v = Gl.clear_color (x v) (y v) (z v) (w v)

let render_meshes (gs:State.state) =
  List.iter (fun m ->
      Gl.use_program @@ m.program.id.value;
      Uniform.set m.program "u_M" @@ `Matrix m.model_matrix;
      Uniform.set m.program "u_V" @@ `Matrix gs.camera.view_matrix;
      Uniform.set m.program "u_P" @@ `Matrix gs.projection_matrix;
      Gl.bind_vertex_array m.vao.value;
      Gl.bind_texture Gl.texture_2d m.texture.atlas.texture.id.id;
      Gl.draw_arrays (mesh_type_opengl m.mesh_type) 0 m.mesh_count;
      Gl.flush ()
    )
  @@ gs.meshes

let render_scene (graphics_state:State.state) : unit =
  Gl.clear(Gl.color_buffer_bit lor Gl.depth_buffer_bit );
  clear_color Colors.background;
  render_meshes graphics_state
