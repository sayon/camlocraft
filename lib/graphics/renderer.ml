open struct
  module Gl = Tgl4.Gl
  module Matrix4F = Ccmath.Matrix.Matrix4F
  module Vector4F = Ccmath.Vector.Vector4F
end

open Mesh
open Opengl_ext
open Kernel.Logger
open Vector4F

type matrix4 = Ccmath.Matrix.Matrix4F.matrix4
type vec4 = Ccmath.Vector.Vector4F.vec4
type color = vec4


type renderer = { mutable meshes: mesh list }

type graphics_ctx = {
  window: GLFW.window ;
  renderer: renderer
}


let load_meshes () : renderer =
  log GeneralLog "Loading meshes" ;
  begin
  match Bmp.image_of_file "data/textures/sand.bmp"  with
    | Some img ->
      Kernel.Logger.log Kernel.Logger.GeneralLog "File loaded successfully";
      let texture = Texture.texture_of_image (img) in
      (* Texture.bind ()  ; *)
      let shader_program = Programs.Collection.sample_program () in
      { meshes = [
            mesh_from_triangles shader_program Geometry.Cube.cube_one 
              texture
          ] }
   | None -> failwith "Can't open the image"
end;



module Colors = struct
  let background = Vector4F.mk_vec4 0.0 0.0 0.0 1.0
end

let clear_color v = Gl.clear_color (x v) (y v) (z v) (w v)

let render renderer =
  clear_color Colors.background;
  List.iter (fun m ->
      Gl.uniform_matrix4fv m.mvp_id 1 false @@ Matrix4F.to_bigarray m.mvp_matrix;
      Gl.uniform_matrix4fv m.mv_id 1 false @@ Matrix4F.to_bigarray m.mv_matrix;
      Gl.use_program @@ m.program.id.value;
      Gl.bind_vertex_array m.vao.value;
      Gl.draw_arrays (mesh_type_opengl m.mesh_type) 0 m.mesh_count;
      Gl.bind_texture Gl.texture_2d m.texture.id.id;
      Gl.flush ()
    )
    @@ renderer.meshes

