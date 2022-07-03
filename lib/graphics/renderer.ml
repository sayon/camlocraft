open struct
  module Gl = Tgl4.Gl
end

open Geometry
open Util
open Shaders

open Ccmath.Matrix
open Ccmath.Vector

type matrix4 = Ccmath.Matrix.Matrix4F.matrix4

type vertex_array_object = { value: int}
type vertex_buffer_object = { value: int }

type mesh_type =
    MeshTriangles
  | MeshTrianglesFan
  | MeshTrianglesStrip
  | MeshTrianglesAdjacency

let mesh_type_opengl = function
  | MeshTriangles -> Tgl4.Gl.triangles
  | MeshTrianglesFan -> Tgl4.Gl.triangle_fan
  | MeshTrianglesStrip -> Tgl4.Gl.triangle_strip
  | MeshTrianglesAdjacency -> Tgl4.Gl.triangles_adjacency


type mesh = {
  vao: vertex_array_object;
  (* This field may be useless and we'd better let GC collect this bigarray. *)
  vertices: (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t;
  mesh_type: mesh_type;
  mesh_count: int (** Parameter for glDrawArrays *);
  shader_program: shader_program;
  mvp_id: int;
  mvp_matrix: matrix4
}

type renderer = { meshes: mesh list ref }

type graphics_ctx = {
  window: GLFW.window ;
  renderer: renderer
}


(** Returns a VBO id containing all vertices. *)
let gen_vertex_buffer ~vertex_buffer : vertex_buffer_object =
  let buffer_id = getThroughTempBuffer @@ Gl.gen_buffers 1 in
  Gl.bind_buffer Gl.array_buffer buffer_id;
  let floatSize = Bigarray.kind_size_in_bytes Bigarray.float32 in
  let vertexBufferLength = (Bigarray.Array1.dim vertex_buffer) * floatSize  in
  Gl.buffer_data Gl.array_buffer vertexBufferLength (Some vertex_buffer) Gl.static_draw;
  Gl.bind_buffer Gl.array_buffer 0;
  { value = buffer_id }

(** Returns a VAO id containing all vertices. *)
let gen_vertex_array ~vertex_buffer : (vertex_array_object* vertex_buffer_object)=
  let vao = getThroughTempBuffer @@ Gl.gen_vertex_arrays 1 in
  Gl.bind_vertex_array  vao;

  let vbo = gen_vertex_buffer ~vertex_buffer:vertex_buffer in

  Gl.bind_buffer Gl.array_buffer vbo.value;

  let stride = 3 * Bigarray.kind_size_in_bytes Bigarray.float32 in
  Gl.vertex_attrib_pointer 0 3 Gl.float false stride (`Offset 0) ;
  Gl.vertex_attrib_pointer 0 3 Gl.float false 0 (`Offset 0) ;
  Gl.enable_vertex_attrib_array 0;

  Gl.bind_buffer Gl.array_buffer 0;

  Gl.bind_vertex_array 0;
  ({value=vao},vbo)

let mesh_from_triangles shader (ts:triangle array) =
  let ba = to_bigarray ts in
  let (vao, _) = gen_vertex_array ~vertex_buffer:ba in
  let mvp_id = Gl.get_uniform_location shader.id "MVP" in
  {
    vao = vao;
    vertices = ba;
    mesh_type = MeshTriangles;
    mesh_count = Array.length ts * 9;
    shader_program = shader;
    mvp_id = mvp_id;
    mvp_matrix =
      let model = Matrix4F.id
      and view = Vector4FMatrixOps.world
                         ~position:(Vector3F.mk_vec3 4.0 3.0 3.0)
                         ~target:(Vector3F.mk_vec3 0.0 0.0 0.0)
                         ~up_vector:(Vector3F.mk_vec3 0.0 1.0 0.0)
      and projection = Vector4FMatrixOps.perspective
          ~input:(`Fov (radians_of_degrees 45.0, 4.0/.3.0))
          ~near_plane: 0.1 ~far_plane: 100.0
          and  ( * ) = Matrix4F.mul
      in
      projection * view * model
  }

  (* // Send our transformation to the currently bound shader, in the "MVP" uniform *)
  (*                                                              // This is done in the main loop since each model will have a different MVP matrix (At least for the M part) *)
  (* glUniformMatrix4fv(MatrixID, 1, GL_FALSE, &mvp[0][0]); *)

let load_meshes () =
  let shader_program = createSampleProgram () in
  
  { meshes = ref [ mesh_from_triangles shader_program Geometry.cube_one ]
  }


let rec do_all f lst =
  match lst with
  | [] -> ()
  | x :: xs -> f x; do_all f xs


let render renderer =
  Gl.clear_color 0.0 0.0 0.0 1.0;
  do_all (fun m ->
      Gl.uniform_matrix4fv m.mvp_id 1 false @@ Ccmath.Matrix.Matrix4F.to_bigarray m.mvp_matrix;

      Gl.use_program m.shader_program.id;
      Gl.bind_vertex_array m.vao.value;
      Gl.draw_arrays (mesh_type_opengl m.mesh_type) 0 m.mesh_count;
      Gl.flush ()
    )
    @@ ! (renderer.meshes)

