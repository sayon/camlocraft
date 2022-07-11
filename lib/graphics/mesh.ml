open struct
  module Gl = Tgl4.Gl
end

open Opengl_ext
open Bigarray
open Geometry
open Programs


type mesh_type =
    MeshTriangles
  | MeshTrianglesFan
  | MeshTrianglesStrip
  | MeshTrianglesAdjacency

let mesh_type_opengl = function
  | MeshTriangles -> Gl.triangles
  | MeshTrianglesFan -> Gl.triangle_fan
  | MeshTrianglesStrip -> Gl.triangle_strip
  | MeshTrianglesAdjacency -> Gl.triangles_adjacency


type mesh = {
  vao: vertex_array_object;
  (* This field may be useless and we'd better let GC collect this bigarray. *)
  vertices: (float, float32_elt, c_layout) Array1.t;
  mesh_type: mesh_type;
  mesh_count: int (** Parameter for glDrawArrays *);
  program: Programs.program;
  mv_id: int;
  mvp_id: int;
  mv_matrix: matrix4;
  mvp_matrix: matrix4
}

let mesh_from_triangles prog layout (ts:vertex triangle array) =
  let module M = Ccmath.Matrix.Matrix4F in
  let module V = Ccmath.Vector.Vector3F in
  let module VM = Ccmath.Matrix.Vector4FMatrixOps in
  let ba = triangles_to_bigarray layout ts in
  let (vao, _) = gen_vertex_array
      ~layout: layout
      ~vertex_buffer: ba
  and mvp_id = (Programs.Uniform.get_by_name prog "u_MVP" )
  and mv_id  = (Programs.Uniform.get_by_name prog "u_MV" )
  and model = M.id
  and view = VM.world
      (* TODO Sample values, to be refactored *)
      ~position:(V.mk_vec3 2.0 2.0 3.0)
      ~target:(V.mk_vec3 0.0 0.0 0.0)
      ~up_vector:Geometry.VectorCollection.up3
  and projection = VM.perspective
      ~input:(`Fov (Geometry.Conversions.radians_of_degrees 97.0, 4.0/.3.0))
      ~near_plane: 0.1 ~far_plane: 100.0 in
  {
    vao = vao;
    vertices = ba;
    mesh_type = MeshTriangles;
    mesh_count = Array.length ts * (triangle_components layout);
    program = prog;
    mv_id = mv_id.uniform_id;
    mvp_id = mvp_id.uniform_id;
    mv_matrix = (let ( * ) = M.mul in view * model);
    mvp_matrix = (let ( * ) = M.mul in projection * view * model)
  }
