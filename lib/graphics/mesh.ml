open struct
  module Gl = Tgl4.Gl
end

open Opengl_ext
open Ccmath.Matrix.Matrix4F
open Kernel.Io.RawBuffer
open Texture.AtlasFragment

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
  (* This field may be useless and we'd better let GC collect this bigarray?. *)
  vertices: raw_buffer;
  mesh_type: mesh_type;
  mesh_count: int (** Parameter for glDrawArrays *);
  program: Programs.program;
  (* TODO refactor: uniforms are now described as a part of the [program] *)
  (* m_id: int; *)
  (* v_id: int; *)
  (* p_id: int; *)
  model_matrix: matrix4;
  (* v_matrix: matrix4; *)
  (* p_matrix: matrix4; *)
  texture: Texture.AtlasFragment.fragment
}

open Geometry.Triangle
open Geometry.VertexDescription
open Geometry.Vertices

let mesh_from_triangles prog (ts:vertex_descr triangle list) atlas name =
  let module M = Ccmath.Matrix.Matrix4F in
  (* let module V = Ccmath.Vector.Vector3F in *)
  (* let module VM = Ccmath.Matrix.Vector4FMatrixOps in *)
  let ba = create_bigarray ts in
  let (vao, _) = gen_vertex_array ~vertex_buffer: ba in
  (* and m_id = (Programs.Uniform.get_by_name prog "u_M" ) *)
  (* and model = M.id              *)
  {
    vao = vao;
    vertices = ba;
    mesh_type = MeshTriangles;
    mesh_count = Stdlib.( * ) (List.length ts) 3;
    program = prog;
    model_matrix = M.id;
    (* mv_matrix = (let ( * ) = M.mul in view * model); *)
    (* mvp_matrix = (let ( * ) = M.mul in projection * view * model); *)
    texture = fragment_of_idx atlas (Texture.Atlas.coord_of atlas name)
  }
