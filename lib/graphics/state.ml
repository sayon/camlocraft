open Kernel.Logger

open Mesh
open Camera

type matrix4 = Ccmath.Matrix.Matrix4F.matrix4
type window = GLFW.window

(** Global graphics state. Use {!Kernel.Facet} to interface with its parts. *)
type state = {
  window: window;
  mutable meshes: mesh list;
  mutable camera: camera;
  projection_matrix: matrix4
}

let load_meshes () : mesh list =
  log GeneralLog "Loading meshes" ;
  [
    let atlas = Texture.AtlasFragment.load "data/textures/sand.bmp" [["sand"]] 512
    and shader_program = Programs.Collection.sample_program () in
    mesh_from_triangles shader_program Geometry.Cube.cube_one atlas "sand"
  ]

let init ~(window:window): state =
  let open Tgl4.Gl in
  enable depth_test;
  depth_func less;

  let module VMO = Ccmath.Matrix.Vector4FMatrixOps in
  {
    window ;
    meshes = load_meshes ();
    camera = Camera.default;
    projection_matrix = VMO.perspective
        ~input:(`Fov (Geometry.Conversions.radians_of_degrees 45.0, 4.0/.3.0))
        ~near_plane: 0.1 ~far_plane: 100.0
  }
