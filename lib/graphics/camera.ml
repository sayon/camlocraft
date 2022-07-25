open Ccmath.Vector.Vector3F
open Ccmath.Matrix.Matrix4F

open struct
  module V3 = Ccmath.Vector.Vector3F
end

type camera_mode = Freeway | Following

type camera = {
  mode: camera_mode;
  mutable position: vec3;
  mutable direction: vec3;
  mutable up: vec3;
  mutable view_matrix: matrix4;
  speed_scale: float
}

let update_matrix (c:camera) =
  let open Ccmath.Matrix.Vector4FMatrixOps in
  c.view_matrix <- world
      ~position:c.position
      ~target:(V3.add c.position c.direction)
      ~up_vector:c.up

let mk_camera  ~position ~direction ~up ~mode ~speed_scale =
  let open Ccmath.Matrix.Vector4FMatrixOps in
  {
    mode;
    position;
    direction;
    up = up;
    view_matrix = world
        ~position:position
        ~target:(V3.add position direction)
        ~up_vector:up;
    speed_scale
  }

let move camera delta : unit =
  camera.position <- V3.add camera.position delta;
  update_matrix camera

let move_fwd camera elapsed : unit =
  move camera (V3.mul (camera.speed_scale *. elapsed) camera.direction)

let move_bwd camera elapsed : unit =
  move camera (V3.mul (-. camera.speed_scale *. elapsed) camera.direction)

let dir_right ~up ~dir =
  normalize @@ cross dir up

let dir_left ~up ~dir =
  V3.mul (-1.0) @@ dir_right ~up:up ~dir:dir

let move_right camera elapsed : unit =
  move camera (V3.mul (camera.speed_scale *. elapsed) (dir_right ~up:camera.up ~dir:camera.direction))

let move_left camera elapsed : unit =
  move camera (V3.mul (camera.speed_scale *. elapsed) (dir_left ~up:camera.up ~dir:camera.direction))

let default : camera =  mk_camera
    ~position:(mk_vec3 2.0 2.0 2.0)
    ~direction:(mk_vec3 (-1.0) (-1.0) (-1.0))
    ~up: Geometry.VectorCollection.up3
    ~mode: Freeway
    ~speed_scale: 10000.0
