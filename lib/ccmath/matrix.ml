(** Implementation of simple vector/matrix operations for 4D vectors and 4x4 matrices.
    Matrices are represented as arrays of arrays.
    Since we perform most computations in GPU using shaders, we do not need high performance; however we may need to use different types inside matrices,
    like `float32` or `float`.
*)


open Field
open Vector

(** Module type for 4x4 matrices. *)
module type Matrix4Type = sig
  module F:FieldType
  type elt = F.elt
  type matrix4

  val create: unit -> matrix4
  val get : matrix4 -> int -> int -> elt
  val set : matrix4 -> int -> int -> elt -> unit
  val add : matrix4 -> matrix4 -> matrix4
  val add_elem : matrix4 -> elt -> matrix4
  val sub : matrix4 -> matrix4 -> matrix4
  val id: matrix4
  val diag: elt -> matrix4
  val mul: matrix4 -> matrix4 -> matrix4

  val transpose: matrix4 -> matrix4

  val from_array_of_arrays: elt array array -> matrix4
end

(** Module for arithmetics on 4x4 matrices.
    Since we perform most computations in GPU using shaders, we only need to form initial *)
module Make_Matrix4 (F:FieldType) (V:Vector4Type with module F:=F) =
struct
  include Field.Ops(F)
  type elt = F.elt
  type matrix4 = elt array array


  let create () = Array.make_matrix 4 4 F.zero

  let get m i j = m.(i).(j)
  let set m i j e = m.(i).(j) <- e

  let forall_2 pred m1 m2 =
    try
      for i = 0 to 3 do
        for j = 0 to 3 do
          if not ( pred m1.(i).(j) m2.(i).(j) )
          then raise Exit
        done
      done;
      true
    with Exit -> false

  let clone m =
    let copy = create () in
    for i = 0 to 3 do
      for j = 0 to 3 do
        copy.(i).(j) <- m.(i).(j)
      done
    done

  let add (m1: matrix4) (m2: matrix4) : matrix4 =
    let copy = create () in
    for i = 0 to 3 do
      for j = 0 to 3 do
        copy.(i).(j) <-  m1.(i).(j) + m2.(i).(j)
      done
    done;
    copy

  let add_elem m e =
    let copy = create () in
    for i = 0 to 3 do
      for j = 0 to 3 do
        copy.(i).(j) <-  m.(i).(j) + e
      done
    done;
    copy

  let sub (m1: matrix4) (m2: matrix4) : matrix4 =
    let copy = create () in
    for i = 0 to 3 do
      for j = 0 to 3 do
        copy.(i).(j) <-  m1.(i).(j) - m2.(i).(j)
      done
    done;
    copy

  let diag e =
    [|
    [|e ; F.zero; F.zero; F.zero |];
    [|F.zero; e ; F.zero; F.zero |];
    [|F.zero; F.zero; e; F.zero |];
    [|F.zero; F.zero; F.zero; e |];
  |]

  let id: matrix4 = diag F.one

  let mul (m1:matrix4) (m2:matrix4) : matrix4 =
    let copy = create () in
    for i = 0 to 3 do
      for j = 0 to 3 do
        for a = 0 to 3 do
            copy.(i).(j) <- copy.(i).(j) + (m1.(a).(j)) * (m2.(i).(a))
          done
      done
    done;
    copy

  let transpose m =
    let copy = create () in
    for i = 0 to 3 do
      for j = 0 to 3 do
        copy.(i).(j) <- m.(j).(i)
      done
    done; copy


  let from_array_of_arrays x = x
end




module Matrix4F = struct
  include Make_Matrix4(FieldFloat)(Vector4F)

  let array_to_string string_of_elt =
    Array.fold_left (fun acc x -> acc ^ (string_of_elt x) ^ " ") ""

  let to_string (m:matrix4)=
    Array.fold_left (fun acc row -> acc ^ array_to_string string_of_float row ^ "\n") "" m

  let to_bigarray m =
    let ba = Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout 16 in
    let pos = ref 0 in
      for i = 0 to 3 do
        for j = 0 to 3 do
          Bigarray.Array1.unsafe_set ba !pos (get m i j);
          pos := Stdlib.(+) ! pos  1
        done
      done;
      ba
end

(** Operations on vectors and matrices that are field-independent. *)
module Make_VectorMatrixOps
    (F:FieldType)
    (V4: Vector4Type with module F := F)
    (V3: Vector3Type with module F := F)
    (M:Matrix4Type with module F:=F)  =
struct
  type matrix4 = M.matrix4
  type vec4 = V4.vec4
  type vec3 = V3.vec3
  include Field.Ops(F)


  let translation_component (m:matrix4) : vec3 =
    let x = M.get m 3 0 and y = M.get m 3 1 and z = M.get m 3 2 in
    V3.mk_vec3 x y z

  let mul_vec_mat (m:matrix4) (v:V4.vec4) : V4.vec4 =
    let result = Array.make 4 F.zero in
    for i = 0 to 3 do
      for j = 0 to 3 do
        result.(i) <- result.(i) + (M.get m i j) * (V4.get v j)
      done
    done;
    V4.from_array result

  let translation (v:vec3) =
    [|
      [| F.one; F.zero; F.zero; F.zero |];
      [| F.zero; F.one; F.zero; F.zero |];
      [| F.zero; F.zero; F.one; F.zero |];
      [| V3.x v; V3.y v; V3.z v; F.one |];
    |]


end

module Vector4FMatrixOps = struct
  include Make_VectorMatrixOps(FieldFloat)(Vector4F)(Vector3F)(Matrix4F)
  open Vector3F
  open Matrix4F


  let scale (scale:vec3) (center:vec3) =
    let m41 = (x center) * (1.0 -. x scale)
    and m42 = (y center)  * (1.0 -. y scale)
    and m43 = (z center) * (1.0 -. z scale) in
    [|
      [| x scale; 0.0; 0.0; 0.0 |];
      [| 0.0; y scale; 0.0; 0.0 |];
      [| 0.0; 0.0; z scale; 0.0 |];
      [| m41; m42; m43 ; 1.0 |];
    |]


  (** Compute rotation matrix for a given unit axis vector *)
  let rotation_axis ~radians ~axis_unit =
    (* Rotation matrix M is computed as follows:
       M = u * trans(u) + (cos a)( I-u*trans(u) ) + (sin a)S

       Where:

       1) I is an identity matrix
       2) u = ( x, y, z ) is an axis vector
       3) S is a matrix:

        [  0 -z  y ]
       S = [  z  0 -x ]
        [ -y  x  0 ]
    *)
    let x = x axis_unit and y = y axis_unit and z = Vector3F.z axis_unit in
    let sa = sin radians and ca = cos radians in
    let xx = x*x and yy = y*y and zz=z*z and xy=x*y and xz=x*z and yz=y*z in
    [|
      [| xx + ca * (1.0 - xx); xy - ca * xy + sa * z; xz - ca * xz - sa * y; 0.0 |];
      [| xy - ca * xy - sa * z; yy + ca * (1.0 - yy); yz - ca * yz + sa * x; 0.0 |] ;
      [| xz - ca * xz + sa * y; yz - ca * yz - sa * x; zz + ca * (1.0 - zz); 0.0 |];
      [| 0.0; 0.0; 0.0; 1.0 |]
    |]

(** Creates a perspective projection matrix based on a field of view (in the `y`
direction), aspect ratio (width/height), and near and far view plane distances.
*)
  let perspective ~input ~near_plane ~far_plane =
    let _perspective_scale_aux x_scale y_scale near_plane far_plane =
      if near_plane <= 0.0
      then raise @@ Invalid_argument "Distance to the near plane is not positive" else
      if far_plane <= 0.0
      then raise @@ Invalid_argument "Distance to the far plane is not positive" else
      if near_plane >= far_plane
      then raise @@ Invalid_argument "The far plane is closer than the near plane" else
        let m33 = far_plane /. (near_plane -. far_plane) in
        let m43 = near_plane *. far_plane /. (near_plane -. far_plane) in
        [|
          [| x_scale; 0.0; 0.0; 0.0 |];
          [| 0.0; y_scale; 0.0; 0.0 |];
          [| 0.0; 0.0; m33; -1.0 |];
          [| 0.0; 0.0; m43; 0.0 |];
        |]
    in
    match input with
    | `Fov (angle,ratio) ->
      if angle <= 0.0 || angle >= Float.pi
      then raise @@ Invalid_argument "Field of view angle is out of range [0;PI]" else
        let y_scale = 1.0 / tan (0.5 *. angle) in
        let x_scale = y_scale /. ratio in
        _perspective_scale_aux x_scale y_scale near_plane far_plane
    | `Dimensions (width,height) ->
      let y_scale =  2.0 *. near_plane /. height in
      let x_scale = 2.0 *. near_plane /. width in
      _perspective_scale_aux x_scale y_scale near_plane far_plane

(** Builds an orthographic projection matrix.
Accepts:
    - left, right -- minimum and maximum X-values of the view volume.
    - bottom, top -- minimum and maximum Y-values of the view volume.
    - z_near_plane, z_far_plane -- minimum and maximum Z-values of the view volume.
*)
  let orthographic ~left ~right ~bottom ~top ~z_near_plane ~z_far_plane =
    let w = right -. left and h = top -. bottom and dist = z_far_plane -. z_near_plane in
    let m41 = (left +. right) /. (left -. right)
    and m42 = (top +. bottom) /. (bottom -. top)
    and m43 = -. z_near_plane /. dist in
    [|
      [| 2.0 /. w; 0.0; 0.0; 0.0 |];
      [| 0.0; 2.0 /. h; 0.0; 0.0 |];
      [| 0.0; 0.0; 1.0 /. dist; 0.0 |];
      [| m41; m42; m43; 1.0 |]
    |]

(** Create a view matrix. *)
  let world ~position ~target ~up_vector =
    let zaxis = normalize (Vector3F.sub position target) in
    let xaxis = normalize (Vector3F.cross up_vector zaxis) in
    let yaxis = Vector3F.cross zaxis xaxis in
      [|
        [| x xaxis; x yaxis; x zaxis; 0.0 |];
        [| y xaxis; y yaxis; y zaxis; 0.0 |];
        [| z xaxis; z yaxis; z zaxis; 0.0 |];
        [| -. Vector3F.dot xaxis position ;
           -. Vector3F.dot yaxis position ;
           -. Vector3F.dot zaxis position ;
           1.0 |]
      |]

(* TODO *)
(*   let world (v:vec3) = *)

end

open Quaternion

module Make_Matrix4_Quaternion_Ops (F:FieldType) (M: Matrix4Type with module F := F)  (Q: QuaternionType with module F := F) =
struct
  include Field.Ops(F)

  type quat = Q.quat
  type matrix4 = M.matrix4
end


module MatrixF_QuaternionF_Ops = struct
  include Make_Matrix4_Quaternion_Ops(FieldFloat)(Matrix4F)(QuaternionF)
  open QuaternionF
  open Matrix4F

  let quat_to_matrix (q:quat) : matrix4 =
    let x = x q and y = y q and z = z q and w = w q in
    [|
      [| 1.0 -. 2.0 *. (y*.y+.z*.z); 2.0 *. (x*.y-.w*.z); 2.0 *. (x*.z-w*.y); 0.0 |];
      [| 2.0 *. ( x*.y -. w*.z ); 1.0 -. 2.0 *. ( z*.z +. x*.x ); 2.0 *. ( y*.z +. w*.x ); 0.0 |];
      [| 2.0 *. ( x*.z +. w*.y ); 2.0 *. ( y*.z -. w*.x ); 1.0 -. 2.0 *. ( y*.y +. x*.x ); 0.0 |];
      [| 0.0; 0.0; 0.0; 1.0 |]
    |]

end
