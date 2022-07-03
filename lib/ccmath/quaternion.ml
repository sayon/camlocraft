open Field
open Vector

(** Type of modules implementing operations on quaternions. *)
module type QuaternionType = sig
  module F:FieldType
  type elt = F.elt
  type quat
  type unop = quat -> quat
  type binop = quat -> quat -> quat

  val add: binop
  val inverse: unop
  val neg: unop
  val sub: binop
  val mul_n: elt -> quat -> quat
  val mul_q: binop
  val conj: unop

  val x: quat -> elt
  val y: quat -> elt
  val z: quat -> elt
  val w: quat -> elt

  val mk_quat: elt-> elt-> elt-> elt-> quat
  val id : quat
  val to_array: quat -> elt array
  val from_array: elt array -> quat
  val zipWith: (elt -> elt -> elt) -> quat -> quat -> quat
  val map: (elt -> elt) -> quat -> quat

  val length_sq: quat -> elt
end
(** Functor to construct vector operations over a specific field.
The order is: 1, i, j, k
*)
module Make_Quaternion(F:FieldType) : QuaternionType with module F := F =
struct
  type elt = F.elt
  type quat = { x: elt; y:elt; z:elt; w:elt}
  type unop = quat -> quat
  type binop = quat -> quat -> quat

  let x q = q.x
  let y q = q.y
  let z q = q.z
  let w q = q.w

  let mk_quat x y z w = { x=x; y=y; z=z; w=w }
  include Field.Ops(F)

  let msg_quaternion_malformed = "Quaternion is malformed"

  let zipWith f (v1:quat) (v2:quat) = match v1, v2 with
    | {x=x1; y=y1; z=z1; w=w1} , {x=x2; y=y2; z=z2;w=w2} ->
      { x = f x1 x2; y = f y1 y2; z = f z1 z2; w = f w1 w2 }

  let map f = function
    | {x; y; z; w} -> { x = f x; y = f y; z = f z; w=f w }
  let to_array v = [| v.w; v.x; v.y; v.z|]

  let from_array v = match v with
    | [| x; y; z; w |] -> { x; y; z; w }
    | _ -> raise @@ Invalid_argument msg_quaternion_malformed


  let add a b = {
    x = a.x + b.x;
    y = a.y + b.y;
    z = a.z + b.z;
    w = a.w + b.w;
  }

  let neg a = {
    x = - a.x;
    y = - a.y;
    z = - a.z;
    w = - a.w;
  }

  let length_sq q =
    q.x * q.x + q.y * q.y + q.z * q.z + q.w * q.w

  let inverse q =
    let ls = length_sq q in
    {
      x = -q.x / ls ;
      y = -q.y / ls ;
      z = -q.z / ls ;
      w = q.w / ls
    }

  (* let from_vec3 x y z = { x = x; y = y; z = z; w = F.zero}  *)

  let conj = function
    | {x; y; z; w} -> { x = - x; y = - y; z = - z; w}

  let sub x y = add x (neg y)


  let mul_n n q =
    {
      x = q.x * n;
      y = q.y * n;
      z = q.z * n;
      w = q.w * n
    }

  let mul_q q1 q2 =
    match q1, q2 with
    | {w = a1; x = b1; y = c1; z = d1}, {w = a2; x = b2; y = c2; z = d2} ->
      {
        w = a1 * a2 - b1 * b2 - c1 * c2 - d1 * d2;
        x = a1 * b2 + b1 * a2 + c1 * d2 - d1 * c2;
        y = a1 * c2 - b1 * d2 + c1 * a2 + d1 * b2;
        z = a1 * d2 + b1 * c2 - c1 * b2 + d1 * a2
      }

  let id  = { x = F.zero; y = F.zero; z = F.zero; w = F.one }

end

module QuaternionF = struct
  include ((Make_Quaternion(FieldFloat)): QuaternionType with module F:= FieldFloat)

  let length q = sqrt @@ length_sq q
  let normalize q = map (fun c -> c /. length q) q

end

module Make_Vector4_Quaternion_Ops (F:FieldType) (V: Vector4Type with module F := F)  (Q: QuaternionType with module F := F) =
struct
  type quat = Q.quat
  type vec4 = V.vec4

  let mul_vec_quat (_:vec4) (_:Q.quat) : V.vec4 =  raise @@ Assert_failure ("mul_vec_quat not implemented",0,0)

end


module QuaternionF_Ops = struct
  open QuaternionF
  open Vector3F

  let from_axis_angle ~(axis:vec3) ~angle  =
    let s = sin @@ angle *. 0.5
    and c = cos @@ angle *. 0.5 in
    mk_quat
      (s *. x axis)
      (s *. y axis)
      (s *. z axis)
      c
  (** A quaternion encoding rotations around Y (yaw), X (pitch) and Z (roll). *)
  let from_yaw_pitch_roll ~yaw ~pitch ~roll =
    let sr = sin (roll*.0.5)
    and cr = cos (roll*.0.5)
    and sp = sin (pitch*.0.5)
    and cp = cos (pitch*.0.5)
    and sy = sin (yaw*.0.5)
    and cy = cos (yaw*.0.5)
    in
    mk_quat
    ( cy *. sp *. cr +. sy *. cp *. sr )
    ( sy *. cp *. cr -. cy *. sp *. sr )
    ( cy *. cp *. sr -. sy *. sp *. cr )
    ( cy *. cp *. cr +. sy *. sp *. sr )

  let rotate (v:vec3) (q:quat) =
    let qconj = conj q and q2v = mk_quat (x v) (y v) (z v) 0.0 in
    let r = mul_q (mul_q q q2v) qconj in
    mk_vec3 (QuaternionF.x r) (QuaternionF.y r) (QuaternionF.z r)

end
