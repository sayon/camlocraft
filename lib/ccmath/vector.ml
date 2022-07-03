open Field

(** A common module type for vectors of any dimension. *)
module type VectorType = sig
  module F:FieldType
  type elt = F.elt
  type vec
  type unop = vec -> vec
  type binop = vec -> vec -> vec
  val add: binop
  val neg: unop
  val sub: binop
  val mul: elt -> vec -> vec
  val id : vec
  val to_array: vec -> elt array
  val from_array: elt array -> vec

  val dot: vec -> vec -> elt

  val length_sq : vec -> elt
  val length : vec -> elt

  val get: vec -> int -> elt

  val zipWith: (elt -> elt -> elt) -> vec -> vec -> vec
  val map: (elt -> elt) -> vec -> vec
end


(** Type of modules defining implementations of three-dimensional vectors. *)
module type Vector3Type = sig
  module F:FieldType
  type vec3
  include VectorType with module F := F and type vec := vec3

  val x: vec3 -> elt
  val y: vec3 -> elt
  val z: vec3 -> elt
  val mk_vec3: elt -> elt -> elt -> vec3

  val cross: binop
  val normalize: unop
end

(** Functor to construct vector operations over a specific field. *)
module Make_Vector3 (F:FieldType) : Vector3Type with module F := F =
struct
  include Field.Ops(F)
  type elt = F.elt
  type vec3 =  { x:elt; y:elt; z:elt }
  type unop = vec3 -> vec3
  type binop = vec3 -> vec3 -> vec3

  let mk_vec3 x y z = {x;y;z}


  let x v = v.x
  let y v = v.y
  let z v = v.z

  let zipWith f (v1:vec3) (v2:vec3) = match v1, v2 with
    | {x=x1; y=y1; z=z1} , {x=x2; y=y2; z=z2} -> { x = f x1 x2; y = f y1 y2; z = f z1 z2 }

  let map f = function
    | {x; y; z} -> {x = f x; y = f y; z = f z}

  let add = zipWith (+)
  let sub = zipWith (-)
  let neg = map (~-)

  let mul n = map ( ( * ) n)

  let id  = {x=F.one; y=F.one; z=F.one}

  let to_array v = [| v.x; v.y; v.z |]
  let from_array v =  { x=  v.(0); y = v.(1); z = v.(2) }

  let dot x y = match x,y with
    | {x=a1; y=a2; z=a3} , {x=b1; y=b2; z=b3} -> a1*b1 + a2*b2 + a3*b3

  let cross x y = match x,y with
    | {x=a1; y=a2; z=a3} , {x=b1; y=b2; z=b3} ->
      mk_vec3 (a2*b3 -a3*b2) (a3*b1-a1*b3) (a1*b1-a2*b1)

  let length_sq x = dot x x
  let length v = F.sqrt @@ length_sq v

  let normalize v = map (fun c -> c / length v) v

  let get v i = match v with | {x; y; z} ->
  match i with
  | 0-> x
  | 1-> y
  | 2-> z
  | _-> raise @@ Invalid_argument "Index out of range for vector3"

end

module Vector3F = ((Make_Vector3(FieldFloat)): Vector3Type with module F:= FieldFloat)
(** Type of modules implementing operations on 4D vectors.*)
module type Vector4Type = sig
  module F:FieldType
  type vec4
  include VectorType with module F := F and type vec := vec4

  val x: vec4 -> elt
  val y: vec4 -> elt
  val z: vec4 -> elt
  val w: vec4 -> elt
  val mk_vec4: elt -> elt -> elt -> elt -> vec4

  val normalize_hom: unop
  val is_normalized_hom: vec4 -> bool

end

(** Functor to construct vector operations over a specific field. *)
module Make_Vector4 (F:FieldType) : Vector4Type with module F := F =
struct
  include Field.Ops(F)
  type elt = F.elt
  type vec4 = {x:elt; y:elt; z:elt; w:elt }
  type unop = vec4 -> vec4
  type binop = vec4 -> vec4 -> vec4

  let x v = v.x
  let y v = v.y
  let z v = v.z
  let w v = v.w

  let mk_vec4 x y z w = {x;y;z;w}

  let zipWith f (v1:vec4) (v2:vec4) = match v1, v2 with
    | {x=x1; y=y1; z=z1; w=w1} , {x=x2; y=y2; z=z2;w=w2} ->
      { x = f x1 x2; y = f y1 y2; z = f z1 z2; w = f w1 w2 }

  let map f = function
    | {x; y; z; w} -> { x = f x; y = f y; z = f z; w=f w }

  let add = zipWith (+)
  let sub = zipWith (-)
  let neg = map F.neg
  let dot x y = match x,y with
    | {x=a1; y=a2; z=a3; w=a4} , {x=b1; y=b2; z=b3; w=b4} -> a1*b1 + a2*b2 + a3*b3 + a4*b4

  let normalize_hom v = if v.w == F.zero then v else map (fun x -> x / v.w) v
  let is_normalized_hom v = v.w == F.zero || v.w == F.one

  let mul n = map ( ( * ) n)

  let id : vec4 = { x = F.one; y = F.one; z = F.one; w= F.one }

  let to_array v = [| v.x; v.y; v.z; v.w |]
  let from_array v =  { x =  v.(0); y = v.(1); z = v.(2); w = v.(3) }

  let length_sq x = dot x x
  let length v = F.sqrt @@ length_sq v

  let get v i = match v with | {x; y; z; w} ->
      match i with
      | 0-> x
      | 1-> y
      | 2-> z
      | 3-> w
      | _-> raise @@ Invalid_argument "Index out of range for vector4"
end

module Vector4F = ((Make_Vector4(FieldFloat)): Vector4Type with module F:= FieldFloat)

module type VectorInteractionsType = sig
  module F:FieldType
  module V3 : Vector3Type with module F := F
  module V4 : Vector4Type with module F := F
  type vec4=V4.vec4
  type vec3=V3.vec3
  val of_vec3: vec3 -> vec4
  val of_vec4: vec4 -> vec3 option
end

module VectorInteractions
    (F:FieldType)
    (V3:Vector3Type with module F:=F)
    (V4:Vector4Type with module F:=F)
  : VectorInteractionsType with module F:=F and module V4:=V4 and module V3:=V3=
struct
  include Field.Ops(F)
  type vec4 = V4.vec4
  type vec3 = V3.vec3

  let of_vec3 v = V4.mk_vec4 (V3.x v) (V3.y v) (V3.z v) F.one

  let of_vec4 v = if
    F.equal (V4.w v) F.zero ||
    F.equal (V4.w v) F.one
    then Some (V3.mk_vec3 (V4.x v) (V4.y v) (V4.z v))
    else None
end
