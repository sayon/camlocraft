(** Implementation of simple vector/matrix operations for 4D vectors and 4x4 matrices.
    Matrices are represented as arrays of arrays.
    Since we perform most computations in GPU using shaders, we do not need high
    performance; however we may need to use different types inside matrices,
    like `float32` or `float`.
*)


(** Field is an algebraic structure on top of a set with two binary operations,
    which is a group w.r.t one operation and a monoid w.r.t. the other operation. *)
module type FieldType = sig
  type elt
  type unop = elt -> elt
  type binop = elt -> elt -> elt

  val one : elt
  val zero : elt
  val add : binop
  val sub : binop
  val neg : unop
  val mul : binop
  val div : binop

end

(** Type of modules implementing operations on 4D vectors.*)
module type Vector4Type = sig
  module F:FieldType
  type elt = F.elt
  type vec4
  type unop = vec4 -> vec4
  type binop = vec4 -> vec4 -> vec4

  val add: binop
  val inverse: unop
  val sub: binop
  val normalize: unop
  val is_normalized: vec4 -> bool
  val mul: elt -> vec4 -> vec4

  val get: int -> vec4 -> elt
  val set: int -> elt -> vec4 -> unit

  val id : vec4
  val to_array: vec4 -> elt array
  val from_array: elt array -> vec4
end

(** Functor to construct vector operations over a specific field. *)
module Make_Vector4 (F:FieldType) : Vector4Type with module F := F =
struct
  type elt = F.elt
  type vec4 = elt array
  type unop = vec4 -> vec4
  type binop = vec4 -> vec4 -> vec4

  let add = Array.map2 F.add

  let inverse = Array.map (fun x -> F.neg x)

  let sub x y = add x (inverse y)

  let normalize v = if v.(3) == F.zero then v else Array.map (fun x -> F.div x v.(3)) v

  let is_normalized v = v.(3) == F.zero || v.(3) == F.one

  let mul n = Array.map (fun c -> F.mul c n)
  let id  = Array.make 4 F.one

  let get i v = Array.get v i
  let set i e v  = Array.set v i e
  let to_array v = v
  let from_array v = v


end

(** Module type for 4x4 matrices. *)
module type Matrix4Type = sig
  module F:FieldType
  module V:Vector4Type with module F:= F
  type elt = F.elt
  type vec4 = V.vec4
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
  val mul_vec: matrix4 -> vec4 -> vec4

  val transpose: matrix4 -> matrix4
end

(** Module for arithmetics on 4x4 matrices.
    Since we perform most computations in GPU using shaders, we only need to form initial *)
module Make_Matrix4 (F:FieldType) (V:Vector4Type with module F:=F) =
struct
  type elt = F.elt
  type vec4 = V.vec4
  type unop = vec4 -> vec4
  type binop = vec4 -> vec4 -> vec4
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
        copy.(i).(j) <-  F.add m1.(i).(j) m2.(i).(j)
      done
    done;
    copy

  let add_elem m e =
    let copy = create () in
    for i = 0 to 3 do
      for j = 0 to 3 do
        copy.(i).(j) <-  F.add m.(i).(j) e
      done
    done;
    copy

  let sub (m1: matrix4) (m2: matrix4) : matrix4 =
    let copy = create () in
    for i = 0 to 3 do
      for j = 0 to 3 do
        copy.(i).(j) <-  F.sub m1.(i).(j) m2.(i).(j)
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
            copy.(i).(j) <- F.add copy.(i).(j) (F.mul (m1.(a).(j)) (m2.(i).(a)))
          done
      done
    done;
    copy

  let mul_vec (m:matrix4) (v:V.vec4) : V.vec4 =
    let result = Array.make 4 F.zero in
    for i = 0 to 3 do
      for j = 0 to 3 do
        result.(i) <- F.add result.(i) @@
          F.mul (m.(i).(j)) (V.get j v)
      done
    done;
    V.from_array result

  let transpose m =
    for i = 0 to 3 do
      for j = 0 to i do
        let t = m.(i).(j) in
        m.(i).(j) <- m.(j).(i);
        m.(j).(i) <- t
      done
    done


end


(** Floats form a quasi-field. Machine arithmetic defies laws of fields such as
    commutativity, so this is an approximation.*)
module FieldFloat : FieldType with type elt = float =
struct
  type elt = float
  type unop = float -> float
  type binop = float -> float -> float
  let one = 1.0
  let add x y = x +. y
  let sub x y = x -. y

  let zero = 0.0
  let mul x y = x *. y
  let div x y = x /. y
  let neg (x:float) = 0.0 -. x
end


module Vector4F = ((Make_Vector4(FieldFloat)): Vector4Type with module F:= FieldFloat)

module Matrix4F = struct
  include Make_Matrix4(FieldFloat)(Vector4F)

  let array_to_string string_of_elt =
    Array.fold_left (fun acc x -> acc ^ (string_of_elt x) ^ " ") ""

  let to_string (m:matrix4)=
    Array.fold_left (fun acc row -> acc ^ array_to_string string_of_float row ^ "\n") "" m

  (* TODO: rotation, translation, scale matrices *)

  (* let rotationFromAxisAngle angle = *)
  (*   [| *)
  (*     [| 1.0 ;    0.0     ;   0.0     ; 0.0  |]; *)
  (*     [| 0.0 ; cos angle  ; sin angle ; 0.0  |]; *)
  (*     [| 0.0 ; -. (sin angle) ; cos angle ; 0.0  |]; *)
  (*     [| 0.0 ;    0.0     ;   0.0     ; 1.0  |]; *)
  (*   |] *)

  (* let translation: float -> matrix4 = _ *)
  (* let scale: float -> matrix4 = _ *)

end
