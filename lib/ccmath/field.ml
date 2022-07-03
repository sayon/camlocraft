(** Field is an algebraic structure on top of a set with addition and multiplication.
    All elements have additive inverses; all non-zero elements have multiplicative inverses.
*)
module type FieldType = sig
  type elt
  type pred = elt -> bool
  type pred2 = elt-> elt -> bool
  type unop = elt -> elt
  type binop = elt -> elt -> elt

  val one : elt
  val zero : elt
  val add : binop
  val sub : binop
  val neg : unop
  val mul : binop
  val div : binop
  (* This operation does not have to be defined for all field elements, of course. *)
  val sqrt: unop
  val equal: pred2
  val to_string: elt -> string
end

module Ops(F:FieldType) =
struct
  let neg x = F.sub F.zero x
  let (+) x1 x2 = F.add x1 x2
  let (-) x1 x2 = F.add x1 (neg x2)
  let ( * ) x1 x2 = F.mul x1 x2
  let ( / ) x1 x2 = F.div x1 x2
  let ( ~- ) x = F.zero - x
end

(** Floats form a quasi-field. Machine arithmetic defies laws of fields such as
    commutativity, so this is an approximation.*)
module FieldFloat : FieldType with type elt = float =
struct
  type elt = float
  type pred = float -> bool
  type pred2 = float-> float -> bool
  type unop = float -> float
  type binop = float -> float -> float
  let one = 1.0
  let add x y = x +. y
  let sub x y = x -. y

  let zero = 0.0
  let mul x y = x *. y
  let div x y = x /. y
  let neg x = 0.0 -. x
  let to_string = string_of_float
  let sqrt = Float.sqrt
  let equal = Float.equal
end
