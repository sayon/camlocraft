open Matrix.Matrix4F

(** Matrix operations *)

let meq =  forall_2 Float.equal
let%test _ = meq (add id id) (diag 2.0)
let%test _ = meq (add_elem
                    [|
                      [| 0.0; 0.0; 0.0; 0.0 |];
                      [| 0.0; 0.0; 0.0; 0.0 |];
                      [| 0.0; 0.0; 0.0; 0.0 |];
                      [| 0.0; 0.0; 0.0; 0.0 |];
                    |] 1.0)
    [|
      [| 1.0; 1.0; 1.0; 1.0 |];
      [| 1.0; 1.0; 1.0; 1.0 |];
      [| 1.0; 1.0; 1.0; 1.0 |];
      [| 1.0; 1.0; 1.0; 1.0 |];
    |]
let%test _ = meq (sub
                    [|
                      [| 4.0; 4.0; 4.0; 4.0 |];
                      [| 4.0; 9.0; 4.0; 4.0 |];
                      [| 4.0; 4.0; 4.0; 4.0 |];
                      [| 4.0; 4.0; 4.0; 4.0 |];
                    |]
                    [|
                      [| 1.0; 1.0; 1.0; 1.0 |];
                      [| 1.0; 1.0; 1.0; 1.0 |];
                      [| 1.0; 1.0; 1.0; 1.0 |];
                      [| 1.0; 1.0; 1.0; 1.0 |];
                    |])

    [|
      [| 3.0; 3.0; 3.0; 3.0 |];
      [| 3.0; 8.0; 3.0; 3.0 |];
      [| 3.0; 3.0; 3.0; 3.0 |];
      [| 3.0; 3.0; 3.0; 3.0 |];
    |]

let%test _ = let case =
               [|
                 [| 2.0;-1.0; 3.0; 5.0 |];
                 [| 1.0; 3.0; 0.0; 4.0 |];
                 [| 3.0; 0.0;-1.0;-2.0 |];
                 [| 0.0; 0.0; 0.0; 1.0 |];
               |] in
  meq (mul id case) case


let%test _ = let case =
               [|
                 [| 2.0;-1.0; 3.0; 5.0 |];
                 [| 1.0; 3.0; 0.0; 4.0 |];
                 [| 3.0; 0.0;-1.0;-2.0 |];
                 [| 0.0; 0.0; 0.0; 1.0 |];
               |] in
  meq (mul id case) case

let%test _ = let case =
               [|
                 [| 2.0;-1.0; 3.0; 5.0 |];
                 [| 1.0; 3.0; 0.0; 4.0 |];
                 [| 3.0; 0.0;-1.0;-2.0 |];
                 [| 0.0; 0.0; 0.0; 1.0 |];
               |] in
  meq (mul case case)
    [|
      [| 12.0;-5.0; 3.0; 5.0 |];
      [| 5.0; 8.0; 3.0; 21.0 |];
      [| 3.0;-3.0;10.0; 15.0 |];
      [| 0.0; 0.0; 0.0; 1.0 |];
    |]
module Vector3Tests = struct
open Vector.Vector3F

let%test _ =
  let v1 = mk_vec3 (-1.0) 0.0 1.0
  and v2 = mk_vec3 (-1.0) 0.0 0.0 in
  (* print_string @@ to_string @@ cross v1 v2; *)
  cross v1 v2 = mk_vec3 0.0 (-1.0) 0.0

end

(** Quaternion tests *)
open Quaternion.QuaternionF

let%test _ = let q = from_array [| 1.0; 2.0; 3.0; 4.0 |] in
  mul_q id q = q

let%test _ = let q = from_array [| 1.0; 2.0; 3.0; 4.0 |] in
  mul_q q id = q

let%test _ = inverse id = id


(* TODO vec3 tests, matrix vector operations, quaternion vector operations *)
