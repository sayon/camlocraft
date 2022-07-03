open Ccmath.Vector.Vector3F

type vec3 = Ccmath.Vector.Vector3F.vec3
type vertex = vec3
type triangle = { a: vertex; b:vertex; c:vertex }

let mk_v x y z = mk_vec3 (float_of_int x) (float_of_int y) (float_of_int z)
(* Make a rectangle.
   1--2
   |  |
   3--4
*)
let mk_rect a1 a2 a3 a4 = [ {a = a1; b = a2; c = a3} ; {a = a2; b = a3; c = a4} ]

let mk_cube a1 a2 a3 a4 a5 a6 a7 a8 =
  Array.concat @@ List.map Array.of_list
    [
      mk_rect a1 a2 a3 a4;
      mk_rect a5 a6 a7 a8;
      mk_rect a1 a2 a5 a6;
      mk_rect a3 a4 a7 a8;
      mk_rect a1 a5 a3 a7;
      mk_rect a2 a4 a6 a8
    ]

let cube_one = mk_cube
    (mk_v 1 0 0)
    (mk_v 1 1 0)
    (mk_v 0 0 0)
    (mk_v 0 1 0)
    (mk_v 1 0 1)
    (mk_v 1 1 1)
    (mk_v 0 0 1)
    (mk_v 0 1 1)

open Bigarray.Array1
let set_vertex arr i v =
  set arr i (x v);
  set arr (i+1) (y v);
  set arr (i+2) (z v)

let set_triangle arr i =
  function
  | {a;b;c} ->
    set_vertex arr i a;
    set_vertex arr (i+3) b;
    set_vertex arr (i+6) c

let fill_bigarray (ts: triangle array) arr off =
  let triangle_floats = 9 in
  let triangle_count = Array.length ts in
  for i = 0 to triangle_count-1 do
    set_triangle arr (off + i*triangle_floats) ts.(i)
  done

let to_bigarray (ts: triangle array) =
  let triangle_floats = 9 in
  let arr = create Bigarray.float32 Bigarray.c_layout @@ Array.length ts * triangle_floats in
  print_string @@ string_of_int @@ Bigarray.Array1.dim arr ;
  fill_bigarray ts arr 0;
  arr


let radians_of_degrees d = d *. Float.pi /. 180.0
let degrees_of_radians r = r /. Float.pi *. 180.0
