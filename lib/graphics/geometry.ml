open Ccmath.Vector.Vector3F
open Opengl_ext
open Kernel.Logger

type vec3 = Ccmath.Vector.Vector3F.vec3
type vertex = vec3
type matrix4 = Ccmath.Matrix.Matrix4F.matrix4
type vector4 = Ccmath__Vector.Vector4F.vec4
type 'a triangle = { a: 'a ; b: 'a ; c: 'a }

let show_vector3 = Ccmath.Vector.Vector3F.to_string

type texture_coord = {s:float; t:float}
let show_texture_coord = function | {s;t} -> Printf.sprintf "{s:%f;t:%f}" s t

type aug_vertex = {v:vertex; n:vec3; tx: texture_coord }

let show_aug_vertex (v:aug_vertex) : string =
  match v with
  | { v; n; tx } ->
    Printf.sprintf "%s; normal: %s; tx: %s" (show_vector3 v) (show_vector3 n) (show_texture_coord tx)

type coord_3D = float
let coord_3D_elt = Bigarray.float32
let triangle_elt_size = Bigarray.kind_size_in_bytes coord_3D_elt

(* TODO refactor *)
let components_in_vertex_descr = function
  | Vertices -> 3
  | VerticesAndNormals -> 3 * 2
  | VerticesAndNormalsAndTexture -> 3 * 2 + 2

let vertex_in_bytes layout = triangle_elt_size * (components_in_vertex_descr layout)

let triangle_components l = 3 * components_in_vertex_descr l

let stride_of_component layout = triangle_elt_size * (triangle_components layout)

let mk_vi x y z = mk_vec3 (float_of_int x) (float_of_int y) (float_of_int z)
(* Make a rectangle.
   1--2
   |  |
   3--4
*)

let mk_rect a1 a2 a3 a4 = [ {a = a1; b = a2; c = a4} ; {a = a1; b = a4; c = a3} ]
let mk_triangle a1 a2 a3 =  {a = a1; b = a2; c = a3}

let triangle_normal_explicit a b c =
  let module V3 = Ccmath.Vector.Vector3F in
  let (-) = V3.sub and cross = V3.cross and normalize = V3.normalize in
  normalize @@ cross (b - a) (c - a)

let triangle_normal t =
  let module V3 = Ccmath.Vector.Vector3F in
  let (-) = V3.sub and cross = V3.cross and normalize = V3.normalize in
  normalize @@ cross (t.b - t.a) (t.c - t.a)

let mk_rect_aug a1 a2 a3 a4 =
  let n = triangle_normal_explicit a1 a2 a4 in
  let augment vtx s t = {v = vtx; n; tx={s;t} } in
  let v1 = augment a1 0.0 1.0
  and v2 = augment a2 1.0 1.0
  and v3 = augment a3 0.0 0.0
  and v4 = augment a4 1.0 0.0
  in
  [ {a = v1; b = v2; c = v4} ; {a = v1; b = v4; c = v3} ]




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
    (mk_vi 1 0 0)
    (mk_vi 1 1 0)
    (mk_vi 0 0 0)
    (mk_vi 0 1 0)
    (mk_vi 1 0 1)
    (mk_vi 1 1 1)
    (mk_vi 0 0 1)
    (mk_vi 0 1 1)

open Bigarray.Array1
let set_vector3 arr i v =
  set arr i (x v);
  set arr (i+1) (y v);
  set arr (i+2) (z v)

let set_vertex_aug arr start_ofs v: unit =
  let put_to_bigarray  =
    List.mapi (fun idx -> Bigarray.Array1.set arr (start_ofs + idx))
  in
  match v with
  | {v ; n; tx={s;t}} ->
    ignore @@ put_to_bigarray [
        x v; y v; z v;
        x n; y n; z n;
        s; t
    ]

let set_triangle_aug arr start_ofs t : unit =
  let stride = vertex_in_bytes VerticesAndNormalsAndTexture in
  match t with | {a;b;c} ->
    set_vertex_aug arr (start_ofs + 0*stride) a;
    set_vertex_aug arr (start_ofs + 1*stride) b;
    set_vertex_aug arr (start_ofs + 2*stride) c

let triangles_to_bigarray

(* let set_triangle layout arr start_ofs t : unit = *)
(*   match t with | {a;b;c} -> *)
(*     let stride = components_in_vertex_descr Vertices in *)
(*     let process  = List.mapi (fun idx elem -> set_vector3 arr (start_ofs + idx * stride) elem) in *)
(*     ignore @@ process @@ *)
(*     match layout with *)
(*     | Vertices -> *)
(*       [a;b;c] *)
(*     | VerticesAndNormals -> let normal = triangle_normal t in *)
(*       [a; normal; b; normal; c; normal] *)
(*     | VerticesAndNormalsAndTexture -> failwith "Textures are not implemented" *)


(* let fill_bigarray layout (ts: vertex triangle array) arr off = *)
(*   let triangle_floats = triangle_components layout in *)
(*   let triangle_count = Array.length ts in *)
(*   for i = 0 to triangle_count-1 do *)
(*     set_triangle layout arr (off + i*triangle_floats) ts.(i) *)
(*   done *)

(* let raw_floats_to_string layout ba = *)
(*   let module BA = Bigarray.Array1 in *)

(*   let s = ref "\n" in *)
(*   let append ss = s := !s ^ ss in *)
(*   let append_vertex label a b c = append @@ Printf.sprintf "%s : (%f,%f,%f)\n" label a b c in *)
(*   let append_vertex2 label a b = append @@ Printf.sprintf "%s : (%f,%f)\n" label a b in *)

(*   let components = triangle_components layout in *)
(*   for i  = 0 to (BA.dim ba / components) -1 do *)
(*     let component j = BA.unsafe_get ba (components*i + j) in *)
(*     match layout with *)
(*     | Vertices -> *)
(*       append_vertex "Vertex" (component 0) (component 1) (component 2) *)
(*     | VerticesAndNormals -> *)
(*       append_vertex "Vertex" (component 0) (component 1) (component 2); *)
(*       append_vertex "Normal" (component 3) (component 4) (component 5) *)
(*     | VerticesAndNormalsAndTexture -> *)
(*       append_vertex "Vertex" (component 0) (component 1) (component 2); *)
(*       append_vertex "Normal" (component 3) (component 4) (component 5); *)
(*       append_vertex2 "Texture" (component 6) (component 7) *)
(*   done; *)
(*   !s *)

(* let triangles_to_bigarray (layout:vertex_array_format) (ts: vertex triangle array) = *)
(*   let triangle_floats = triangle_components layout in *)
(*   let arr = create Bigarray.float32 Bigarray.c_layout @@ Array.length ts * triangle_floats in *)
(*   fill_bigarray layout ts arr 0; *)
(*   log GeneralLog (raw_floats_to_string layout arr); *)
(*   arr *)

module Conversions = struct
let radians_of_degrees d = d *. Float.pi /. 180.0
let degrees_of_radians r = r /. Float.pi *. 180.0
end

module VectorCollection = struct
  let up3 = mk_vec3 0.0 1.0 0.0
end
