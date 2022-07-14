open Ccmath.Vector.Vector3F
open Kernel.Io.RawBuffer

type vec3 = Ccmath.Vector.Vector3F.vec3
type matrix4 = Ccmath.Matrix.Matrix4F.matrix4
type vec4 = Ccmath.Vector.Vector4F.vec4

module VertexDescription = struct

  type texture_coord = { s: float; t: float }
  type vertex_descr = { v: vec3; n: vec3 option ; tx: texture_coord option }

  let size_in_floats = function
    | {n;tx;_} -> 3
                  + (if Option.is_some n then 3 else 0)
                  + (if Option.is_some tx then 2 else 0)

  let iteri_floats f {v;n;tx} =
    let i = ref 0 in
    let inc () = i := !i + 1 in
    f !i (x v); inc ();
    f !i (y v); inc ();
    f !i (z v); inc ();
    (match n with
     | Some n ->
       f !i (x n); inc ();
       f !i (y n); inc ();
       f !i (z n); inc ()
     | None -> ()
    );
    (match tx with
     | Some ({s;t}) ->
       f !i s; inc ();
       f !i t
     | None -> ()
    )

  let show_texture_coord = function | {s;t} -> Printf.sprintf "{s:%.2f;t:%.2f}" s t

  let show_aug_vertex (v:vertex_descr) : string =
    let show_vector3 = Ccmath.Vector.Vector3F.to_string in
    let show_normal n = Printf.sprintf " normal: %s" (show_vector3 n)
    and show_option f o : string = match o with | None -> "" | Some e -> f e in
    match v with
    | { v; n; tx } ->
      Printf.sprintf "%s %s %s"
        (show_vector3 v)
        (show_option show_normal n)
        (show_option show_texture_coord tx)
  (* let set_raw_float ba ofs f = *)
  (*   Kernel.Logger.log Kernel.Logger.GeneralLog (Printf.sprintf "setting float=%f with ofs=%d" f ofs); *)
  (*   Kernel.Io.RawBuffer.set_raw_float ba ofs f *)

  let to_bigarray (ba:raw_buffer) (start_ofs: int) (v:vertex_descr) =
    let open Kernel.Logger in
    log GeneralLog @@ show_aug_vertex v;
    iteri_floats (fun i f ->
        let ofs = (start_ofs + i * (Bigarray.kind_size_in_bytes Bigarray.float32)) in
        set_raw_float ba ofs f
      ) v

  let size_in_bytes v = (Bigarray.kind_size_in_bytes Bigarray.float32) * size_in_floats v

end

module Triangle = struct

  type 'a triangle = { a: 'a ; b: 'a ; c: 'a }

  let mk_triangle a b c =  { a ; b ; c }

  let size_in_floats (t : VertexDescription.vertex_descr triangle) =
    3 * (VertexDescription.size_in_floats (t.a) )

  let size_in_bytes t = (Bigarray.kind_size_in_bytes Bigarray.float32) * size_in_floats t

  let normal_aux a b c =
    let module V3 = Ccmath.Vector.Vector3F in
    let (-) = V3.sub and cross = V3.cross and normalize = V3.normalize in
    normalize @@ cross (b - a) (c - a)

  let normal t = normal_aux t.a t.b t.c

  let iter f { a; b; c } = f a; f b; f c
  let iteri f { a; b; c } = f 0 a; f 1 b; f 2 c

  (* let to_vertex_list { a; b; c } = [a;b;c] *)
  let to_bigarray start_ofs ba =
    let module V = VertexDescription in
    iteri
      (fun i_vtx v ->
         V.to_bigarray ba
           (start_ofs + (i_vtx * (V.size_in_bytes v))) v )
end


module Rectangle = struct

  let mk_rect a1 a2 a3 a4 =
    let open Triangle in
    let open VertexDescription in
    let n = normal_aux a1 a2 a4 in
    let augment v s t : vertex_descr = {v=v; n = Some n; tx= Some {s;t} } in
    let v1 = augment a1 0.0 1.0
    and v2 = augment a2 1.0 1.0
    and v3 = augment a3 0.0 0.0
    and v4 = augment a4 1.0 0.0
    in
    [ mk_triangle v1 v2 v4; mk_triangle v1 v4 v3 ]

end


module Cube = struct

  (** One face is defined by a rectangle
      [a1]-[a2]-[a3]-[a4], another one by
      [a5]-[a6]-[a7]-[a8].
  *)

  let mk_cube a1 a2 a3 a4 a5 a6 a7 a8 =
    let open Rectangle in
    List.concat @@
    [
      mk_rect a1 a2 a3 a4;
      mk_rect a5 a6 a7 a8;
      mk_rect a1 a2 a5 a6;
      mk_rect a3 a4 a7 a8;
      mk_rect a1 a5 a3 a7;
      mk_rect a2 a4 a6 a8
    ]

  open struct
    let mk_vi x y z = mk_vec3 (float_of_int x) (float_of_int y) (float_of_int z)
  end

  let cube_one = mk_cube
      (mk_vi 1 0 0)
      (mk_vi 1 1 0)
      (mk_vi 0 0 0)
      (mk_vi 0 1 0)
      (mk_vi 1 0 1)
      (mk_vi 1 1 1)
      (mk_vi 0 0 1)
      (mk_vi 0 1 1)
end


module Vertices = struct
  open VertexDescription
  open Triangle

  let needed_capacity (ls: vertex_descr triangle list) =
    match ls with
    | [] -> 0
    | (t::_) -> (Triangle.size_in_bytes t) * (List.length ls)

  let to_bigarray (buf:raw_buffer) (start_ofs: int) (ls: vertex_descr triangle list) : unit =
    match ls with
    | [] -> ()
    | (t::_) ->
      let triangle_stride = Triangle.size_in_bytes t in
      List.iteri (fun i -> Triangle.to_bigarray (start_ofs + i * triangle_stride) buf) ls

  let iteri_vertices f =
    function
    | [] -> ()
    | (t::_) as ls ->
      let triangle_in_bytes = Triangle.size_in_bytes t in
      List.iteri (fun i -> Triangle.iteri (fun j -> f (i * triangle_in_bytes + j))) ls

  let create_bigarray (ls: vertex_descr triangle list) : raw_buffer =
    let ba =  Kernel.Io.RawBuffer.create (needed_capacity ls) in
    to_bigarray ba 0 ls;
    ba

end

module VectorCollection = struct
  let up3 = mk_vec3 0.0 0.0 1.0
end

module Conversions = struct
  let radians_of_degrees d = d *. Float.pi /. 180.0
  let degrees_of_radians r = r /. Float.pi *. 180.0
end
