open struct
  module Gl=Tgl4.Gl
end

open Kernel.Logger
open Kernel.Io
open Kernel.Io.Bigarray_ext

(** Shader representation and utility functions. *)

(** ID of a shader, used by OpenGL *)
type id = { value : int }

(** OpenGl gives access to either {!VertexShader} or {!FragmentShader} *)
type kind = VertexShader | FragmentShader

let kind_to_gl = function
  | VertexShader -> Gl.vertex_shader
  | FragmentShader -> Gl.fragment_shader

type shader = {
  id: id;
  kind: kind;
  name: string;
  source: string
}

type creation_result =
    LoadingError of string
  | CompilationError of string
  | Compiled of shader

open struct
  let compilation_failed shader = 0 = get_through_buffer @@ Gl.get_shaderiv shader Gl.compile_status
  let error_string shader = get_error_string shader Gl.get_shaderiv Gl.get_shader_info_log

  let diagnose name status =
    let prefix = Printf.sprintf "Shader \"%s\": "  name in
    log ShadersLog @@
    prefix ^
    match status with
    | LoadingError e -> Printf.sprintf "Error loading shader \"%s\": %s\"" name e
    | Compiled s ->  Printf.sprintf "Shader compiled successfully with id %d" s.id.value
    | CompilationError e -> Printf.sprintf "Compilation error: \n%s"  e

  let force_or_except = function
    | LoadingError e -> failwith e
    | CompilationError e -> failwith e
    | Compiled s -> s
end

let from_source_try ~name ~source ~kind =
  let shader  = Gl.create_shader @@ kind_to_gl kind in
  Gl.shader_source shader source;
  log ShadersLog @@ Printf.sprintf
    "Loading shader under the name \"%s\": \n\"\"\"\n%s\n\"\"\"\n" name source;
  Gl.compile_shader shader;
  if compilation_failed shader then
    let result = CompilationError (error_string shader) in
    Gl.delete_shader shader; result
  else
    Compiled { id = {value=shader}; kind; name; source }


let from_file_try ~name ~filename ~kind =
  let s = match File.read filename with
    | Right error -> LoadingError error
    | Left source -> from_source_try
                       ~name:name
                       ~source:source
                      ~kind:kind
  in
  diagnose name s;
  s

let from_file ~name ~filename ~kind = force_or_except @@
  from_file_try ~name:name ~filename:filename ~kind:kind

module Collection = struct

  let sample_vertex_shader () =
    from_file
      ~name: "Simple vertex shader"
      ~filename:"lib/simple_vertex_shader.glsl"
      ~kind: VertexShader

  let sample_fragment_shader () =
    from_file
      ~name: "Simple fragment shader"
      ~filename:"lib/simple_fragment_shader.glsl"
      ~kind: FragmentShader
end
