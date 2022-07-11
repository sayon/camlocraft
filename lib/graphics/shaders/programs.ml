open struct
  module Gl=Tgl4.Gl
end

open Kernel.Logger
open Kernel.Io.Bigarray_ext
open Shader

type id = { value: int }

type uniform_variable = { var_name: string; uniform_id: int }

(** A program is a set of compiled shaders linked together. The shaders are
    connected through in/out variables and share uniform variables with CPU. *)
type program = {
  id: id;
  name: string;
  shaders: shader list;
  uniforms: ( string * uniform_variable ) list
}

module Uniform = struct
  let get_id pid name = Gl.get_uniform_location pid name

  let init (pid:id) name =
    ( name, {var_name = name; uniform_id = get_id pid.value name })

  let get_by_name p s = List.assoc s p.uniforms

  (* TODO a typetag for the variable could be useful *)
  let set_int p uname value = match get_by_name p uname with
    | {uniform_id; _} -> Gl.uniform1i uniform_id value

end

type creation_result =
    LinkingError of string
  | Linked of program


open struct

  let error_string program =
    get_error_string program Gl.get_programiv Gl.get_program_info_log
  let link_status p = get_through_buffer (Gl.get_programiv p Gl.link_status)
  let program_not_linked p = link_status p = 0

  let diagnose name status =
    let prefix = Printf.sprintf "Program \"%s\": "  name in
    log ShadersLog @@
    prefix ^
    match status with
    | LinkingError e -> Printf.sprintf "Error linking program \"%s\": %s\"" name e
    | Linked _ -> "Linking successful"

end

let create_program
    ~name
    ~(vertex_shader:shader)
    ~(fragment_shader:shader)
    ~(uniforms: string list): creation_result =
  let program = Gl.create_program () in
  match vertex_shader, fragment_shader with
  | { id=vs; kind=VertexShader; _}, { id=fs; kind=FragmentShader; _} ->
    Gl.attach_shader program vs.value;
    Gl.attach_shader program fs.value;
    Gl.link_program program;
    if program_not_linked program
    then
      LinkingError (error_string program)
    else
      Linked { id = {value=program}; name; shaders=[vertex_shader; fragment_shader];
               uniforms = List.map (Uniform.init {value=program}) uniforms
             }
  | _,_ ->
    raise @@ Invalid_argument "Shader types (vertex, fragment) do not match"

open struct
let cleanup_shader (program:program) (shader:shader) : unit =
  Gl.detach_shader program.id.value shader.id.value;
  Gl.delete_shader shader.id.value
end

let cleanup_all_shaders (program:program) : unit =
  Kernel.Util.do_all (cleanup_shader program) program.shaders

let load_program ~name ~vertex_shader_path ~fragment_shader_path ~uniforms =
let vs = Shader.from_file ~name:vertex_shader_path ~filename:vertex_shader_path ~kind:VertexShader
and fs = Shader.from_file ~name:fragment_shader_path ~filename:fragment_shader_path ~kind:FragmentShader
in
log ShadersLog @@ Printf.sprintf "Loading program \"%s\"" name;
let res = create_program
    ~name:name
    ~vertex_shader: vs
    ~fragment_shader: fs
    ~uniforms: uniforms
in
diagnose name res;
match res with
| Linked program ->
  cleanup_all_shaders program; program
| LinkingError e -> failwith e


module Collection = struct
  let sample_program () =
    load_program ~name:"Sample program"
      ~vertex_shader_path:"lib/simple_vertex_shader.glsl"
      ~fragment_shader_path:"lib/simple_fragment_shader.glsl"
      ~uniforms:[ "u_MVP"; "u_MV";  "u_texture"]

end
