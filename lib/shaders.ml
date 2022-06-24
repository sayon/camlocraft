open struct
  module Gl = Tgl4.Gl
end

open Bigarray_ext

let log = print_string

let creationResult ~shader =
  let buffer = createIntBuffer ~size:1 in
  Gl.get_shaderiv shader Gl.compile_status buffer;
  Bigarray.Array1.unsafe_get buffer 0

let getErrorString obj getIV getInfoLog =
  let bufferInfoLogLength = createIntBuffer ~size:1 in
  getIV obj Gl.info_log_length bufferInfoLogLength;
  let length = Int32.to_int @@ Bigarray.Array1.unsafe_get bufferInfoLogLength 0 in
  let bufferErrorMessage = createByteBuffer ~size:length in
  getInfoLog obj length None bufferErrorMessage;
  Bigstring.to_string bufferErrorMessage

let diagnose ~shader =
  let status = creationResult ~shader:shader in
  log @@ if status <> Int32.zero then
    getErrorString shader Gl.get_shaderiv Gl.get_shader_info_log
  else
    "shader compiled successfully";
  log "\n" ;
  flush_all ()

let create ~source ~shaderType =
  let shader  = Gl.create_shader shaderType in
  Gl.shader_source shader  source;
  Gl.compile_shader shader ;
  diagnose ~shader:shader;
  shader

(* This is a quick project so I don't mind that much to mix levels of abstraction *)
let createVertex ~source = create ~source:source ~shaderType:Gl.vertex_shader

let createFragment ~source = create ~source:source ~shaderType:Gl.fragment_shader


let read_file filename =
  let ch = open_in filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

let createSampleVertex () =
  let simpleVertexShader = read_file "lib/simple_vertex_shader.glsl" in
  createVertex ~source:simpleVertexShader

let createSampleFragment () =
let simpleFragmentShader = read_file "lib/simple_fragment_shader.glsl" in
createFragment ~source:simpleFragmentShader


let programCreationResult ~program =
  let buffer = createIntBuffer ~size:1 in
  Gl.get_programiv program Gl.link_status buffer;
  Bigarray.Array1.unsafe_get buffer 0

let diagnoseProgram ~program =
  let status = programCreationResult ~program:program in
  print_string @@ if status <> Int32.zero then
    getErrorString program Gl.get_programiv Gl.get_program_info_log
  else "program compiled and linked successfully";
  print_string "\n" ;
  flush_all ()

let createProgram ~vertexShader ~fragmentShader =
  let program = Gl.create_program () in
  Gl.attach_shader program vertexShader;
  Gl.attach_shader program fragmentShader;
  Gl.link_program program ;
  diagnoseProgram ~program:program;
  program


let createSampleProgram () =
  let vs = createSampleVertex ()
  and fs = createSampleFragment () in
  let program = createProgram
      ~vertexShader: vs
      ~fragmentShader: fs in
  Gl.detach_shader program vs;
  Gl.detach_shader program fs;
  Gl.delete_shader vs;
  Gl.delete_shader fs;
  program
