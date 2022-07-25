open struct
  module Gl = Tgl4.Gl
end

open Kernel.Logger

(* TODO refactor to explicitly produce a 'global state' including wireframe status *)
let fps : int = 60

type rendering_mode = Polygons | Wireframe

let wireframe_mode = function
  | true  ->
      log GeneralLog "Switching to wireframe mode";
      Gl.polygon_mode Gl.front_and_back Gl.line;
  | false ->
      log GeneralLog "Switching to polygons mode";
      Gl.polygon_mode Gl.front_and_back Gl.fill;

