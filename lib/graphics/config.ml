open struct
  module Gl = Tgl4.Gl
end

open Kernel.Logger

let fps : int = 30

type rendering_mode = Polygons | Wireframe

type config = {
  rendering_mode: rendering_mode ref
}

let default = {
  rendering_mode = ref Polygons
}

let current = default

let toggle_wireframe () =
  match !(current.rendering_mode) with
  | Wireframe -> ()
    (* log GeneralLog "Switching to polygons mode"; *)
    (* Gl.polygon_mode Gl.front_and_back Gl.fill; *)
    (* (current.rendering_mode) := Polygons *)
  | Polygons ->
    log GeneralLog "Switching to wireframe mode";
    Gl.polygon_mode Gl.front_and_back Gl.line;
    (current.rendering_mode) := Wireframe

module KeyMap = Map.Make(struct type t = GLFW.key let compare = Stdlib.compare end)


let keys = KeyMap.(
    empty |>
    add GLFW.F toggle_wireframe
  )

