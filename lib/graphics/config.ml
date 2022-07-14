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

let wireframe_on () =
  if !(current.rendering_mode) <> Wireframe then
    begin
      log GeneralLog "Switching to wireframe mode";
      Gl.polygon_mode Gl.front_and_back Gl.line;
      current.rendering_mode := Wireframe
    end
  else ()

let polygons_on () =
  if !(current.rendering_mode) <> Polygons then
    begin
      log GeneralLog "Switching to polygons mode";
      Gl.polygon_mode Gl.front_and_back Gl.fill;
      (current.rendering_mode) := Polygons
    end
  else ()

module KeyMap = Map.Make(struct type t = GLFW.key let compare = Stdlib.compare end)


let keys = KeyMap.(
    empty |>
    add GLFW.F wireframe_on |>
    add GLFW.S polygons_on
  )

