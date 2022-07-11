open struct
  module Gl = Tgl4.Gl
end

type vertex_array_object = int
type vertex_buffer_object = int

type graphics_ctx = {
  window: GLFW.window ;
  (* FIXME needs refactoring *)
  program: int ;
}
