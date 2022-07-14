open struct
  module Gl = Tgl4.Gl
end


type graphics_ctx = {
  window: GLFW.window ;
  (* FIXME needs refactoring *)
  program: int ;
}
