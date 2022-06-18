type shader_program = int
type vertex_array_object = int
type vertex_buffer_object = int
type graphics_ctx = {
  window : GLFW.window;
  program : shader_program;
  vao : vertex_array_object;
}

val genVertexBuffer : vertices:float array -> int
val genVertexArray : vertices:float array -> int * int
