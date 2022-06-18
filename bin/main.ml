open struct
  module Gl = Tgl4.Gl
end

open Camlocraft.Shaders
open Camlocraft.Graphics

let fps : int = 30
let windowTitle = "Camlocraft"

let processInput ~window : unit=
  if GLFW.getKey ~window:window ~key:GLFW.Escape ||
     GLFW.getKey ~window:window ~key:GLFW.Q
  then
    GLFW.setWindowShouldClose ~window:window ~b:true

let init_graphic_context ~window  =
  let vao,_ = genVertexArray
      ~vertices:
        [|
          -0.5; -0.5; 0.0;
          0.5; -0.5; 0.0;
          0.0;  0.5; 0.0
        |] in
  let program = createSampleProgram () in
  Gl.use_program program;
  Gl.bind_vertex_array vao;
  Gl.clear_color 0.2 0.3 0.3 1.0;
  { window = window; program = program; vao = vao }

let render (c:graphics_ctx) =
  Gl.bind_vertex_array c.vao;
  Gl.draw_arrays Gl.triangles 0 3;
  Gl.flush ()

let init () =
  GLFW.init ();
  GLFW.windowHint ~hint:GLFW.ContextVersionMajor ~value:3;
  GLFW.windowHint ~hint:GLFW.ContextVersionMinor ~value:2;
  GLFW.windowHint ~hint:GLFW.OpenGLProfile ~value:GLFW.CoreProfile;
  GLFW.windowHint ~hint:GLFW.OpenGLForwardCompat ~value:true;
  let window = GLFW.createWindow
      ~width:800
      ~height:600
      ~title:windowTitle
      ?monitor:None ?share:None () in
  GLFW.makeContextCurrent ~window: (Some window);
  ignore @@ GLFW.setFramebufferSizeCallback ~window:window ~f:(Some (fun _ w h -> Gl.viewport 0 0 w h));
  window

let deinit () =  GLFW.terminate ()

let mainLoop (graphicContext:graphics_ctx) =
  let shouldClose () = GLFW.windowShouldClose ~window:graphicContext.window in

  while (not @@ shouldClose ()) do

    Gl.clear(Gl.color_buffer_bit lor Gl.depth_buffer_bit ); 

    processInput ~window:graphicContext.window;
    render graphicContext;

    GLFW.swapBuffers ~window:graphicContext.window;
    GLFW.pollEvents ();
    Unix.sleepf ( 1.0 /. float_of_int fps )
  done

let _ =
  let window = init () in
  let graphicContext =  init_graphic_context ~window:window in
  mainLoop graphicContext;
  deinit ()

