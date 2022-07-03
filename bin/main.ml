open struct
  module Gl = Tgl4.Gl
end

open Graphics.Renderer

let processInput ~window : unit=
  if GLFW.getKey ~window:window ~key:GLFW.Escape ||
     GLFW.getKey ~window:window ~key:GLFW.Q
  then
    GLFW.setWindowShouldClose ~window:window ~b:true

let init_graphic_context ~window  =
  Gl.clear_color 0.2 0.3 0.3 1.0;
  { window = window; renderer = load_meshes () }

let init () =
  GLFW.init ();
  GLFW.windowHint ~hint:GLFW.ContextVersionMajor ~value:3;
  GLFW.windowHint ~hint:GLFW.ContextVersionMinor ~value:2;
  GLFW.windowHint ~hint:GLFW.OpenGLProfile ~value:GLFW.CoreProfile;
  GLFW.windowHint ~hint:GLFW.OpenGLForwardCompat ~value:true;
  let window = GLFW.createWindow
      ~width:800
      ~height:600
      ~title:Config.windowTitle
      ?monitor:None ?share:None () in
  GLFW.makeContextCurrent ~window: (Some window);
  ignore @@ GLFW.setFramebufferSizeCallback ~window:window ~f:(Some (fun _ w h -> Gl.viewport 0 0 w h));
  window

let deinit () =  GLFW.terminate ()

let mainLoop (ctx:graphics_ctx) =
  let shouldClose () = GLFW.windowShouldClose ~window:ctx.window in

  while (not @@ shouldClose ()) do

    Gl.clear(Gl.color_buffer_bit lor Gl.depth_buffer_bit );

    processInput ~window:ctx.window;
    render ctx.renderer;

    GLFW.swapBuffers ~window:ctx.window;
    GLFW.pollEvents ();
    Unix.sleepf ( 1.0 /. float_of_int Config.fps )
  done

let _ =
  let window = init () in
  let graphicContext =  init_graphic_context ~window:window in
  mainLoop graphicContext;
  deinit ()

