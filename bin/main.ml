open struct
  module Gl = Tgl4.Gl
end

open Setup
open Graphics.Renderer

let process_input ~window : unit=
  let key_pressed k = GLFW.getKey ~window:window ~key:k in
  if key_pressed GLFW.Q
  then GLFW.setWindowShouldClose ~window:window ~b:true
  else Graphics.Config.KeyMap.iter
      (fun k a -> if key_pressed k then (a ()))
      Graphics.Config.keys


 let init_graphic_context ~window  =
  Gl.clear_color 0.2 0.3 0.3 1.0;
  Gl.enable Gl.depth_test;
  Gl.depth_func Gl.less;
  { window = window; renderer = load_meshes () }


let main_loop (ctx:graphics_ctx) =
  let should_close () = GLFW.windowShouldClose ~window:ctx.window in
  while (not @@ should_close ()) do

    Gl.clear(Gl.color_buffer_bit lor Gl.depth_buffer_bit );

    process_input ~window:ctx.window;
    render ctx.renderer;

    GLFW.swapBuffers ~window:ctx.window;
    GLFW.pollEvents ();
    Unix.sleepf ( 1.0 /. float_of_int Graphics.Config.fps )
  done

let _ =
  let window = init () in
  let graphic_context =  init_graphic_context ~window:window in
  main_loop graphic_context;
  ignore @@ window;
  ignore @@ graphic_context;
  deinit ()

