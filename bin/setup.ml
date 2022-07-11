open struct
  module Gl = Tgl4.Gl
end

let window_title = "Camlocraft"
let init () =
  GLFW.init ();
  GLFW.windowHint ~hint:GLFW.ContextVersionMajor ~value:3;
  GLFW.windowHint ~hint:GLFW.ContextVersionMinor ~value:3;
  GLFW.windowHint ~hint:GLFW.OpenGLProfile ~value:GLFW.CoreProfile;
  GLFW.windowHint ~hint:GLFW.OpenGLForwardCompat ~value:true;
  let window = GLFW.createWindow
      ~width:800
      ~height:600
      ~title:window_title
      ?monitor:None ?share:None () in
  GLFW.makeContextCurrent ~window: (Some window);
  ignore @@ GLFW.setFramebufferSizeCallback ~window:window ~f:(Some (fun _ w h -> Gl.viewport 0 0 w h));
  window


let deinit () =  GLFW.terminate ()
