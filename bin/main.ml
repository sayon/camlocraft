open struct
  module Gl = Tgl4.Gl
end

open Setup
open Graphics.Renderer
open Kernel.Time
open Kernel.State
open Kernel.Events

open Kernel.Logger

open Control.Command

(* TODO: should accept global states *)
let rec process_input (gs:Control.Universe.state) = function
  | [] -> ()
  | (cmd:command) :: cmds ->
    let graphics = (Control.Universe.graphics_facet.get gs) in
    let camera = graphics.camera in
    (match cmd with
     | ApplicationCommand ac ->
       begin
         match ac with | Close ->
           Control.Universe.init_quit gs
       end
     | GraphicMode gc ->
       begin
         match gc with
         | Wireframe -> Graphics.Config.wireframe_mode true
         | Polygons -> Graphics.Config.wireframe_mode false
       end
     | CameraMove {dir; elapsed} ->
       begin
         log GeneralLog @@ Printf.sprintf "Moving elapsed: %f" elapsed;
         let factor = 0.001 in
         let dist = (factor *. elapsed)  in

         match dir with
         | Forward ->
           Graphics.Camera.move_fwd camera dist
         | Backward ->
           Graphics.Camera.move_bwd camera dist
         | Right ->
           Graphics.Camera.move_right camera dist
         | Left ->
           Graphics.Camera.move_left camera dist
         | TurnRight -> ()
         | TurnLeft -> ()
         | TurnUp -> ()
         | TurnDown -> ()
       end
    );
    process_input gs cmds



let on_frame_start = handlers_for
    [
      (time_facet, Handlers.update_on_frame_start);
    ]
and on_frame_end = handlers_for
    [
      (time_facet, log_time_state);
      (time_facet, Handlers.update_on_frame_end);
      (time_facet, Handlers.idle);
    ]


let main_loop (gs:Control.Universe.state)=

  let window = (Control.Universe.graphics_facet.get gs).window in
  while (not @@ Control.Universe.should_close gs) do
    begin
      execute_handlers gs.world_state on_frame_start;
      process_input gs (Control.Input.gather_input gs);
      render_scene gs.graphics_state;

      GLFW.swapBuffers ~window:window;
      GLFW.pollEvents ();

      execute_handlers gs.world_state on_frame_end
      (* Unix.sleepf ( 1.0 /. float_of_int Graphics.Config.fps); *)
    end
  done

let _ =
  let window = Setup.init () in

  let gs : Control.Universe.state = {
    control_state = Control.State.init ();
    world_state = Kernel.State.init ();
    graphics_state = Graphics.State.init ~window:window
  } in
  main_loop gs;
  deinit ()
