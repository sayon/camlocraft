open Kernel.Facet

type state = {
  mutable control_state:State.state;
  mutable world_state:Kernel.State.state;
  mutable graphics_state:Graphics.State.state
}

let control_facet : _ facet = {
  get = (fun gs -> gs.control_state) ;
  set = (fun gs v -> gs.control_state <- v)
}

let world_facet : _ facet = {
  get = (fun gs -> gs.world_state) ;
  set = (fun gs v -> gs.world_state <- v)
}

let graphics_facet : _ facet = {
  get = (fun gs -> gs.graphics_state) ;
  set = (fun gs v -> gs.graphics_state <- v)
}

let should_close gs = GLFW.windowShouldClose ~window:gs.graphics_state.window
let init_quit gs    = GLFW.setWindowShouldClose ~window:gs.graphics_state.window ~b:true
