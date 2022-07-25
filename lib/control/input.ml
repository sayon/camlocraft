open Kernel.Time
open Command
open Universe

module KeyMap = Map.Make(struct type t = GLFW.key let compare = Stdlib.compare end)

let keys =
  let move gs dir =
    let elapsed_est gs = since_last_frame
      @@ Kernel.State.time_facet.get
      @@ world_facet.get gs in
    let elapsed gs = Float.max (elapsed_est gs) Float.epsilon in
    CameraMove ({dir; elapsed = elapsed gs }) in
  let open GLFW in
  KeyMap.(
    empty |>
    add Q (fun _  -> ApplicationCommand Close) |>
    add F (fun _  -> GraphicMode Wireframe ) |>
    add P (fun _  -> GraphicMode Polygons ) |>
    add W (fun gs -> move gs Forward) |>
    add S (fun gs -> move gs Backward) |>
    add A (fun gs -> move gs Left) |>
    add D (fun gs -> move gs Right)
  )

let gather_input (gs:Universe.state): command list =
  let window = (graphics_facet.get gs).window in
  let key_pressed k = GLFW.getKey ~window:window ~key:k in
  KeyMap.fold
    (fun k a ls -> if key_pressed k then (a gs)::ls else ls)
    keys []
