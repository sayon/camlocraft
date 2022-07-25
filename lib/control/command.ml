open Kernel.Time

type camera_move_kind = | Forward | Backward | Right | Left | TurnRight | TurnLeft | TurnUp | TurnDown
type camera_command = { dir: camera_move_kind ; elapsed: time }
type graphics_command = | Wireframe | Polygons
type app_command = Close
type command =
  | ApplicationCommand of app_command
  | GraphicMode of graphics_command
  | CameraMove of camera_command
