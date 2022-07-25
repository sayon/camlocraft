open Logger

(** Facilities related to the game and absolute time. *)
type time = float
let epsilon = Float.epsilon

(** Global time state; used as a part of a global state. *)
type state = {
  prev_frame_start: time;
  prev_frame_end: time;
  cur_frame_start: time option;
}

(** Use this function to get current time. *)
let current_time () = GLFW.getTime ()

(** Starting value for the time state*)
let init_state () =
  let t = current_time () and e = epsilon in {
    prev_frame_start = t -. e -. e;
    prev_frame_end = t -. e;
    cur_frame_start = Some t
  }

let since_last_frame {prev_frame_end; cur_frame_start; _ } =
  match cur_frame_start with
  | Some s -> s -. prev_frame_end
  | None -> 0.0

let fps s =
  let elapsed = since_last_frame s in
  1.0 /. elapsed

module Handlers = struct
  let update_on_frame_start {prev_frame_start; prev_frame_end; _ }
    = {
      prev_frame_start;
      prev_frame_end;
      cur_frame_start = Some ( current_time () )
    }
  let update_on_frame_end { cur_frame_start; _ } : state
    = {
      prev_frame_start = Option.get cur_frame_start;
      prev_frame_end = current_time ();
      cur_frame_start = None
    }

  let fps_hard_cap = 60

  let idle s =
    let elapsed = current_time () -. s.prev_frame_end
    and quota = 1.0 /. (Float.of_int fps_hard_cap) in
    let wait = quota -. elapsed in
    Unix.sleepf wait ;
    s

end



let show ({prev_frame_start; prev_frame_end; cur_frame_start} as s) =
  Printf.sprintf "prev: %f to %f | cur: %f to now | elapsed %f FPS %f"
    prev_frame_start prev_frame_end
    begin match cur_frame_start with | Some f -> f |None -> 0.0 end
    (since_last_frame s)
    (fps s)

let log_time_state s = log GeneralLog (show s); s
