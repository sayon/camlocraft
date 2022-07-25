open Facet

(** Global world state, depends on facets *)
type state = {
  mutable time: Time.state;
}

let time_facet : _ facet = {
  get = (fun gs -> gs.time) ;
  set = (fun gs v -> gs.time <- v)
}

let init () = {
  time = Time.init_state ()
}
