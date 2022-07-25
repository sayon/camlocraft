open Kernel.Facet
open Command

(** Global control state. Use {!Kernel.Facet} to interface with its parts. *)
type state = {
  mutable actions: command list
}

let init () = {
  actions = []
}

let actions_facet : _ facet = {
  get = (fun gs -> gs.actions) ;
  set = (fun gs v -> gs.actions<- v)
}

