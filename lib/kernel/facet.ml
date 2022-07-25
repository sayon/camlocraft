(** Facets *)

(** Facets are allowing an abstraction over a part of the state. In Camlocraft,
    the global state consists of three major parts:

    1) Global engine state, used by the world and logic 2) View state,
    everything needed to render the world 3) Control state, interfacing with the
    player

    Facets allow to get these three aspects from the state, and also split them
    into more granular parts.

*)

(** A facet of a bigger state {!'a} projected to a smaller state {!'b} *)
type ('a, 'b) facet = { get: 'a -> 'b; set: 'a -> 'b -> unit }

(** Lift a function [f] over a state facet {!'b} to a function over bigger state {!'a}. *)
let lift
    (gs:'a)
    (facet: ('a,'b) facet)
    (f:'b -> 'b) : unit =
  facet.set gs
    (f
       (facet.get gs))
