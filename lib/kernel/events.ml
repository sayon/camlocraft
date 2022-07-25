open Facet

type 'a handler =
  | Handler :  { get: 'a -> 's; update: 's->'s ; ret : 'a -> 's -> unit } -> 'a handler

let handler_execute (gs:'a) (h:'a handler) = match h with
  | Handler {get; update; ret} ->
    ret gs (update (get gs))

let handler_of_facet (facet:('a, 's) facet) (f:'s->'s) : 'a handler =
  Handler {get = facet.get; ret = facet.set; update = f }

type 'a handlers = { mutable handlers: 'a handler list }

let execute_handlers gs (handlers: 'a handlers) =
  List.iter (handler_execute gs) handlers.handlers

let handlers_for hs = { handlers = List.map (Util.uncurry handler_of_facet) hs}
