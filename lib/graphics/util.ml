open Bigarray_ext

(** Utility function to use generators returning their result by a
    pointer. *)
let getThroughTempBuffer generator =
  let bufferForID = createIntBuffer ~size:1 in
  generator bufferForID;
  get ~buffers:bufferForID ~id:0
