open Io.Bigarray_ext
open Tgl4

type log_kind = ShadersLog | GeneralLog | TexturesLog

let log_kind_repr kind = "Log: " ^
                         (match kind with
                          | ShadersLog -> "Shader"
                          | GeneralLog -> "General"
                          | TexturesLog -> "Textures")
                         ^ ": \t"

type logger = {
  raw_printer: string -> unit ;
  flush: unit -> unit
}

let default_logger = { raw_printer = print_string ; flush = flush_all }
let current_logger = ref default_logger

let log kind msg =
  if msg <> "" then
  !current_logger.raw_printer (log_kind_repr kind);
  !current_logger.raw_printer msg;
  !current_logger.raw_printer "\n";
  !current_logger.flush ()

let log_if kind cond error_expr good_expr =
  log kind @@ if cond ()
  then error_expr ()
  else good_expr ()


let get_error_string obj getIV getInfoLog =
  let bufferInfoLogLength = create_int_buffer ~size:1 in
  getIV obj Gl.info_log_length bufferInfoLogLength;
  let length = Int32.to_int @@ Bigarray.Array1.unsafe_get bufferInfoLogLength 0 in
  let bufferErrorMessage = create_byte_buffer ~size:length in
  getInfoLog obj length None bufferErrorMessage;
  Bigstring.to_string bufferErrorMessage

