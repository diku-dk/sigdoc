signature PARSE_ARG = sig
  datatype t = Nullary of string * (unit -> unit)
             | Unary of string * (string -> unit)
             | Multi of string * (string list -> unit)
  val run : t list -> string list
end
