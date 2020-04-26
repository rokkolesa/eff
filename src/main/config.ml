(** Configuration parameters *)

let version = "5.0"

type pervasives = PervasivesNone | PervasivesDefault

let pervasives_file = ref PervasivesDefault

type backend = Runtime | Multicore of string | Js of string

let backend = ref Runtime

let ascii = ref false

let interactive_shell = ref true

let wrapper = ref (Some ["rlwrap"; "ledit"])

let verbosity = ref 3

let output_formatter = ref Format.std_formatter

let error_formatter = ref Format.err_formatter
