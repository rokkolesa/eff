open CoreUtils
module V = Value

type translation = Exists of string | Unknown

let comparison_functions =
  Assoc.of_list 
    [("===", Exists "x => y => x === y")
    ; ("<", Exists "x => y => x < y")]

let constants =
  Assoc.of_list
    [ ("infinity", Exists "Infinity")
    ; ("neg_infinity", Exists "-Infinity")
    ; ("nan", Exists "NaN") ]

let arithmetic_operations =
  Assoc.of_list
    [ ("~-", Exists "x => -x")
    ; ("+", Exists "x => y => x + y")
    ; ("-", Exists "x => y => x - y")
    ; ("*", Exists "x => y => x * y")
    ; ("/", Exists "x => y => x / y")
    ; ("%", Exists "x => y => x % y")
    ; ("**", Exists "x => y => x ** y")
    ; ("exp", Exists "Math.exp")
    ; ("expm1", Exists "Math.expm1")
    ; ("log", Exists "Math.log")
    ; ("log1p", Exists "Math.log1p")
    ; ("cos", Exists "Math.cos")
    ; ("sin", Exists "Math.sin")
    ; ("tan", Exists "Math.tan")
    ; ("acos", Exists "Math.acos")
    ; ("asin", Exists "Math.asin")
    ; ("atan", Exists "Math.atan")
    ; ("sqrt", Exists "Math.sqrt") ]

let string_operations =
  Assoc.of_list [("string_length", Exists "a => a.length")]

let conversion_functions =
  Assoc.of_list
    [ ("to_string", Exists "String")
    ; ("float_of_int", Exists "x => x")
    ; ("int_of_float", Exists "x => ~~x") ]

let other =
  Assoc.of_list
    [("throw", Exists "function(x) { throw x; }")]

let values =
  comparison_functions 
  |> Assoc.concat constants
  |> Assoc.concat arithmetic_operations
  |> Assoc.concat string_operations
  |> Assoc.concat conversion_functions
  |> Assoc.concat other
