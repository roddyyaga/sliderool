open Core

type variable = Any | Nonempty | Pattern of string

(** Given a list of parsed tokens
   and a map from variable names to types of regex pattern,
   produce a regular expression for parsing the relevant kind of logs *)
let build token_list variable_patterns =
  let token_to_regex token =
    let open Log_format_lexer in
    match token with
    | STRING s -> s
    | VARIABLE s -> (
        match Map.find variable_patterns s with
        | Some Any | None -> "(.*)"
        | Some Nonempty -> "(.+)"
        | Some (Pattern p) -> Printf.sprintf "(%s)" p )
    | EOF -> ""
  in
  String.concat (List.map ~f:token_to_regex token_list)
