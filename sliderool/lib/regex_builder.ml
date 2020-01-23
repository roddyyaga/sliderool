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
        | Some Any | None -> "\\(.*\\)"
        | Some Nonempty -> "\\(.+\\)"
        | Some (Pattern p) -> Printf.sprintf "\\(%s\\)" p )
    | EOF -> ""
  in
  String.concat (List.map ~f:token_to_regex token_list)

let build_matcher log_format ?variable_patterns =
  let token_list = Log_format_lexer.parse_string log_format in
  let variable_patterns =
    Option.value ~default:(Map.empty (module String)) variable_patterns
  in
  let _, group_indices =
    List.fold ~init:(1, [])
      ~f:(fun (next_index, previous_indices) token ->
        let open Log_format_lexer in
        match token with
        | EOF -> (next_index, previous_indices)
        | VARIABLE _ -> (next_index + 1, next_index :: previous_indices)
        | STRING s -> (
            let pattern = Re.Str.regexp ".*\\\\(.*\\\\).*" in
            match Re.Str.string_match pattern s 0 with
            | true ->
                (* There will be a false positive group in the generated pattern *)
                (next_index + 1, previous_indices)
            | false -> (next_index, previous_indices) ))
      token_list
  in
  let group_indices = List.rev group_indices in
  let pattern = Re.Str.regexp (build token_list variable_patterns) in
  let matcher line =
    match Re.Str.string_match pattern line 0 with
    | true ->
        Some (List.map ~f:(fun i -> Re.Str.matched_group i line) group_indices)
    | false -> None
  in
  matcher
