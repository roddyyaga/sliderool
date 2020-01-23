{
open Lexing

type token = VARIABLE of string | STRING of string | EOF

type state = Initial | Variable | String | Eof

let token_to_string token =
    let open Printf in
    match token with
    | VARIABLE s -> sprintf "VARIABLE(%s)" s
    | STRING s -> sprintf "%s" s
    | EOF -> "EOF"

let add_and_continue buffer lexbuf new_state =
  let () = Buffer.add_string buffer (lexeme lexbuf) in
  (None, buffer, new_state)

let escape s = match s with "\\$" -> "$" | "\\\\" -> "\\" | _ -> assert false

let add_escaped buffer lexbuf =
  let () = Buffer.add_string buffer (escape (lexeme lexbuf)) in
  (None, buffer, String)

let finish_token_start_next buffer next_token_prefix old_state new_state =
  let token_contents = Buffer.contents buffer in
  let finished_token =
    match old_state with
    | Variable -> VARIABLE token_contents
    | String -> STRING token_contents
    | _ -> assert false
  in
  let new_buffer = Buffer.create 16 in
  let () = Buffer.add_string new_buffer next_token_prefix in
  (Some finished_token, new_buffer, new_state)

module Errors = struct
    let initial_backslash = "\\ in initial state"
    let variable_backslash = "\\ in variable state"
    let string_backslash = "\\ in string state"
end

exception ParseError of string
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let variable = alpha | digit | '_'
let valid_escaped = '\\' '$' | '\\' '\\'

rule parse_initial buffer = parse
    | '$'                { add_and_continue buffer lexbuf Variable }
    | valid_escaped      { add_escaped buffer lexbuf }
    | '\\'                { raise (ParseError Errors.initial_backslash)  }
    | _                  { add_and_continue buffer lexbuf String }
    | eof                { (Some EOF, buffer, Eof) }

and parse_variable buffer = parse
    | variable        { add_and_continue buffer lexbuf Variable }
    | valid_escaped   { finish_token_start_next buffer (escape (lexeme lexbuf)) Variable String }
    | '\\'             { raise (ParseError Errors.variable_backslash)  }
    | '$'             { finish_token_start_next buffer (lexeme lexbuf) Variable Variable }
    | _               { finish_token_start_next buffer (lexeme lexbuf) Variable String }
    | eof             { (Some (VARIABLE (Buffer.contents buffer)), buffer, Eof) }

and parse_string buffer = parse
    | '$'             { finish_token_start_next buffer (lexeme lexbuf) String Variable }
    | valid_escaped   { add_escaped buffer lexbuf }
    | '\\'             { raise (ParseError Errors.string_backslash)  }
    | _               { add_and_continue buffer lexbuf String }
    | eof             { (Some (STRING (Buffer.contents buffer)), buffer, Eof) }

{
let parse lexbuf =
  let rec loop acc (token_opt, buffer, state) =
    match state with
    | Initial -> loop acc (parse_initial buffer lexbuf)
    | String | Variable -> (
        let next_rule =
          if state = String then parse_string else parse_variable
        in
        match token_opt with
        | None -> loop acc (next_rule buffer lexbuf)
        | Some token -> loop (token :: acc) (next_rule buffer lexbuf) )
    | Eof -> (
        match token_opt with
        | Some EOF -> List.rev (EOF :: acc)
        | Some other -> List.rev (EOF :: (other :: acc))
        | _ -> assert false )
  in
  loop [] (None, Buffer.create 16, Initial)

let print_parse lexbuf =
    parse lexbuf
    |> List.map token_to_string
    |> String.concat "\n"
    |> print_endline

let parse_string s = parse (Lexing.from_string s)
}
