open Core
open Lib.Log_format_lexer

let parse_string s = parse (Lexing.from_string s)

let string_concat_gen first_gen second_gen =
  let open QCheck.Gen in
  map2 (fun x y -> x ^ y) first_gen second_gen

(** A quickcheck generator for lists of tokens
    with the property that converting them to a string and parsing
    should be idempotent.

    In other words, it should {b not} generate lists such as
    [[VARIABLE "$abc"; STRING "def"; EOF]], since the correct parse of
    ["$abcdef"] is [[VARIABLE "$abcdef"]] rather than that list. *)
let valid_token_list_gen =
  let open QCheck.Gen in
  let small_nat_nonzero = int_range 1 99 in
  let small_string_nonempty ?gen = string_size small_nat_nonzero ?gen in
  (* Generate characters that can be part of a variable name
   * (not including the initial '$' *)
  let variable_name_content_char =
    oneof [ char_range 'a' 'z'; numeral; return '_' ]
  in

  (* Generate a valid variable name *)
  let variable_name_gen =
    string_concat_gen (return "$")
      (small_string_nonempty ~gen:variable_name_content_char)
  in

  (* Generate a character that can't be part of a variable name
   * and can therefore start any string *)
  let always_valid_string_start_chars =
    String.to_list " !@£#%^&*()-=+[]{};$\\':\"|,./<>?`~\n\t"
  in
  let string_prefix_gen =
    string_size
      ~gen:(oneof (List.map ~f:return always_valid_string_start_chars))
      (return 1)
  in

  (* Generate the content of a string that is the first token *)
  let initial_string_gen = small_string_nonempty ~gen:printable in

  (* Generate the content of a string that isn't necessarily the first token *)
  let non_initial_string_gen =
    string_concat_gen string_prefix_gen
      (* Prefix guaranteed non-empty so don't need [small_string_nonempty] *)
      (small_string ~gen:printable)
  in

  (* Generate the token list by combining sublists,
   * to avoid generating lists with consecutive [STRING]s
   * which are invalid. *)
  let singleton_list_gen gen = list_repeat 1 gen in
  let pair_list_gen = map2 (fun x y -> [ x; y ]) in
  let variable_gen = map (fun s -> VARIABLE s) variable_name_gen in
  (* Generate a token that isn't the first or last *)
  let middle_token_list_gen =
    oneof
      [
        singleton_list_gen variable_gen;
        pair_list_gen variable_gen
          (map (fun s -> STRING s) non_initial_string_gen);
      ]
  in

  (* Generate the first token of a list with at least two elements *)
  let initial_token_list_gen =
    oneof
      [
        list_repeat 0 (return EOF);
        singleton_list_gen (map (fun s -> STRING s) initial_string_gen);
      ]
  in
  let eof = singleton_list_gen (return EOF) in
  let ( @@@ ) = map2 (fun xs ys -> xs @ ys) in
  let flatten xs_gen state = List.join (xs_gen state) in
  let non_empty_gen =
    singleton_list_gen initial_token_list_gen
    @@@ small_list middle_token_list_gen
    @@@ singleton_list_gen eof
  in
  flatten non_empty_gen

let unescape s =
  s
  |> String.substr_replace_all ~pattern:"\\" ~with_:"\\\\"
  |> String.substr_replace_all ~pattern:"$" ~with_:"\\$"

let from_list tokens =
  tokens
  |> List.map ~f:(function
       | VARIABLE s -> s
       | STRING s -> unescape s
       | EOF -> "")
  |> String.concat

let print_tokens tokens =
  let open Printf in
  tokens
  |> List.map ~f:(function
       | VARIABLE s -> sprintf "VARIABLE(\"%s\")" s
       | STRING s -> sprintf "STRING(\"%s\")" s
       | EOF -> "EOF")
  |> String.concat ~sep:"\n"

let arbitrary_token_list = QCheck.make ~print:print_tokens valid_token_list_gen

let debug_print ts parsed =
  print_endline "expected:";
  print_endline (print_tokens ts);
  print_endline "got:";
  print_endline (print_tokens parsed)

let token_list_quickcheck =
  QCheck.Test.make ~name:"token list quickcheck" ~count:10000
    arbitrary_token_list (fun ts ->
      let parsed = parse_string (from_list ts) in
      debug_print ts parsed;
      ts = parsed)

let test_empty () =
  Alcotest.(check string)
    "empty token list"
    (from_list (parse_string ""))
    (from_list [ EOF ])

let test_initial_backslash () =
  Alcotest.check_raises "initial backslash error"
    (ParseError Errors.initial_backslash) (fun () ->
      let _ = parse_string "\\abcdefg" in
      ())

let test_string_backslash () =
  Alcotest.check_raises "string backslash error"
    (ParseError Errors.string_backslash) (fun () ->
      let _ = parse_string "gfedcba\\abcdefg" in
      ())

let test_variable_backslash () =
  Alcotest.check_raises "string backslash error"
    (ParseError Errors.variable_backslash) (fun () ->
      let _ = parse_string "$gfedcba\\abcdefg" in
      ())

let () =
  let quickcheck =
    List.map ~f:QCheck_alcotest.to_alcotest [ token_list_quickcheck ]
  in
  let open Alcotest in
  run "Log format string parser"
    [
      ("quickcheck", quickcheck);
      ("empty", [ test_case "empty" `Quick test_empty ]);
      ( "invalid format strings",
        [
          test_case "initial backslash" `Quick test_initial_backslash;
          test_case "string backslash" `Quick test_string_backslash;
          test_case "variable backslash" `Quick test_variable_backslash;
        ] );
    ]
