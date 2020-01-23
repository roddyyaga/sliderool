open Core

let test_basic_build () =
  Alcotest.(check string)
    "regex builder basic"
    (Lib.Regex_builder.build
       (Lib.Log_format_lexer.parse_string "$abc - $def_1 \\\\a\\\\b\\$")
       (Map.empty (module String)))
    "\\(.*\\) - \\(.*\\) \\a\\b$"

let test_basic_match () =
  let log_format = "$a abc" in
  let matcher = Lib.Regex_builder.build_matcher log_format in
  Alcotest.(check (option (list string)))
    "regex matcher basic" (matcher "123 abc")
    (Some [ "123" ])

let test_complex_match () =
  let log_format = "$a - $b -\\\\(abc\\\\) def$ghi!!!" in
  let matcher = Lib.Regex_builder.build_matcher log_format in
  Alcotest.(check (option (list string)))
    "regex matcher complex"
    (matcher "123 - 4 -abc defQQQQQQ!!!")
    (Some [ "123"; "4"; "QQQQQQ" ])

let _ =
  let open Alcotest in
  run "Regex builder"
    [
      ("building", [ test_case "basic pattern" `Quick test_basic_build ]);
      ( "matching",
        [
          test_case "basic pattern" `Quick test_basic_match;
          test_case "complex pattern" `Quick test_complex_match;
        ] );
    ]
