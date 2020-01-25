open Core

let default_pattern =
  "$address - $user \\\\[$timestamp\\\\] \"$request_type $route $protocol\" \
   $status $bytes_sent \"$referrer\" \"$user_agent\""

let default_matcher = Lib.Regex_builder.build_matcher default_pattern

let nginx_matcher =
  Lib.Regex_builder.build_matcher Lib.Formats.Default_nginx.pattern

let rec match_lines matcher read_line =
  match read_line () with
  | Some line -> (
      match matcher line with
      | Some values ->
          List.iter ~f:print_endline values;
          match_lines matcher read_line
      | None ->
          print_endline ("No match: " ^ line);
          match_lines matcher read_line )
  | None -> ()

let user_summary read_line =
  let rec get_lines read_line acc =
    match read_line () with
    | Some line -> get_lines read_line (line :: acc)
    | None -> acc
  in
  let lines =
    List.map
      ~f:(fun line ->
        match nginx_matcher line with
        | Some blah -> Lib.Formats.Default_nginx.from_list blah
        | None -> None)
      (get_lines read_line [])
    |> List.filter_opt
  in
  let open Lib.Formats.Default_nginx in
  let agents =
    List.dedup_and_sort ~compare:String.compare
      (List.map ~f:(fun r -> r.user_agent) lines)
  in
  List.iter ~f:print_endline agents;
  print_endline (List.length agents |> string_of_int);
  print_endline (List.length lines |> string_of_int)

let analyse =
  Command.basic ~summary:"Analyse some logs"
    (Command.Param.return (fun () ->
         user_summary (fun () -> In_channel.input_line Pervasives.stdin)))

let () = Command.run analyse
