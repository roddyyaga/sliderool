open Core

let default_pattern =
  "$remote_addr - $remote_user \\\\[$time_local\\\\] \"$request\" $status \
   $body_bytes_sent \"$http_referer\" \"$http_user_agent\""

let default_matcher = Lib.Regex_builder.build_matcher default_pattern

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

let analyse =
  Command.basic ~summary:"Analyse some logs"
    (Command.Param.return (fun () ->
         match_lines default_matcher (fun () ->
             In_channel.input_line Pervasives.stdin)))

let () = Command.run analyse
