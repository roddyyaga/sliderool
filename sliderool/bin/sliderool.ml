open Core

let analyse =
  Command.basic ~summary:"Analyse some logs"
    (Command.Param.return (fun () ->
         match In_channel.input_line Pervasives.stdin with
         | Some line ->
             Lib.Log_format_lexer.print_parse
               (Lexing.from_string line)
         | None -> ()))

let () = Command.run analyse
