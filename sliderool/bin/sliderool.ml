open Core

let analyse =
  Command.basic ~summary:"Analyse some logs"
    (Command.Param.return (fun () -> ()))

let () = Command.run analyse
