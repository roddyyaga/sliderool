open Core

type request =
  | Get
  | Head
  | Post
  | Put
  | Delete
  | Connect
  | Options
  | Trace
  | Patch

let request_from_string s =
  match s with
  | "GET" -> Some Get
  | "HEAD" -> Some Head
  | "POST" -> Some Post
  | "PUT" -> Some Put
  | "DELETE" -> Some Delete
  | "CONNECT" -> Some Connect
  | "OPTIONS" -> Some Options
  | "TRACE" -> Some Trace
  | "PATCH" -> Some Patch
  | _ -> None

let protocol_from_string s = match s with "HTTP/1.1" -> `Http_11 | _ -> `Other

type protocol = [ `Http_11 | `Other ]

module type Log_line = sig
  type t

  val pattern : string

  val from_list : string list -> t option
end

module Default_nginx = struct
  type t = {
    address: string;
    user: string option;
    timestamp: string;
    request: request;
    route: string;
    protocol: protocol;
    status: int;
    bytes_sent: int;
    referrer: string option;
    user_agent: string;
  }

  let pattern =
    "$address - $user \\\\[$timestamp\\\\] \"$request $route $protocol\" \
     $status $bytes_sent \"$referrer\" \"$user_agent\""

  let from_list variables =
    match variables with
    | [
     address;
     user_string;
     timestamp_string;
     request_string;
     route;
     protocol_string;
     status_string;
     bytes_sent_string;
     referrer_string;
     user_agent;
    ] -> (
        let user = if user_string = "-" then None else Some user_string in
        let timestamp = timestamp_string in
        let request_opt = request_from_string request_string in
        let protocol = protocol_from_string protocol_string in
        let status = int_of_string status_string in
        let bytes_sent = int_of_string bytes_sent_string in
        let referrer =
          if referrer_string = "-" then None else Some referrer_string
        in
        match request_opt with
        | Some request ->
            Some
              {
                address;
                user;
                timestamp;
                request;
                route;
                protocol;
                status;
                bytes_sent;
                referrer;
                user_agent;
              }
        | None -> None )
    | _ -> None
end
