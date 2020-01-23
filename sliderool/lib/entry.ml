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

type visitor = { ip: string; date: Date.t; user_agent: string }

type t = {
  timestamp: Date.t;
  remote_user: string option;
  request_type: request;
  route: string;
  status: int;
  response_size: int;
  referrer: string option;
  visitor: visitor;
}

let nginx_default_pattern =
  Re.Str.regexp
    {re|\(.*\) - \(.*\) \[\(.*\)\] "\(.*\) \(.*\) .*" \(.*\) \(.*\) "\(.*\)" "\(.*\)"|re}

(*let from_nginx_default line =
  match Re.Str.string_match nginx_default_pattern line 0 with
  | true -> (
      match
        List.map
          ~f:(fun i -> Re.Str.matched_group i line)
          [ 1; 2; 3; 4; 5; 6; 7; 8; 9 ]
      with
      | [
       ip;
       remote_user_string;
       timestamp_string;
       request_type_string;
       route_string;
       status_string;
       response_size_string;
       referrer_string;
       user_agent;
      ] ->
          ""
      | _ -> "a" )
  | false -> "didn match!"*)
