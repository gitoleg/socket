
open Lwt

open Result

type fd = Lwt_unix.file_descr

(** requests to running server  *)
type requests = [
  | `Request_count  (** clients count  *)
  | `Request_addr   (** server address *)
]

exception Shutdowned
exception Request_failed of string

(** messages to running server  *)
type server_msgs = [ 
  | requests
  | `Data of string
  | `Stop      
]

(** answers from running server *)
type msg = [
  | `Clients of int
  | `Address of Unix.sockaddr
]

type t = (msg, server_msgs) Pipe.pipe_end

let create_socket max port =
  let () = Sys.(set_signal sigpipe Signal_ignore) in
  let addr = Unix.(ADDR_INET (inet_addr_loopback, port)) in
  let sock = Lwt_unix.(socket PF_INET SOCK_STREAM 0) in
  Lwt_unix.setsockopt sock Lwt_unix.SO_REUSEADDR true;
  Lwt_unix.bind sock addr;
  Lwt_unix.listen sock max;
  sock

let send_to_client (id, fd) msg =
  let rec send' start to_send =
    lwt n = Lwt_unix.send fd msg start to_send [] in
    if start + n = to_send then return_unit
    else send' (start + n) (to_send - n) in
  try_lwt
    lwt () = send' 0 (String.length msg) in
    return (Done ())
  with Unix.Unix_error (er,_,_) ->
    let str = Unix.error_message er in
    return (Fail (id,str))

let accept_connections monitor clients =
  lwt fds = Monitor.accept monitor in
  let () = List.iter (fun fd -> 
      let id = Uuidm.create `V4 in
      Hashtbl.add clients id fd) fds in
  return_unit 

let run_server ~holdon ~max ~port pipe_end =
  let sock = create_socket max port in
  let clients = Hashtbl.create max in
  let monitor = Monitor.create sock in
  let remove_client id fd = 
    try_lwt 
      Hashtbl.remove clients id;
      Lwt_unix.close fd
    with _ -> return_unit in
  let rec run' () = 
    lwt () = accept_connections monitor clients in
    match_lwt Pipe.get pipe_end with 
    | None -> run' ()
    | Some msg -> match msg with
      |`Request_count ->
        let count = Hashtbl.length clients in
        Pipe.push pipe_end (`Clients count);
        run' () 
      | `Request_addr ->
        Pipe.push pipe_end (`Address (Lwt_unix.getsockname sock));
        run' ()
      | `Data data ->
        let clients' = 
          Hashtbl.fold (fun id fd acc -> (id,fd) :: acc) clients [] in
        lwt result = 
          Lwt_list.map_p (fun c -> send_to_client c data) clients' in      
        let () = 
          if not holdon then
            List.iter (fun r -> match r with 
                | Done () -> ()
                | Fail (id,fd) -> Hashtbl.remove clients id) result in
        run' () 
      | `Stop -> 
        lwt () = Hashtbl.fold (fun id fd return_unit -> 
            return_unit >>= fun () -> remove_client id fd) 
            clients return_unit in
        Pipe.close_end pipe_end;
        Lwt_unix.close sock in
  run' ()

let of_port ?(holdon=false) ?(max=10) port : t =   
  let pipe_end, pipe_end' = Pipe.create () in
  ignore (run_server ~holdon ~max ~port pipe_end');
  pipe_end

let request_error msg = fail (Request_failed msg)

let push_request pipe_end request =
  try
    Pipe.push pipe_end request;
    return_unit
  with Pipe.Closed -> fail Shutdowned
  
let clients pipe_end = 
  lwt () = push_request pipe_end `Request_count in
  match_lwt Pipe.next pipe_end with 
  | `Clients n -> return n
  | _ -> request_error "clients count"

let address pipe_end = 
  lwt () = push_request pipe_end `Request_addr in
  match_lwt Pipe.next pipe_end with
  | `Address a -> return a
  | _ -> request_error "server address" 

let no_clients t = 
  lwt count = clients t in
  return (count = 0)

let send pipe_end msg = 
  try
    Pipe.push pipe_end (`Data msg)
  with Pipe.Closed -> raise Shutdowned

let shutdown pipe_end = 
  try
    Pipe.push pipe_end `Stop;
    Pipe.close_end pipe_end
  with Pipe.Closed -> raise Shutdowned

