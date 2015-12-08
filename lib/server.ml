
open Lwt

open Result

type fd = Lwt_unix.file_descr

type server_info = {
  clients : int;
  address : Unix.sockaddr;
}

exception Shutdowned

(** messages to running server  *)
type server_msgs = [ 
  | `Request_info
  | `Send of string
  | `Stop      
]

type t = (server_info, server_msgs) Pipe.pipe_end

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
      |`Request_info ->
        let clients = Hashtbl.length clients in
        let address = Lwt_unix.getsockname sock in
        let info = {clients; address} in
        Pipe.push pipe_end info;
        run' () 
      | `Send data ->
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

let push_request pipe_end request =
  try
    Pipe.push pipe_end request;
    return_unit
  with Pipe.Closed -> fail Shutdowned

let get_info pipe_end = 
  lwt () = push_request pipe_end `Request_info in
  Pipe.next pipe_end 

let clients pipe_end = 
  lwt info = get_info pipe_end in
  return info.clients

let address pipe_end = 
  lwt info = get_info pipe_end in
  return info.address

let no_clients t = 
  lwt count = clients t in
  return (count = 0)

let send pipe_end msg = 
  try
    Pipe.push pipe_end (`Send msg)
  with Pipe.Closed -> raise Shutdowned

let shutdown pipe_end = 
  try
    Pipe.push pipe_end `Stop;
    Pipe.close_end pipe_end
  with Pipe.Closed -> raise Shutdowned

