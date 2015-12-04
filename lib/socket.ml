
open Lwt
open Lwt_log

type id = string 
type fd = Lwt_unix.file_descr
type ('a, 'b) io_result = 
  | Ok of 'a
  | Error of 'b

module Pipe = struct

  type ('a, 'b) pipe_end = 'a Lwt_stream.t * ('b option -> unit)
  type ('a, 'b) t = ('a, 'b) pipe_end * ('b, 'a) pipe_end

  let create () = 
    let take_side, push = Lwt_stream.create () in
    let take_side', push' = Lwt_stream.create () in
    (take_side, push'), (take_side', push)

end

module Monitor = struct

  type msg = [ 
    | `Request
    | `New_connection of fd 
    | `Stop
  ]
  
  type t = (fd list, msg) Pipe.pipe_end

  let new_connection sock = 
    lwt fd, _ = Lwt_unix.accept sock in
    return (`New_connection fd)

  let run sock (listen,push) = 
    let get_msg () = Lwt_stream.next listen in
    let rec run' fds = 
      match_lwt 
        Lwt.pick [new_connection sock; get_msg ()] with
      | `New_connection fd -> run' (fd::fds)
      | `Request -> push (Some fds); run' [] 
      | `Stop -> return_unit in
    ignore (run' [])

  let create sock =
    let fst_end, snd_end = Pipe.create () in
    let () = run sock fst_end in
    snd_end

  let stop (_, push_msgs) = push_msgs (Some `Stop)

  let accept (data, push_msgs) =  
    let () = push_msgs (Some `Request) in
    Lwt_stream.next data

end

module Server = struct 

  type t = {
    socket  : fd;
    monitor : Monitor.t;
    clients : (id,fd) Hashtbl.t ;
    holdon  : bool;
  }

  type result = (unit, (id * string)) io_result

  let create_socket max port =
    let () = Sys.(set_signal sigpipe Signal_ignore) in
    let addr = Unix.(ADDR_INET (inet_addr_loopback, port)) in
    let sock = Lwt_unix.(socket PF_INET SOCK_STREAM 0) in
    Lwt_unix.setsockopt sock Lwt_unix.SO_REUSEADDR true;
    Lwt_unix.bind sock addr;
    Lwt_unix.listen sock max;
    sock

  let of_port ?(holdon=false) ?(max=10) port = 
    let socket = create_socket max port in
    let monitor = Monitor.create socket in
    let clients = Hashtbl.create max in
    { socket; monitor; clients; holdon; }

  let update_clients {monitor; clients;} = 
    lwt fds = Monitor.accept monitor in
    let add fd =
      let id = Uuidm.(to_string (create `V4)) in
      Hashtbl.add clients id fd in
    let () = List.iter add fds in
    return_unit

  let send_to_client (id, fd) msg =
    let rec send' start to_send =
      lwt n = Lwt_unix.send fd msg start to_send [] in
      if start + n = to_send then return_unit
      else send' (start + n) (to_send - n) in
    try_lwt
      lwt () = send' 0 (String.length msg) in
      return (Ok ())
    with Unix.Unix_error (er,_,_) ->
      let str = Unix.error_message er in
      return (Error (id,str))

  let cleanup t bad_fds = 
    List.iter (fun (id,_) -> Hashtbl.remove t.clients id) bad_fds

  let send t msg = 
    lwt () = update_clients t in
    let clients' = 
      Hashtbl.fold (fun id fd acc -> (id,fd) :: acc) t.clients [] in
    lwt result = Lwt_list.map_p (fun c -> send_to_client c msg) clients'
    in 
    let () = 
      if not t.holdon then 
        let bads = List.fold_left (fun acc r ->
            match r with
            | Ok () -> acc
            | Error c -> c::acc) [] result in
        cleanup t bads in
    return_unit

  let clients t = Hashtbl.length t.clients
  let address t = Lwt_unix.getsockname t.socket
  let no_clients t = clients t = 0

  let shutdown t = 
    Monitor.stop t.monitor;
    lwt () = Hashtbl.fold (fun _ fd return_unit -> 
        return_unit >>= fun () -> Lwt_unix.close fd) 
        t.clients return_unit in
    Lwt_unix.close t.socket

end

module Client = struct 
  
  type t = fd

  type 'a result = ('a, exn) io_result

  let create port =
    let addr = Unix.(ADDR_INET (inet_addr_any, port)) in
    let sock = Lwt_unix.(socket PF_INET SOCK_STREAM 0) in
    try_lwt
      lwt () = Lwt_unix.connect sock addr in
      return (Ok sock)
    with exn -> return (Error exn)

  let read sock buf start size = 
    let rec read' start to_read = 
      lwt n = Lwt_unix.recv sock buf start to_read [] in
      if n < to_read then read' (start + n) (to_read - n) 
      else return_unit in
    try_lwt
      read' start  size >>= fun () -> return (Ok ())
    with exn -> return (Error exn)

  let recv sock ?(from=0) ?upto buf = 
    let buf_len = Bytes.length buf in
    let read_end = match upto with
      | Some v -> v
      | None -> buf_len in
    let to_read = read_end - from in
    if to_read < 0 || to_read > buf_len then 
      let err_str = 
        Printf.sprintf 
          "unable to read from %d to %d in buffer of %d length"
          from read_end buf_len in
      return (Error (Invalid_argument err_str))
    else read sock buf from to_read

  let recv_line sock = 
    let chan = Lwt_io.(of_fd sock ~mode:input) in
    try_lwt
      Lwt_io.read_line chan >>= fun str -> return (Ok str)
    with exn -> return (Error exn)  

end
