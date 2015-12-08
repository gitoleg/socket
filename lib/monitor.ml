
open Lwt

type fd = Lwt_unix.file_descr

type msg = [ 
  | `Accept
  | `New_connection of fd
  | `Stop
]

type t = (fd list, msg) Pipe.pipe_end

let new_connection sock = 
  lwt fd, _ = Lwt_unix.accept sock in
  return (`New_connection fd)

let run sock pipe_end = 
  let get_msg () = Pipe.next pipe_end in
  let rec run' fds = 
    match_lwt 
      Lwt.pick [new_connection sock; get_msg ()] with
    | `New_connection fd -> run' (fd::fds)
    | `Accept -> Pipe.push pipe_end fds; run' [] 
    | `Stop -> return_unit in
  ignore (run' [])

let create sock =
  let fst_end, snd_end = Pipe.create () in
  let () = run sock fst_end in
  snd_end

let accept pipe_end =  
  let () = Pipe.push pipe_end `Accept in
  Pipe.next pipe_end

let stop pipe_end = Pipe.push pipe_end `Stop
