open Lwt

open Result

type t = Lwt_unix.file_descr

type 'a result = ('a, exn) Result.t

let create port =
  let addr = Unix.(ADDR_INET (inet_addr_any, port)) in
  let sock = Lwt_unix.(socket PF_INET SOCK_STREAM 0) in
  try_lwt
    lwt () = Lwt_unix.connect sock addr in
    return (Done sock)
  with exn -> return (Fail exn)

let read sock buf start size = 
  let rec read' start to_read = 
    lwt n = Lwt_unix.recv sock buf start to_read [] in
    if n < to_read then read' (start + n) (to_read - n) 
    else return_unit in
  try_lwt
    read' start  size >>= fun () -> return (Done ())
  with exn -> return (Fail exn)

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
    return (Fail (Invalid_argument err_str))
  else read sock buf from to_read

let recv_line sock = 
  let chan = Lwt_io.(of_fd sock ~mode:input) in
  try_lwt
    Lwt_io.read_line chan >>= fun str -> return (Done str)
  with exn -> return (Fail exn)  
