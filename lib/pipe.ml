
open Lwt

type ('a, 'b) pipe_end = 'a Lwt_stream.t * ('b option -> unit)

type ('a, 'b) t = ('a, 'b) pipe_end * ('b, 'a) pipe_end

exception Closed

let create () = 
  let take_side, push = Lwt_stream.create () in
  let take_side', push' = Lwt_stream.create () in
  (take_side, push'), (take_side', push)

let push (_, push) value = 
  try
    push (Some value)
  with Lwt_stream.Closed -> raise Closed

(** we use the same exception as for closed pipe,
    because Lwt_stream.next raises an exception only
    if stream is closed *)
let next (stream,_) = 
  try_lwt
    Lwt_stream.next stream
  with Lwt_stream.Empty -> fail Closed  

let get (stream,_) = Lwt_stream.get stream

let close_end (_,push) = push None
let close (p,p')  = close_end p; close_end p'
