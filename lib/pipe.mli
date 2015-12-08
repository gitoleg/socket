
(** module for communication between lwt threads. *)

(** pipe end it's a source of 'a and a sink of 'b.
    Actually, pipe end is build on lwt stream *)
type ('a, 'b) pipe_end = private 'a Lwt_stream.t * ('b option -> unit)

(** pipe consists of two pipe ends. Suddenly. *)
type ('a, 'b) t = ('a, 'b) pipe_end * ('b, 'a) pipe_end

exception Closed

val create: unit -> ('a, 'b) t

(** [push pipe_end value] - pushes a value into pipe end.
    fail with {!Closed} if the pipe is closed. *)
val push: ('a, 'b) pipe_end -> 'b -> unit

(** [next st] remove and returns the next element of the pipe, or
    fail with {!Closed} if the pipe is closed. *)
val next: ('a, 'b) pipe_end -> 'a Lwt.t

(** [get pipe] remove and returns the first element of the pipe, if
    any. *)
val get: ('a, 'b) pipe_end -> 'a option Lwt.t

val close: ('a, 'b) t -> unit
val close_end: ('a, 'b) pipe_end -> unit
