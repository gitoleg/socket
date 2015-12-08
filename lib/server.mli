type t 

(** [on_port ~max port] - creates a server on [port] 
    that ready to serve up to [max] connections.
    if [holdon] is true then ignores every failed writing to
    client, otherwise drops such client. Default to false. *)
val of_port: ?holdon:bool -> ?max:int -> int -> t

exception Shutdowned
(** Exception raised when trying to work with server
    after it was shutdowned *)

val send: t -> string -> unit 
val clients: t -> int Lwt.t
val address: t -> Unix.sockaddr Lwt.t
val no_clients: t -> bool Lwt.t

(** [shutdown server] - stops the server and close all
    active connections *)
val shutdown : t -> unit
