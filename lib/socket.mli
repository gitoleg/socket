
type ('a, 'b) io_result = 
  | Ok of 'a
  | Error of 'b

module Server : sig

  type t 

  (** [on_port ~max port] - creates a server on [port] 
      that ready to serve up to [max] connections.
      if [holdon] is true then ignores every failed writing to
      client, otherwise drops such client. Default to false. *)
  val of_port: ?holdon:bool -> ?max:int -> int -> t

  val send: t -> string -> unit Lwt.t
  val clients: t -> int
  val address: t -> Unix.sockaddr
  val no_clients: t -> bool

  (** [shutdown server] - stops the server and close all
      active connections *)
  val shutdown : t -> unit Lwt.t

end

module Client : sig 

  type t 
  type 'a result = ('a, exn) io_result

  val create: int -> t result Lwt.t
  val recv_line: t -> string result Lwt.t
  val recv: t -> ?from:int -> ?upto:int -> Bytes.t -> unit result Lwt.t

end
