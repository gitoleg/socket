

type t 

type 'a result = ('a, exn) Result.t
    
val of_port: int -> t result Lwt.t
val of_addr: Unix.sockaddr -> t result Lwt.t
val recv_line: t -> string result Lwt.t
val recv: t -> ?from:int -> ?upto:int -> Bytes.t -> unit result Lwt.t
