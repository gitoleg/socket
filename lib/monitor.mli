
(** module for handling server connections *)

type t

(** [create socket] - creates a monitor from bound socket *)
val create: Lwt_unix.file_descr -> t

(** [accept monitor] - every new call returns a 
    FRESH list of connected file descriptors,
    without blocking at Lwt_unix.accept. Monitor
    does remember only untaken file descriptors. *)
val accept: t -> Lwt_unix.file_descr list Lwt.t

(** [stop monitor] - stops monitor forever *)
val stop: t -> unit


