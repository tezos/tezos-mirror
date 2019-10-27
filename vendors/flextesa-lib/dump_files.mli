(** Write little file trees, and save them in the state. *)

open Internal_pervasives

type t = private
  {mutable trees: (string * string * (string * string) list) list}

val make : unit -> t

val write :
     < application_name: string
     ; dump_files: t
     ; paths: Paths.t
     ; runner: Running_processes.State.t
     ; .. >
  -> name:string
  -> path:string
  -> (string * string) list
  -> ( unit
     , [> System_error.t | `Wrong_status of Process_result.t * string] )
     Asynchronous_result.t

val pp : Format.formatter -> t -> unit
