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
  -> (unit, [> System_error.t | Process_result.Error.t]) Asynchronous_result.t

val pp : t Fmt.t
