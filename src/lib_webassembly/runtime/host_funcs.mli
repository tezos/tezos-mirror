(** The type of a Host function implementation *)
type host_func =
  | Host_func of
      (Input_buffer.t ->
      Output_buffer.t ->
      Instance.memory_inst Instance.Vector.t ->
      Values.value list ->
      Values.value list Lwt.t)
[@@ocaml.unboxed]

(** A (mutable) host function registry *)
type registry

(** [empty ()] creates a new empty registry *)
val empty : unit -> registry

(** [register ~func_name implem] registers the implementation of a
    named host function in the global symbol table. Will erase a
    previous implementation for the given name. *)
val register : global_name:string -> host_func -> registry -> unit

(** [lookup ~func_name] looks for the implementation of a named host
    function in the global symbol table. May raise [Not_found].*)
val lookup : global_name:string -> registry -> host_func
