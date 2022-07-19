(** The type of a Host function implementation *)
type host_func =
  Input_buffer.t ->
  Instance.module_inst ref ->
  Values.value list ->
  Values.value list Lwt.t

(** [register ~func_name implem] registers the implementation of a
    named host function in the global symbol table. Will erase a
    previous implementation for the given name. *)
val register : global_name:string -> host_func -> unit

(** [lookup ~func_name] looks for the implementation of a named host
    function in the global symbol table. May raise [Not_found].*)
val lookup : global_name:string -> host_func
