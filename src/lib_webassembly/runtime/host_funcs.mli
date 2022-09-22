(** Description of what memories are currently available. *)
type available_memories =
  | No_memories_during_init
  | Available_memories of Instance.memory_inst Instance.Vector.t

type reveal_tick_kind = Preimage

(** The type of a Host function implementation *)
type host_func =
  | Host_func of
      (Input_buffer.t ->
      Output_buffer.t ->
      Durable_storage.t ->
      available_memories ->
      Values.value list ->
      (Durable_storage.t * Values.value list) Lwt.t)
  | Reveal_tick of reveal_tick_kind

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
