(** Description of what memories are currently available. *)
type available_memories =
  | No_memories_during_init
  | Available_memories of Instance.memory_inst Instance.Vector.t

type reveal_destination = {base : int32; max_bytes : int32}

type reveal = Reveal_raw_data of string | Reveal_metadata

type ticks = Z.t

type reveal_func =
  available_memories ->
  Values.value list ->
  (reveal * reveal_destination, int32) result Lwt.t

(** The type of a Host function implementation *)
type host_func =
  | Host_func of
      (Input_buffer.t ->
      Output_buffer.t ->
      Durable_storage.t ->
      available_memories ->
      Values.value list ->
      (Durable_storage.t * Values.value list * ticks) Lwt.t)
  | Reveal_func of reveal_func

(** An (immutable) host function registry builder that can be turned
    into a registry using {!construct}. *)
type builder

(** A registry builder without any host functions. *)
val empty_builder : builder

(** [with_host_function ~global_name ~implem] adds the implementation
    of a named host function in the builder. Will erase a previous
    implementation for the given name. *)
val with_host_function :
  global_name:string -> implem:host_func -> builder -> builder

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

(** [construct builder] creates a new registry from the blueprint
    encoded in [builder]. *)
val construct : builder -> registry
