open Tezos_scoru_wasm
open Tezos_webassembly_interpreter

type error_kind =
  [ `Decode
  | `Eval
  | `Init
  | `Invalid_state
  | `Link
  | `No_fallback_decode
  | `No_fallback_init
  | `No_fallback_link
  | `Too_many_reboots
  | `Too_many_ticks
  | `Unknown ]

module type S = sig
  type t

  type tree

  val initial_tree :
    version:Wasm_pvm_state.version ->
    ?tree:tree ->
    ?ticks_per_snapshot:int64 ->
    ?max_reboots:Z.t ->
    ?from_binary:bool ->
    ?outbox_validity_period:int32 ->
    ?outbox_message_limit:Z.t ->
    string ->
    tree Lwt.t

  val input_info : int32 -> Z.t -> Wasm_pvm_state.input_info

  val reveal_builtins : Builtins.reveals

  val eval_until_stuck :
    ?reveal_builtins:Builtins.reveals ->
    ?write_debug:Builtins.write_debug ->
    ?max_steps:int64 ->
    tree ->
    (Wasm_pvm_errors.t * tree) tzresult Lwt.t

  val eval_to_snapshot :
    ?reveal_builtins:Builtins.reveals ->
    ?write_debug:Builtins.write_debug ->
    ?max_steps:int64 ->
    tree ->
    tree Lwt.t

  val eval_until_input_requested :
    ?reveal_builtins:Builtins.reveals option ->
    ?write_debug:Builtins.write_debug ->
    ?after_fast_exec:(unit -> unit) ->
    ?fast_exec:bool ->
    ?max_steps:int64 ->
    tree ->
    tree Lwt.t

  val eval_until_input_or_reveal_requested :
    ?write_debug:Builtins.write_debug ->
    ?after_fast_exec:(unit -> unit) ->
    ?fast_exec:bool ->
    ?max_steps:int64 ->
    tree ->
    tree Lwt.t

  val set_sol_input : int32 -> tree -> tree Lwt.t

  val set_protocol_migration_input :
    Pvm_input_kind.protocol -> int32 -> tree -> tree Lwt.t

  val set_info_per_level_input :
    ?migration_block:bool -> int32 -> tree -> tree Lwt.t

  val set_eol_input : int32 -> Z.t -> tree -> tree Lwt.t

  val set_raw_message : int32 -> Z.t -> string -> tree -> tree Lwt.t

  val set_input_step : string -> int -> tree -> tree Lwt.t

  val set_internal_message : int32 -> Z.t -> string -> tree -> tree Lwt.t

  val set_inputs_step :
    ?migrate_to:Pvm_input_kind.protocol ->
    (int32 -> Z.t -> 'a -> tree -> tree Lwt.t) ->
    'a trace ->
    int32 ->
    tree ->
    tree Lwt.t

  val set_full_input_step :
    ?migrate_to:Pvm_input_kind.protocol ->
    string list ->
    int32 ->
    tree ->
    tree Lwt.t

  val set_empty_inbox_step :
    ?migrate_to:Pvm_input_kind.protocol -> int32 -> tree -> tree Lwt.t

  val set_full_input_step_gen :
    ?migrate_to:Pvm_input_kind.protocol ->
    (int32 -> Z.t -> 'a -> tree -> tree Lwt.t) ->
    'a list ->
    int32 ->
    tree ->
    tree Lwt.t

  val set_full_raw_input_step :
    ?migrate_to:Pvm_input_kind.protocol ->
    string trace ->
    int32 ->
    tree ->
    tree Lwt.t

  val eval_until_init : tree -> tree Lwt.t

  val eval_to_result :
    ?write_debug:Builtins.write_debug ->
    ?reveal_builtins:Builtins.reveals ->
    tree ->
    (tree * int64) Lwt.t

  val pp_state :
    Format.formatter -> Wasm_pvm_state.Internal_state.tick_state -> unit

  val print_error_state : Wasm_pvm_errors.t -> string

  (** [check_error kind reason error] checks a Wasm PVM error [error] is of a
      given [kind] with a possible [reason].

      - If [kind] is [None], returns true.

      - If [reason] is [None], it simply check the given kind, otherwise it
      actually check the reason in the error. *)
  val check_error :
    ?expected_kind:error_kind ->
    ?expected_reason:string ->
    Wasm_pvm_errors.t ->
    bool

  val is_stuck :
    ?step:error_kind ->
    ?reason:string ->
    Wasm_pvm_state.Internal_state.tick_state ->
    bool

  val wrap_as_durable_storage : tree -> Durable_storage.t Lwt.t

  val has_stuck_flag : tree -> bool Lwt.t

  val make_durable : (string * string) list -> Durable_storage.t Lwt.t

  val make_module_inst :
    version:Wasm_pvm_state.version ->
    string list ->
    int32 ->
    Instance.module_inst Instance.ModuleMap.t
    * Instance.module_key
    * Host_funcs.registry

  val retrieve_memory :
    Instance.module_inst Instance.ModuleMap.t -> Partial_memory.memory Lwt.t

  module Ctx :
    Tezos_tree_encoding.Encodings_util.S
      with type Tree.tree = tree
      with type t = t

  module Tree_encoding_runner :
    Tezos_tree_encoding.Runner.S with type tree = tree

  module Wasm : Wasm_pvm_sig.S with type tree = tree

  module Wasm_fast : Wasm_pvm_sig.S with type tree = tree
end
