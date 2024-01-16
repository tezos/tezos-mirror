(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021-2023 Nomadic Labs <contact@nomadic-labs.com>           *)
(* Copyright (c) 2022-2023 TriliTech <contact@trili.tech>                    *)
(* Copyright (c) 2023 Functori <contact@functori.com>                        *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

(** Helpers built upon the Sc_rollup_node and Sc_rollup_client *)

(** Hooks that handles hashes specific to smart rollups. *)
val hooks : Tezt.Process.hooks

(** Expose Tezos_regression.rpc_hooks for regressions *)
val rpc_hooks : RPC_core.rpc_hooks

(** [hex_encode s] returns the hexadecimal representation of the given string [s]. *)
val hex_encode : string -> string

(** [read_kernel filename] reads binary encoded WebAssembly module (e.g.
    `foo.wasm`) and returns a hex-encoded Wasm PVM boot sector, suitable for
    passing to [originate_sc_rollup].
*)
val read_kernel : ?base:string -> string -> string

module Installer_kernel_config : sig
  (** Moves path [from] at path [to_]. *)
  type move_args = {from : string; to_ : string}

  (** Reveals hash [hash] at path [to_]. *)
  type reveal_args = {hash : string; to_ : string}

  (** Sets value [value] at path [to_]. *)
  type set_args = {value : string; to_ : string}

  type instr = Move of move_args | Reveal of reveal_args | Set of set_args

  (** Set of instructions used by the installer-client. *)
  type t = instr list

  (** [of_json path] parses the JSON file at [path] and returns the
      installer config.  Note that the instruction [Reveal] cannot be
      expressed in JSON because of the YAML definition, therefore [path]
      must not contain these instructions.  *)
  val of_json : string -> t
end

type installer_result = {
  output : string;  (** Output path of the boot sector. *)
  boot_sector : string;  (** Boot sector. *)
  root_hash : string;  (** Root hash of the boot sector. *)
}

(** [prepare_installer_kernel ~base_installee ~preimages_dir ?config
    installee] feeds the [smart-rollup-installer] with a kernel
    ([installee]), and returns the boot sector corresponding to the
    installer for this specific kernel. [installee] is read from
    [base_installee] on the disk.

    A {!Installer_kernel_config.t} can optionally be provided to the installer
    via [config].

    The preimages of the [installee] are saved to [preimages_dir]. This should be the
    reveal data directory of the rollup node.

    The returned installer is hex-encoded and may be passed to [originate_sc_rollup]
    directly.
*)
val prepare_installer_kernel :
  ?runner:Runner.t ->
  ?base_installee:string ->
  preimages_dir:string ->
  ?config:
    [< `Config of Installer_kernel_config.t
    | `Path of string
    | `Both of Installer_kernel_config.t * string ] ->
  string ->
  installer_result Lwt.t

(** [setup_l1 protocol] initializes a protocol with the given parameters, and
    returns the L1 node and client. *)
val setup_l1 :
  ?timestamp:Client.timestamp ->
  ?bootstrap_smart_rollups:Protocol.bootstrap_smart_rollup list ->
  ?bootstrap_contracts:Protocol.bootstrap_contract list ->
  ?commitment_period:int ->
  ?challenge_window:int ->
  ?timeout:int ->
  ?whitelist_enable:bool ->
  ?rpc_local:bool ->
  ?riscv_pvm_enable:bool ->
  Protocol.t ->
  (Node.t * Client.t) Lwt.t

(** [originate_sc_rollup] is a wrapper above {!Client.originate_sc_rollup} that
    waits for the block to be included. *)
val originate_sc_rollup :
  ?hooks:Process_hooks.t ->
  ?burn_cap:Tez.t ->
  ?whitelist:string list ->
  ?alias:string ->
  ?src:string ->
  kind:string ->
  ?parameters_ty:string ->
  ?boot_sector:string ->
  Client.t ->
  string Lwt.t

val setup_rollup :
  kind:string ->
  ?hooks:Process_hooks.t ->
  ?alias:string ->
  ?mode:Sc_rollup_node.mode ->
  ?boot_sector:string ->
  ?parameters_ty:string ->
  ?src:string ->
  ?operator:string ->
  ?operators:(Sc_rollup_node.purpose * string) list ->
  ?data_dir:string ->
  ?rollup_node_name:string ->
  ?whitelist:string list ->
  ?sc_rollup:string ->
  ?allow_degraded:bool ->
  Node.t ->
  Client.t ->
  (Sc_rollup_node.t * string) Lwt.t

val originate_forward_smart_contract :
  ?src:string -> Client.t -> Protocol.t -> string Lwt.t

(** [default_boot_sector_of k] returns a valid boot sector for a PVM of
    kind [kind]. *)
val default_boot_sector_of : kind:string -> string

val last_cemented_commitment_hash_with_level :
  sc_rollup:string -> Client.t -> (string * int) Lwt.t

(** [genesis_commitment ~sc_rollup client] returns the genesis
    commitment, fails if this commitment have been cleaned from the
    context. *)
val genesis_commitment :
  sc_rollup:string -> Client.t -> RPC.smart_rollup_commitment Lwt.t

(** [call_rpc ~smart_rollup_node ~service] call the RPC for [service] on
    [smart_rollup_node]. *)
val call_rpc :
  smart_rollup_node:Sc_rollup_node.t -> service:string -> JSON.t Lwt.t

(** Bootstrap smart rollup setup information.*)
type bootstrap_smart_rollup_setup = {
  bootstrap_smart_rollup : Protocol.bootstrap_smart_rollup;
      (** The bootstrap smart rollup to add in the parameter files. *)
  smart_rollup_node_data_dir : string;
      (** The data dir to use for the smart rollup node, where the smart
          rollup preimages are available. *)
  smart_rollup_node_extra_args : Sc_rollup_node.argument list;
      (** The extra arguments needed by the smart rollup node when the smart
          rollup is a bootstrap smart rollup. *)
}

(** [setup_bootstrap_smart_rollup ?name ~address ?parameters_ty ?base_installee
    ~installee ?config ()] creates a {!bootstrap_smart_rollup_setup} that can
    be used to run a bootstrap smart rollup.

    [name] is the smart rollup node data-dir's name. Defaults to ["smart-rollup"].
    [address] is the smart rollup address.
    [parameters_ty] is the smart rollup type. Defaults to ["string"].
    [base_installee] is the directory where [installee] (the kernel) can be found.
    [config] is the optional smart rollup installer configuration, see {!prepare_installer_kernel}.
*)
val setup_bootstrap_smart_rollup :
  ?name:string ->
  address:string ->
  ?parameters_ty:string ->
  ?whitelist:string list ->
  ?base_installee:string ->
  installee:string ->
  ?config:[< `Config of Installer_kernel_config.t | `Path of string] ->
  unit ->
  bootstrap_smart_rollup_setup Lwt.t

(* Refutation game scenarios
   -------------------------
*)

type refutation_scenario_parameters = {
  loser_modes : string list;
  inputs : string list list;
  final_level : int;
  empty_levels : int list;
  stop_loser_at : int list;
  reset_honest_on : (string * int * Sc_rollup_node.mode option) list;
  bad_reveal_at : int list;
  priority : [`Priority_honest | `Priority_loser | `No_priority];
  allow_degraded : bool;
}

val refutation_scenario_parameters :
  ?loser_modes:string list ->
  final_level:int ->
  ?empty_levels:int list ->
  ?stop_loser_at:int list ->
  ?reset_honest_on:(string * int * Sc_rollup_node.mode option) list ->
  ?bad_reveal_at:int list ->
  ?priority:[`No_priority | `Priority_honest | `Priority_loser] ->
  ?allow_degraded:bool ->
  string list list ->
  refutation_scenario_parameters

type test = {variant : string option; tags : string list; description : string}

val format_title_scenario : string -> test -> string

val send_message :
  ?hooks:Process_hooks.t -> ?src:string -> Client.t -> string -> unit Lwt.t

val send_messages :
  ?hooks:Process_hooks.t ->
  ?src:string ->
  ?batch_size:int ->
  int ->
  Client.t ->
  unit Lwt.t

(** Smart rollup constants *)

type sc_rollup_constants = {
  origination_size : int;
  challenge_window_in_blocks : int;
  stake_amount : Tez.t;
  commitment_period_in_blocks : int;
  max_lookahead_in_blocks : int32;
  max_active_outbox_levels : int32;
  max_outbox_messages_per_level : int;
  number_of_sections_in_dissection : int;
  timeout_period_in_blocks : int;
}

val get_sc_rollup_constants : Client.t -> sc_rollup_constants Lwt.t

val publish_commitment :
  ?src:string ->
  commitment:RPC.smart_rollup_commitment ->
  Client.t ->
  string ->
  unit Runnable.process

val forge_and_publish_commitment_return_runnable :
  ?compressed_state:string ->
  ?number_of_ticks:int ->
  inbox_level:int ->
  predecessor:string ->
  sc_rollup:string ->
  src:string ->
  Client.t ->
  RPC.smart_rollup_commitment * unit Runnable.process

val get_staked_on_commitment :
  sc_rollup:string -> staker:string -> Client.t -> string Lwt.t

val forge_and_publish_commitment :
  ?compressed_state:string ->
  ?number_of_ticks:int ->
  inbox_level:int ->
  predecessor:string ->
  sc_rollup:string ->
  src:string ->
  Client.t ->
  (RPC.smart_rollup_commitment * string) Lwt.t

val bake_period_then_publish_commitment :
  ?compressed_state:string ->
  ?number_of_ticks:int ->
  sc_rollup:string ->
  src:string ->
  Client.t ->
  (RPC.smart_rollup_commitment * string) Lwt.t

val cement_commitment :
  Protocol.t ->
  ?src:string ->
  ?fail:string ->
  sc_rollup:string ->
  hash:string ->
  Client.t ->
  unit Lwt.t

val bake_operation_via_rpc :
  __LOC__:string -> Client.t -> Operation_core.Manager.t -> unit Lwt.t

val start_refute :
  Client.t ->
  source:Account.key ->
  opponent:string ->
  sc_rollup:string ->
  player_commitment_hash:string ->
  opponent_commitment_hash:string ->
  unit Lwt.t

val move_refute_with_unique_state_hash :
  ?number_of_sections_in_dissection:int ->
  Client.t ->
  source:Account.key ->
  opponent:string ->
  sc_rollup:string ->
  state_hash:string ->
  unit Lwt.t

val timeout :
  ?expect_failure:bool ->
  sc_rollup:string ->
  staker1:string ->
  staker2:string ->
  ?src:string ->
  Client.t ->
  unit Lwt.t

val send_text_messages :
  ?format:[< `Hex | `Raw > `Raw] ->
  ?hooks:Process_hooks.t ->
  ?src:string ->
  Client.t ->
  string list ->
  unit Lwt.t

type reveal_hash = {message : string; filename : string}

val reveal_hash : protocol:'a -> kind:string -> string -> reveal_hash

(** A common test that use both in sc_rollup and sc_rollup_migration for refutation tests *)
val test_refutation_scenario_aux :
  mode:Sc_rollup_node.mode ->
  kind:string ->
  refutation_scenario_parameters ->
  'a ->
  Sc_rollup_node.t ->
  string ->
  Node.t ->
  Client.t ->
  unit Lwt.t

val inputs_for : int -> string list list

val wait_for_injecting_event :
  ?tags:string list -> ?count:int -> Sc_rollup_node.t -> int Lwt.t

val injecting_refute_event : 'a -> Sc_rollup_node.t -> unit Lwt.t

type transaction = {
  destination : string;
  entrypoint : string option;
  parameters : JSON.u;
  parameters_ty : JSON.u option;
}

(** Converts a list of transactions into a JSON object. *)
val json_of_output_tx_batch : transaction list -> JSON.u
