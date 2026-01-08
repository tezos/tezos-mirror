(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2025 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

open Evm_node_wasm_runtime

let () = wasm_runtime_logger_init ()

let static_context = wasm_runtime_new_context ()

module Shared_inbox : sig
  (** [wrap ?l1_timestamp inbox] encapsulates [inbox] between [sol;
      ipl] and [eol]. These messages are necessary for the proxy mode
      of the kernel, and they are indirectly used by the delayed inbox
      implementation (because the index of the deposit in the shared
      inbox is used to compute the hash).

      When [l1_timestamp] is provided it's used in [ipl], default
      value is `epoch`. *)
  val wrap : ?l1_timestamp:Time.Protocol.t -> string list -> string list
end = struct
  let sol =
    Tezos_scoru_wasm.Pvm_input_kind.(
      Internal_for_tests.to_binary_input (Internal Start_of_level) None)

  let ipl ?(l1_timestamp = Time.Protocol.epoch) () =
    let block_hash = Block_hash.zero in
    let info =
      Data_encoding.(
        Binary.to_string_exn
          (tup2 Time.Protocol.encoding Block_hash.encoding)
          (l1_timestamp, block_hash))
    in
    Tezos_scoru_wasm.Pvm_input_kind.(
      Internal_for_tests.to_binary_input (Internal Info_per_level) (Some info))

  let eol =
    Tezos_scoru_wasm.Pvm_input_kind.(
      Internal_for_tests.to_binary_input (Internal End_of_level) None)

  let wrap ?l1_timestamp inbox = (sol :: ipl ?l1_timestamp () :: inbox) @ [eol]
end

type kernel_input = [`Inbox of string trace | `Skip_stage_one]

let run ~pool ?(trace_host_funs = false) ?l1_timestamp ~preimages_dir
    ?preimages_endpoint ~native_execution ~entrypoint tree rollup_address inbox
    : Irmin_context.tree Lwt.t =
  let scope = Opentelemetry.Scope.get_ambient_scope () in
  Lwt_domain.detach
    pool
    (fun () ->
      wasm_runtime_run
        ~scope:(Wasm_runtime_callbacks.root_scope scope)
        ~trace_host_funs
        ~context:static_context
        ~preimages_dir
        ?preimages_endpoint:(Option.map Uri.to_string preimages_endpoint)
        ~native_execution
        ~entrypoint
        ~tree
        ~rollup_address:
          (Tezos_crypto.Hashed.Smart_rollup_address.to_bytes rollup_address)
        ~level:0l
        (match inbox with
        | `Skip_stage_one -> []
        | `Inbox inbox -> Shared_inbox.(wrap ?l1_timestamp inbox)))
    ()

let preload_kernel ~pool tree =
  Lwt_domain.detach
    pool
    (fun () ->
      Evm_node_wasm_runtime.wasm_runtime_preload_kernel static_context tree)
    ()
