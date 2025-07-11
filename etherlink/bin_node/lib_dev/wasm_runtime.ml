(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
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

let run ?l1_timestamp ~preimages_dir ?preimages_endpoint ~native_execution
    ~entrypoint tree rollup_address inbox : Irmin_context.tree Lwt.t =
  Lwt_preemptive.detach
    (fun () ->
      wasm_runtime_run
        ~preimages_dir
        ?preimages_endpoint:(Option.map Uri.to_string preimages_endpoint)
        ~native_execution
        ~entrypoint
        static_context
        tree
        (Tezos_crypto.Hashed.Smart_rollup_address.to_bytes rollup_address)
        0l
        Shared_inbox.(wrap ?l1_timestamp inbox))
    ()

let preload_kernel tree =
  Lwt_preemptive.detach
    (fun () ->
      Evm_node_wasm_runtime.wasm_runtime_preload_kernel static_context tree)
    ()
