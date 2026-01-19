(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2021 Nomadic Development. <contact@tezcore.com>             *)
(* Copyright (c) 2024 TriliTech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

module RPC = struct
  module Proto = Registerer.Registered
  include Plugin.RPC
end

module Metrics = struct
  include Plugin.Metrics

  let hash = Registerer.Registered.hash
end

module Http_cache_headers = struct
  include Plugin.Http_cache_headers

  let hash = Registerer.Registered.hash
end

module Shell_helpers = struct
  include Plugin.Shell_helpers

  let hash = Registerer.Registered.hash
end

let () = Protocol_plugin.register_rpc (module RPC)

let () = Protocol_plugin.register_metrics (module Metrics)

let () =
  Protocol_plugin.register_http_cache_headers_plugin (module Http_cache_headers)

let () = Protocol_plugin.register_shell_helpers (module Shell_helpers)

module Delegators_contribution_plugin = struct
  let hash = Registerer.Registered.hash

  let convert_pkh :
      Tezos_crypto.Signature.public_key_hash ->
      Tezos_crypto__.Signature_v1.public_key_hash = function
    | Ed25519 x -> Ed25519 x
    | Secp256k1 x -> Secp256k1 x
    | P256 x -> P256 x
    | Bls x -> Bls x
    | Mldsa44 x ->
        Tezos_crypto.Signature.V1.Of_V_latest.get_public_key_hash_exn
          (Mldsa44 x)

  let delegated_breakdown_at_sampling context ~cycle ~delegate_pkh =
    let open Lwt_result_syntax in
    let* output =
      Delegators_contribution.delegated_breakdown_at_sampling
        context
        ~cycle
        ~delegate_pkh:(convert_pkh delegate_pkh)
    in
    match output with
    | `Ok
        {
          min_delegated_amount;
          min_delegated_level;
          overstaked;
          total_delegated_including_overdelegated;
          total_delegated_after_limits;
          overdelegated;
        } ->
        return
          (`Ok
             {
               Protocol_plugin.min_delegated_amount;
               min_delegated_level;
               overstaked;
               total_delegated_including_overdelegated;
               total_delegated_after_limits;
               overdelegated;
             })
    | (`Retry_at_level _ | `Cycle_too_far_in_future) as x -> return x

  let min_delegated_breakdown context ~delegate_pkh =
    let open Lwt_result_syntax in
    let* {
           total_delegated;
           own_delegated;
           delegators_contributions;
           former_delegators_unstake_requests;
         } =
      Delegators_contribution.min_delegated_breakdown
        context
        ~delegate_pkh:(convert_pkh delegate_pkh)
    in

    return
      {
        Protocol_plugin.total_delegated;
        own_delegated;
        delegators_contributions;
        former_delegators_unstake_requests;
      }
end

let () =
  Protocol_plugin.register_delegators_contribution
    (module Delegators_contribution_plugin)
