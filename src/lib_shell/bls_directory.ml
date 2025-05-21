(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(* This RPCs use Signature.V2 *)
module Bls = Tezos_crypto.Signature.Bls

let check_public_key_with_proof pk proof =
  Signature.Bls.pop_verify pk (Signature.Bls.to_bytes proof)

let build_rpc_directory () =
  let open Lwt_result_syntax in
  let dir : unit Tezos_rpc.Directory.t ref = ref Tezos_rpc.Directory.empty in
  let register0 s f =
    dir := Tezos_rpc.Directory.register !dir s (fun () p q -> f p q)
  in
  register0 Bls_services.S.aggregate_signatures (fun () sigs ->
      return @@ Bls.aggregate_signature_opt sigs) ;
  register0 Bls_services.S.check_proof (fun () pk_with_proof ->
      return @@ check_public_key_with_proof pk_with_proof.pk pk_with_proof.proof) ;
  register0 Bls_services.S.aggregate_public_keys (fun () pks_with_pops ->
      let pop_checks =
        List.for_all
          (fun (s : Bls_services.S.public_key_with_proof) ->
            check_public_key_with_proof s.pk s.proof)
          pks_with_pops
      in
      let res =
        if pop_checks then
          let pkeys =
            List.map
              (fun (s : Bls_services.S.public_key_with_proof) -> s.pk)
              pks_with_pops
          in
          let pk = Bls.aggregate_public_key_opt pkeys in
          Option.map
            (fun pk ->
              let pkh = Bls.Public_key.hash pk in
              Bls_services.S.{pk; pkh})
            pk
        else None
      in
      return res) ;
  register0 Bls_services.S.threshold_signatures (fun () sigs ->
      let sigs =
        List.map
          (fun (s : Bls_services.S.threshold_signature) -> (s.id, s.signature))
          sigs
      in
      return @@ Bls.threshold_signature_opt sigs) ;
  !dir
