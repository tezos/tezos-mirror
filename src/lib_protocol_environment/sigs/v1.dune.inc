(rule
 (targets v1.ml)
 (deps

   ;; Part of OCaml stdlib 4.09.1
   v1/pervasives.mli
   v1/list.mli
   v1/string.mli
   v1/char.mli
   v1/bytes.mli
   v1/int32.mli
   v1/int64.mli
   v1/format.mli

   ;; Part of external libraries
   v1/hex.mli           ; 1.4.0
   v1/z.mli             ; 1.10
   v1/lwt.mli           ; 5.3.0
   v1/lwt_list.mli
   v1/data_encoding.mli ; 0.2

   ;; Tezos extended stdlib revision 32f04ec483b8d9c62dd0957389eb5268d8b3a38d
   v1/raw_hashes.mli
   v1/compare.mli
   v1/error_monad.mli
   v1/logging.mli
   v1/time.mli
   v1/option.mli
   v1/tzEndian.mli

   v1/RPC_arg.mli
   v1/RPC_path.mli
   v1/RPC_query.mli
   v1/RPC_service.mli
   v1/RPC_answer.mli
   v1/RPC_directory.mli

   v1/base58.mli
   v1/s.mli
   v1/set.mli
   v1/map.mli
   v1/blake2B.mli
   v1/bls12_381.mli
   v1/ed25519.mli
   v1/secp256k1.mli
   v1/p256.mli
   v1/chain_id.mli
   v1/signature.mli
   v1/block_hash.mli
   v1/operation_hash.mli
   v1/operation_list_hash.mli
   v1/operation_list_list_hash.mli
   v1/protocol_hash.mli
   v1/context_hash.mli
   v1/pvss_secp256k1.mli
   v1/sapling.mli

   ;; Tezos specifics
   v1/micheline.mli
   v1/block_header.mli
   v1/fitness.mli
   v1/operation.mli
   v1/protocol.mli
   v1/context.mli
   v1/updater.mli
   v1/RPC_context.mli

 )
 (action (with-stdout-to %{targets} (chdir %{workspace_root}}
          (run %{libexec:tezos-protocol-environment:s_packer} "sigs" %{deps})))))
