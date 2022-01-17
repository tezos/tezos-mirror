(rule
 (targets v5.ml)
 (deps

   ;; Part of OCaml stdlib 4.09.1
   v5/pervasives.mli
   v5/string.mli
   v5/char.mli
   v5/bytes.mli
   v5/int32.mli
   v5/int64.mli
   v5/format.mli

   v5/logging.mli

   ;; Part of external libraries
   v5/hex.mli           ; 1.4.0
   v5/z.mli             ; 1.10
   v5/lwt.mli           ; 5.3.0
   v5/data_encoding.mli ; 0.2

   ;; Tezos extended stdlib revision 32f04ec483b8d9c62dd0957389eb5268d8b3a38d
   v5/raw_hashes.mli
   v5/compare.mli
   v5/time.mli
   v5/tzEndian.mli
   v5/bits.mli
   v5/equality_witness.mli
   v5/fallbackArray.mli

   ;; Part of Error_monad/Lwtreslib
   v5/error_monad.mli
   v5/seq.mli
   v5/list.mli
   v5/set.mli
   v5/map.mli
   v5/option.mli
   v5/result.mli

   ;; everything RPC
   v5/RPC_arg.mli
   v5/RPC_path.mli
   v5/RPC_query.mli
   v5/RPC_service.mli
   v5/RPC_answer.mli
   v5/RPC_directory.mli

   ;; Tezos common types and functions (cryptographic primitives, contract
   ;; addresses, operations, etc.)
   v5/base58.mli
   v5/s.mli
   v5/blake2B.mli
   v5/bls12_381.mli
   v5/bls_signature.mli
   v5/ed25519.mli
   v5/secp256k1.mli
   v5/p256.mli
   v5/chain_id.mli
   v5/signature.mli
   v5/block_hash.mli
   v5/operation_hash.mli
   v5/operation_list_hash.mli
   v5/operation_list_list_hash.mli
   v5/protocol_hash.mli
   v5/context_hash.mli
   v5/pvss_secp256k1.mli
   v5/sapling.mli
   v5/timelock.mli

   v5/micheline.mli
   v5/block_header.mli
   v5/fitness.mli
   v5/operation.mli
   v5/protocol.mli
   v5/context.mli
   v5/updater.mli
   v5/RPC_context.mli
 )
 (action (with-stdout-to %{targets} (chdir %{workspace_root}}
          (run %{libexec:tezos-protocol-environment-packer:s_packer} "sigs" %{deps})))))
