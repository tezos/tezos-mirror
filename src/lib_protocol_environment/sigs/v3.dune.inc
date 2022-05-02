(rule
 (targets v3.ml)
 (deps

   ;; Part of OCaml stdlib 4.09.1
   v3/pervasives.mli
   v3/string.mli
   v3/char.mli
   v3/bytes.mli
   v3/int32.mli
   v3/int64.mli
   v3/format.mli

   v3/logging.mli

   ;; Part of external libraries
   v3/hex.mli           ; 1.4.0
   v3/z.mli             ; 1.10
   v3/lwt.mli           ; 5.3.0
   v3/data_encoding.mli ; 0.2

   ;; Tezos extended stdlib revision 32f04ec483b8d9c62dd0957389eb5268d8b3a38d
   v3/raw_hashes.mli
   v3/compare.mli
   v3/time.mli
   v3/tzEndian.mli
   v3/bits.mli
   v3/equality_witness.mli
   v3/fallbackArray.mli

   ;; Part of Error_monad/Lwtreslib
   v3/error_monad.mli
   v3/seq.mli
   v3/list.mli
   v3/set.mli
   v3/map.mli
   v3/option.mli
   v3/result.mli

   ;; everything RPC
   v3/RPC_arg.mli
   v3/RPC_path.mli
   v3/RPC_query.mli
   v3/RPC_service.mli
   v3/RPC_answer.mli
   v3/RPC_directory.mli

   ;; Tezos common types and functions (cryptographic primitives, contract
   ;; addresses, operations, etc.)
   v3/base58.mli
   v3/s.mli
   v3/blake2B.mli
   v3/bls12_381.mli
   v3/ed25519.mli
   v3/secp256k1.mli
   v3/p256.mli
   v3/chain_id.mli
   v3/signature.mli
   v3/block_hash.mli
   v3/operation_hash.mli
   v3/operation_list_hash.mli
   v3/operation_list_list_hash.mli
   v3/protocol_hash.mli
   v3/context_hash.mli
   v3/pvss_secp256k1.mli
   v3/sapling.mli
   v3/timelock.mli

   v3/micheline.mli
   v3/block_header.mli
   v3/fitness.mli
   v3/operation.mli
   v3/context.mli
   v3/updater.mli
   v3/RPC_context.mli
 )
 (action (with-stdout-to %{targets} (chdir %{workspace_root}}
          (run %{libexec:tezos-protocol-environment:s_packer} "sigs" %{deps})))))
