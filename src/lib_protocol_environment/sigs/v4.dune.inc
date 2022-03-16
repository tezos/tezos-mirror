(rule
 (targets v4.ml)
 (deps

   ;; Part of OCaml stdlib 4.09.1
   v4/pervasives.mli
   v4/string.mli
   v4/char.mli
   v4/bytes.mli
   v4/int32.mli
   v4/int64.mli
   v4/format.mli

   v4/logging.mli

   ;; Part of external libraries
   v4/hex.mli           ; 1.4.0
   v4/z.mli             ; 1.10
   v4/lwt.mli           ; 5.3.0
   v4/data_encoding.mli ; 0.2

   ;; Tezos extended stdlib revision 32f04ec483b8d9c62dd0957389eb5268d8b3a38d
   v4/raw_hashes.mli
   v4/compare.mli
   v4/time.mli
   v4/tzEndian.mli
   v4/bits.mli
   v4/equality_witness.mli
   v4/fallbackArray.mli

   ;; Part of Error_monad/Lwtreslib
   v4/error_monad.mli
   v4/seq.mli
   v4/list.mli
   v4/set.mli
   v4/map.mli
   v4/option.mli
   v4/result.mli

   ;; everything RPC
   v4/RPC_arg.mli
   v4/RPC_path.mli
   v4/RPC_query.mli
   v4/RPC_service.mli
   v4/RPC_answer.mli
   v4/RPC_directory.mli

   ;; Tezos common types and functions (cryptographic primitives, contract
   ;; addresses, operations, etc.)
   v4/base58.mli
   v4/s.mli
   v4/blake2B.mli
   v4/bls12_381.mli
   v4/bls_signature.mli
   v4/ed25519.mli
   v4/secp256k1.mli
   v4/p256.mli
   v4/chain_id.mli
   v4/signature.mli
   v4/block_hash.mli
   v4/operation_hash.mli
   v4/operation_list_hash.mli
   v4/operation_list_list_hash.mli
   v4/protocol_hash.mli
   v4/context_hash.mli
   v4/pvss_secp256k1.mli
   v4/sapling.mli
   v4/timelock.mli

   v4/micheline.mli
   v4/block_header.mli
   v4/fitness.mli
   v4/operation.mli
   v4/protocol.mli
   v4/context.mli
   v4/updater.mli
   v4/RPC_context.mli
 )
 (action (with-stdout-to %{targets} (chdir %{workspace_root}}
          (run %{libexec:tezos-protocol-environment:s_packer} "sigs" %{deps})))))
