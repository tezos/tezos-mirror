(rule
 (targets v6.ml)
 (deps

   ;; Part of OCaml stdlib 4.09.1
   v6/pervasives.mli
   v6/either.mli
   v6/string.mli
   v6/char.mli
   v6/bytes.mli
   v6/int32.mli
   v6/int64.mli
   v6/format.mli

   v6/logging.mli

   ;; Part of external libraries
   v6/hex.mli           ; 1.4.0
   v6/z.mli             ; 1.10
   v6/lwt.mli           ; 5.3.0
   v6/data_encoding.mli ; 0.2

   ;; Tezos extended stdlib revision 32f04ec483b8d9c62dd0957389eb5268d8b3a38d
   v6/raw_hashes.mli
   v6/compare.mli
   v6/time.mli
   v6/tzEndian.mli
   v6/bits.mli
   v6/equality_witness.mli
   v6/fallbackArray.mli

   ;; Part of Error_monad/Lwtreslib
   v6/error_monad.mli
   v6/seq.mli
   v6/list.mli
   v6/set.mli
   v6/map.mli
   v6/option.mli
   v6/result.mli

   ;; everything RPC
   v6/RPC_arg.mli
   v6/RPC_path.mli
   v6/RPC_query.mli
   v6/RPC_service.mli
   v6/RPC_answer.mli
   v6/RPC_directory.mli

   ;; Tezos common types and functions (cryptographic primitives, contract
   ;; addresses, operations, etc.)
   v6/base58.mli
   v6/s.mli
   v6/blake2B.mli
   v6/bls12_381.mli
   v6/bls_signature.mli
   v6/ed25519.mli
   v6/secp256k1.mli
   v6/p256.mli
   v6/chain_id.mli
   v6/signature.mli
   v6/block_hash.mli
   v6/operation_hash.mli
   v6/operation_list_hash.mli
   v6/operation_list_list_hash.mli
   v6/protocol_hash.mli
   v6/context_hash.mli
   v6/pvss_secp256k1.mli
   v6/sapling.mli
   v6/timelock.mli

   v6/micheline.mli
   v6/block_header.mli
   v6/bounded.mli
   v6/fitness.mli
   v6/operation.mli
   v6/protocol.mli
   v6/context.mli
   v6/updater.mli
   v6/RPC_context.mli

 )
 (action (with-stdout-to %{targets} (chdir %{workspace_root}}
          (run %{libexec:tezos-protocol-environment:s_packer} "sigs" %{deps})))))
